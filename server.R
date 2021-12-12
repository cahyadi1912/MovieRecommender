## server.R

# retrieve user ratings from UI
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# load movies data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


# load ratings data and create realRatingMatrix
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

train = ratings;
i = paste0('u', train$UserID)
j = paste0('m', train$MovieID)
x = train$Rating

tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)

Rmat = new('realRatingMatrix', data = Rmat)


# create movies.stats, containing popularity (sum ratings) for each movie
# this will be used to display movies selection for users to rate
movies.stats = ratings %>% 
  group_by(MovieID) %>% 
  summarize(popularity = sum(Rating) )
movies.stats = movies.stats [order( movies.stats$popularity, decreasing = TRUE ), ]


# read genre_topmovies.txt which contains top-10 movies for each genre
genre_topmovies = read.table("data/genre_topmovies.txt", header = TRUE)

# load a pre-trained IBCF recommender (training is done using all MovieLens data)
rec <-readRDS("data/shinyapp_rec.rds")

shinyServer(function(input, output, session) {
  
  ############### recommendation by genre ##################

  # retrieve movies recommendation for the selected genre
  genre_df <- reactive({
    selectedgenre <- input$selectedgenre
    tmp = genre_topmovies[genre_topmovies$Genre == selectedgenre, ]$MovieID
    movies_idx = match(tmp, movies$MovieID)
    movies_idx
  })
  

  # display movies recommendation (Genre)
  output$genreresults <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- genre_df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result[(i - 1) * num_movies + j]])
            )
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  

  ############### recommendation by rating ##################
  
  # show the movies to rate
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    # movies selection for users to rate:
    # select randomly from the top 500 most popular movies 
    tmp = movies.stats[1:500, ]
    idxs = sample(1:(nrow(tmp)),num_rows * num_movies, FALSE )
    movies.to.rate.idxs <- match(tmp$MovieID[idxs], movies$MovieID)

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[movies.to.rate.idxs[(i - 1) * num_movies + j]], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[movies.to.rate.idxs[(i - 1) * num_movies + j]])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[movies.to.rate.idxs[(i - 1) * num_movies + j]]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # calculate movies recommendation when the submit button is clicked
  ratings_df <- eventReactive(input$ratingbtn, {
    withBusyIndicatorServer("ratingbtn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      gc(verbose = FALSE)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      
      print ("User rating")
      print (user_ratings )
      
      # create realRatingMatrix for the new user
      user_ratings$MovieID <- paste0('m', user_ratings$MovieID )
      
      Rmatcolumns = colnames(Rmat)
      user.rmat = matrix( rep(NA, length(Rmatcolumns)), 
                          nrow = 1,
                          dimnames = list (user = "Edwin", item = Rmatcolumns)
                        )
      user.rmat[1, user_ratings$MovieID] = user_ratings$Rating
      user.rmat = as(user.rmat, "realRatingMatrix")
      
      # predict missing ratings for the new user
      pred = predict(rec, user.rmat, type ='ratings')
      pred.mat = as(pred, "matrix")
      
      pred.mat = pred.mat[1, order(pred.mat[1, ], decreasing = TRUE )][1:10]
      print ( pred.mat )
      
      # we only recommend movies with predicted ratings >= 2.5
      pred.mat = pred.mat[(!is.na(pred.mat)) & pred.mat >= 2.5 ]
      
      movies_idx = NULL
      if ( length(pred.mat) > 0 ){
        tmp.ids = names(pred.mat)
        tmp.ids = as.integer(gsub("m", "", tmp.ids))
        movies_idx = match(tmp.ids, movies$MovieID)
      }
      movies_idx
      
    }) # still busy
    
  }) # clicked on button
  
  
  
  # display movies recommendation (Rating)
  output$ratingresults <- renderUI({
    recom_result <- ratings_df()
    
    result = NULL
    if ( length(recom_result) == 0 ){
      result = p("There is no recommended movie with rating 3 and above. Please rate more movies...")
    }
    else
    {
      moviesperrow = 5
      num_rows = ceiling( length(recom_result) / moviesperrow )
      
      result = 
        lapply(1:num_rows, function(i) {
          num_movies = min( length(recom_result) - (i-1)*moviesperrow, moviesperrow)
          list(fluidRow(lapply(1:num_movies, function(j) {
            box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * moviesperrow + j),
                
                div(style = "text-align:center", 
                    a(img(src = movies$image_url[recom_result[(i - 1) * moviesperrow + j]], height = 150))
                ),
                div(style="text-align:center; font-size: 100%", 
                    strong( movies$Title[recom_result[(i - 1) * moviesperrow + j]])
                )
                
            )        
          }))) # columns
        }) # rows
      
    }
    
    result;
    
  }) # renderUI function
  
}) # server function
