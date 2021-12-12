## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
library(rsconnect)

source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")


sidebar <- dashboardSidebar(sidebarMenu(
                              menuItem("Recommendation by Genre", tabName="tabItemGenre"),
                              menuItem("Recommendation by Rating", tabName="tabItemRating")
                            )
           )

tabItemGenre <- tabItem(tabName="tabItemGenre",
                         fluidRow(
                           box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                             div(class = "selectgenre",
                               selectInput("selectedgenre", "Genre", genre_list)
                             )
                           )
                         ),
                         fluidRow(
                           useShinyjs(),
                           box(
                             width = 12, status = "info", solidHeader = TRUE,
                             title = "Step 2: Discover movies you might like",
                             br(),
                             tableOutput("genreresults")
                           )
                         )
                )

tabItemRating <- tabItem(tabName="tabItemRating",
                         fluidRow(
                           box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                               div(class = "rateitems",
                                   uiOutput('ratings')
                               )
                           )
                         ),
                         fluidRow(
                           useShinyjs(),
                           box(
                             width = 12, status = "info", solidHeader = TRUE,
                             title = "Step 2: Discover movies you might like",
                             br(),
                             withBusyIndicatorUI(
                               actionButton("ratingbtn", "Click here to get your recommendations", class = "btn-warning")
                             ),
                             br(),
                             tableOutput("ratingresults")
                           )
                         )
                )

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          sidebar,
          dashboardBody(includeCSS("css/movies.css"),
                        tabItems(
                          tabItemRating,
                          tabItemGenre
                        )

          )
    )
) 