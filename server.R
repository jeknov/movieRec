#server.R
library(shiny)
library(proxy)
library(recommenderlab)
library(reshape2)
source("helpercode.R")

search <- read.csv("search.csv", stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv", header = TRUE)
search <- search[-which((search$movieId %in% ratings$movieId) == FALSE),]

formatInput <- function(v,a,d){
  ## This function formats the user's input of Valence-Arousal-Dominance
  ## and outputs them as a vector
  
  c(v,a,d)
}

shinyServer(function(input, output) {
  
  output$ui <- renderUI({
    if (is.null(input$input_genre))
      return()
    
    switch(input$input_genre,
           "Action" = selectInput("select", "Movie of Genre #1",
                                  choices = sort(subset(search, Action == 1)$title),
                                  selected = sort(subset(search, Action == 1)$title)[1]),
           "Adventure" = selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Animation" =  selectInput("select", "Movie of Genre #1",
                                      choices = sort(subset(search, Animation == 1)$title),
                                      selected = sort(subset(search, Animation == 1)$title)[1]),
           "Children" =  selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(search, Children == 1)$title),
                                     selected = sort(subset(search, Children == 1)$title)[1]),
           "Comedy" =  selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(search, Comedy == 1)$title),
                                   selected = sort(subset(search, Comedy == 1)$title)[1]),
           "Crime" =  selectInput("select", "Movie of Genre #1",
                                  choices = sort(subset(search, Crime == 1)$title),
                                  selected = sort(subset(search, Crime == 1)$title)[1]),
           "Documentary" =  selectInput("select", "Movie of Genre #1",
                                        choices = sort(subset(search, Documentary == 1)$title),
                                        selected = sort(subset(search, Documentary == 1)$title)[1]),
           "Drama" =  selectInput("select", "Movie of Genre #1",
                                  choices = sort(subset(search, Drama == 1)$title),
                                  selected = sort(subset(search, Drama == 1)$title)[1]),
           "Fantasy" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(search, Fantasy == 1)$title),
                                    selected = sort(subset(search, Fantasy == 1)$title)[1]),
           "Film.Noir" =  selectInput("select", "Movie of Genre #1",
                                      choices = sort(subset(search, Film.Noir == 1)$title),
                                      selected = sort(subset(search, Film.Noir == 1)$title)[1]),
           "Horror" =  selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(search, Horror == 1)$title),
                                   selected = sort(subset(search, Horror == 1)$title)[1]),
           "Musical" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(search, Musical == 1)$title),
                                    selected = sort(subset(search, Musical == 1)$title)[1]),
           "Mystery" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(search, Mystery == 1)$title),
                                    selected = sort(subset(search, Mystery == 1)$title)[1]),
           "Romance" =  selectInput("select", "Movie of Genre #1",
                                    choices = sort(subset(search, Romance == 1)$title),
                                    selected = sort(subset(search, Romance == 1)$title)[1]),
           "Sci.Fi" =  selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(search, Sci.Fi == 1)$title),
                                   selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
           "Thriller" =  selectInput("select", "Movie of Genre #1",
                                     choices = sort(subset(search, Thriller == 1)$title),
                                     selected = sort(subset(search, Thriller == 1)$title)[1]),
           "War" =  selectInput("select", "Movie of Genre #1",
                                choices = sort(subset(search, War == 1)$title),
                                selected = sort(subset(search, War == 1)$title)[1]),
           "Western" = selectInput("select", "Movie of Genre #1",
                                   choices = sort(subset(search, Western == 1)$title),
                                   selected = sort(subset(search, Western == 1)$title)[1])
    )
  })
  
  output$ui2 <- renderUI({
    if (is.null(input$input_genre2))
      return()
    
    switch(input$input_genre2,
           "Action" = selectInput("select2", "Movie of Genre #2",
                                  choices = sort(subset(search, Action == 1)$title),
                                  selected = sort(subset(search, Action == 1)$title)[1]),
           "Adventure" = selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Animation" =  selectInput("select2", "Movie of Genre #2",
                                      choices = sort(subset(search, Animation == 1)$title),
                                      selected = sort(subset(search, Animation == 1)$title)[1]),
           "Children" =  selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(search, Children == 1)$title),
                                     selected = sort(subset(search, Children == 1)$title)[1]),
           "Comedy" =  selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(search, Comedy == 1)$title),
                                   selected = sort(subset(search, Comedy == 1)$title)[1]),
           "Crime" =  selectInput("select2", "Movie of Genre #2",
                                  choices = sort(subset(search, Crime == 1)$title),
                                  selected = sort(subset(search, Crime == 1)$title)[1]),
           "Documentary" =  selectInput("select2", "Movie of Genre #2",
                                        choices = sort(subset(search, Documentary == 1)$title),
                                        selected = sort(subset(search, Documentary == 1)$title)[1]),
           "Drama" =  selectInput("select2", "Movie of Genre #2",
                                  choices = sort(subset(search, Drama == 1)$title),
                                  selected = sort(subset(search, Drama == 1)$title)[1]),
           "Fantasy" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(search, Fantasy == 1)$title),
                                    selected = sort(subset(search, Fantasy == 1)$title)[1]),
           "Film.Noir" =  selectInput("select2", "Movie of Genre #2",
                                      choices = sort(subset(search, Film.Noir == 1)$title),
                                      selected = sort(subset(search, Film.Noir == 1)$title)[1]),
           "Horror" =  selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(search, Horror == 1)$title),
                                   selected = sort(subset(search, Horror == 1)$title)[1]),
           "Musical" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(search, Musical == 1)$title),
                                    selected = sort(subset(search, Musical == 1)$title)[1]),
           "Mystery" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(search, Mystery == 1)$title),
                                    selected = sort(subset(search, Mystery == 1)$title)[1]),
           "Romance" =  selectInput("select2", "Movie of Genre #2",
                                    choices = sort(subset(search, Romance == 1)$title),
                                    selected = sort(subset(search, Romance == 1)$title)[1]),
           "Sci.Fi" =  selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(search, Sci.Fi == 1)$title),
                                   selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
           "Thriller" =  selectInput("select2", "Movie of Genre #2",
                                     choices = sort(subset(search, Thriller == 1)$title),
                                     selected = sort(subset(search, Thriller == 1)$title)[1]),
           "War" =  selectInput("select2", "Movie of Genre #2",
                                choices = sort(subset(search, War == 1)$title),
                                selected = sort(subset(search, War == 1)$title)[1]),
           "Western" = selectInput("select2", "Movie of Genre #2",
                                   choices = sort(subset(search, Western == 1)$title),
                                   selected = sort(subset(search, Western == 1)$title)[1])
    )
  })
  
  output$ui3 <- renderUI({
    if (is.null(input$input_genre3))
      return()
    
    switch(input$input_genre3,
           "Action" = selectInput("select3", "Movie of Genre #3",
                                  choices = sort(subset(search, Action == 1)$title),
                                  selected = sort(subset(search, Action == 1)$title)[1]),
           "Adventure" = selectInput("select3", "Movie of Genre #3",
                                     choices = sort(subset(search, Adventure == 1)$title),
                                     selected = sort(subset(search, Adventure == 1)$title)[1]),
           "Animation" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Animation == 1)$title),
                                      selected = sort(subset(search, Animation == 1)$title)[1]),
           "Children" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Children == 1)$title),
                                      selected = sort(subset(search, Children == 1)$title)[1]),
           "Comedy" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Comedy == 1)$title),
                                      selected = sort(subset(search, Comedy == 1)$title)[1]),
           "Crime" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Crime == 1)$title),
                                      selected = sort(subset(search, Crime == 1)$title)[1]),
           "Documentary" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Documentary == 1)$title),
                                      selected = sort(subset(search, Documentary == 1)$title)[1]),
           "Drama" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Drama == 1)$title),
                                      selected = sort(subset(search, Drama == 1)$title)[1]),
           "Fantasy" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Fantasy == 1)$title),
                                      selected = sort(subset(search, Fantasy == 1)$title)[1]),
           "Film.Noir" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Film.Noir == 1)$title),
                                      selected = sort(subset(search, Film.Noir == 1)$title)[1]),
           "Horror" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Horror == 1)$title),
                                      selected = sort(subset(search, Horror == 1)$title)[1]),
           "Musical" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Musical == 1)$title),
                                      selected = sort(subset(search, Musical == 1)$title)[1]),
           "Mystery" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Mystery == 1)$title),
                                      selected = sort(subset(search, Mystery == 1)$title)[1]),
           "Romance" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Romance == 1)$title),
                                      selected = sort(subset(search, Romance == 1)$title)[1]),
           "Sci.Fi" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Sci.Fi == 1)$title),
                                      selected = sort(subset(search, Sci.Fi == 1)$title)[1]),
           "Thriller" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, Thriller == 1)$title),
                                      selected = sort(subset(search, Thriller == 1)$title)[1]),
           "War" =  selectInput("select3", "Movie of Genre #3",
                                      choices = sort(subset(search, War == 1)$title),
                                      selected = sort(subset(search, War == 1)$title)[1]),
           "Western" = selectInput("select3", "Movie of Genre #3",
                                  choices = sort(subset(search, Western == 1)$title),
                                  selected = sort(subset(search, Western == 1)$title)[1])
    )
  })
  
  output$table <- renderTable({
    movie_recommendation(input$select, input$select2, input$select3)
  })
  
  output$dynamic_value <- renderPrint({
    c(input$select,input$select2,input$select3)
  })
  
})