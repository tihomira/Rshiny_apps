library(shiny)
library(shinythemes)
library(tm)
library(stringr)

source("./functions.R")
quadgrams <- readRDS(file = "./data/4grams.RData")
trigrams <- readRDS(file = "./data/3grams.RData")
bigrams <- readRDS(file = "./data/2grams.RData")

shinyServer(function(input, output) {
      
      wordPrediction <- reactive({
            text <- input$text
            textInput <- cleanInput(text)
            wordCount <- length(textInput)
            wordPrediction <- nextWordPrediction(wordCount,textInput)})
      
      output$predictedWord <- renderPrint(wordPrediction())
})
