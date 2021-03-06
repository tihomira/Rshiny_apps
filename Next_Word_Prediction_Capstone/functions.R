library(shinythemes)
library(shiny)
library(tm)
library(stringr)
library(markdown)
library(stylo)

quadgrams <- readRDS(file = "./data/4grams.RData")
trigrams <- readRDS(file = "./data/3grams.RData")
bigrams <- readRDS(file = "./data/2grams.RData")

dataCleaner <- function(text){
      
      cleanText <- tolower(text)
      cleanText <- removePunctuation(cleanText)
      cleanText <- removeNumbers(cleanText)
      cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
      cleanText <- stripWhitespace(cleanText)
      return(cleanText)
}

cleanInput <- function(text){
      
      textInput <- dataCleaner(text)
      textInput <- txt.to.words.ext(textInput, 
                                    language = "English.all", 
                                    preserve.case = TRUE)
      return(textInput)
}


nextWordPrediction <- function(wordCount,textInput){
      
      if (wordCount >= 3) {
            textInput <- textInput[(wordCount - 2):wordCount] 
            
      }
      
      else if (wordCount == 2) {
            textInput <- c(NA,textInput)   
      }
      
      else {
            textInput <- c(NA,NA,textInput)
      }
      
      
      wordPrediction <- as.character(quadgrams[quadgrams$unigram == textInput[1] & 
                                                     quadgrams$bigram == textInput[2] & 
                                                     quadgrams$trigram == textInput[3],][1,]$quadgram)
      
      if (is.na(wordPrediction)) {
            wordPrediction1 <- as.character(trigrams[trigrams$unigram == textInput[2] & 
                                                           trigrams$bigram == textInput[3],][1,]$trigram)
            
            if (is.na(wordPrediction)) {
                  wordPrediction <- as.character(bigrams[bigrams$unigram == textInput[3],][1,]$bigram)
            }
      }
      
      
      print(wordPrediction)
      
}