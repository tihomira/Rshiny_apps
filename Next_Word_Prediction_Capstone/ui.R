library(shiny)
library(shinythemes)
library(tm)
library(stringr)



shinyUI(navbarPage("Shiny Application",
                   tabPanel("Next Word Prediction"),
                   
                   helpText(h3("Next Word Prediction Application"),align = "left"),
                   helpText(h5("This is a simple word prediction application. \nYou can type some text in the field below and the application will do its best to predict the next word. Please, give it a couple of seconds to load."),align = "left"),
                   hr(),
                   fluidRow(column(4,
                         textInput("text", 
                                   label = h4("Enter english text:"), 
                                   width = "80%",
                                   placeholder = "Enter text here..")),
                         
                    # hr(),
                    helpText(h4("Prediction of the next word:")),
                    fluidRow(column(3, verbatimTextOutput("predictedWord"))),
                    br(),
                    hr(),
                    helpText(em("*The application was build as a final project of Coursera's Data Science Specialization."))
      
)))



