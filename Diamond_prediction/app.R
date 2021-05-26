library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)
data(diamonds)

# linear model
LinMmodel <- lm(price ~ carat + cut + clarity + color + x + y + z, data = diamonds)


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel(strong("DIAMOND'S PRICE PREDICTION")),
  sidebarLayout(
    sidebarPanel(
      helpText("This application predicts the price in $ of a diamond based on several attributes. Make your preferences and move the sliders."),
      hr(),
      sliderInput(inputId = "X",
                  label = "Length in millimeters:",
                  value = 2.2,
                  min = 0.5,
                  max = 10,
                  step = .1,
                  post = "mm"),
      sliderInput(inputId = "Y",
                  label = "Width in millimeters:",
                  value = 5.2,
                  min = 0.5,
                  max = 10,
                  step = .1,
                  post = "mm"),
      sliderInput(inputId = "Z",
                  label = "Depth in millimeters:",
                  value = 7.3,
                  min = 0.5,
                  max = 10,
                  step = .1,
                  post = "mm"),
      hr(),
      # helpText("Make a choice of the desired cut:"),
      radioButtons(inputId = "RBcut",
                   label = "Desired cut: ",
                   choices = c("Fair" = "Fair", 
                               "Good" = "Good",
                               "Very Good" = "Very Good", 
                               "Premium" = "Premium",
                               "Ideal"="Ideal"),
                   selected = "Premium",
                   inline = TRUE),
      hr(),
      # helpText("Make a choice of the desired color:"),
      selectInput(inputId = "Scolor",
                  label = "Desired color: ",
                  selected = "D",
                  choices = c("D (best)" = "D", 
                              "E" = "E",
                              "F" = "F", 
                              "G" = "G",
                              "H" = "H",
                              "I" = "I",
                              "J (worst)" = "J")),
      hr(),
      # helpText("Make a choice of the desired clarity: a measurement of how clear the diamond is"),
      selectInput(inputId = "Sclar",
                  label = "Desired clarity: ",
                  selected = "IF",
                  choices = c("IF (best)" = "IF", 
                              "VVS1" = "VVS1",
                              "VVS2" = "VVS2",
                              "VS2" = "VS2",
                              "VS1" = "VS1",
                              "SI1" = "SI1", 
                              "SI2" = "SI2",
                              "I1 (worst)" = "I1")),
      hr(),
      # helpText("Make a choice of the desired clarity: a measurement of how clear the diamond is"),
      sliderInput(inputId = "carat",
                  label = "Choose the carat of the diamond:",
                  value = 1.2,
                  min = 0.1,
                  max = 4.5,
                  step = .01,
                  post = "ct.")
      
      
    ),
    
    mainPanel(
      htmlOutput("detailedText"),
      htmlOutput("prediction"),
      hr(),
      plotlyOutput("plot", width = "70%"),
      verbatimTextOutput("outcome"),
      hr(),
      em("*This app is based on linear regression model. "),
      br(),
      em("The data set that it was derived from contains the prices and other attributes of almost 54,000 diamonds. "),
      em("Name of the data set:", strong("diamonds")),
      em(", R package:", strong("ggplot2"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$detailedText <- renderText({
    paste("Based on the following attributes: </br> length: ",
          strong(input$X),
          "mm </br> width: ",
          strong(input$Y),
          "mm </br> depth: ",
          strong(input$Z),
          "mm </br> quality: ",
          strong(input$RBcut),
          " </br> color: ",
          strong(input$Scolor),
          " </br> clarity: ",
          strong(input$Sclar),
          " </br> weight: ",
          strong(input$carat),
          "ct.</br>the predicted price would be approximately: </br> ________________________ ")
  })
  
  
  
  
  output$prediction <- renderText({
    df <- data.frame(x = input$X,
                     y = input$Y,
                     z = input$Z,
                     cut = factor(input$RBcut, levels = levels(diamonds$cut)),
                     color = factor(input$Scolor, levels = levels(diamonds$color)),
                     clarity = factor(input$Sclar, levels = levels(diamonds$clarity)),
                     carat = input$carat)
    
    pr <- predict(LinMmodel, newdata = df, interval = "prediction")
    
    paste(h1( strong("$ "), strong(floor(pr))))
    
    
  })
  output$plot <- renderPlotly({
    library(plotly)
    set.seed(150)
    d <- diamonds[sample(nrow(diamonds), 500), ]
    plot_ly(d, x = ~carat, y = ~price, color = ~carat,
            size = ~carat, alpha = 1/2,text =  ~paste("Carat: ", carat, 
                                                      "</br> Cut: ", cut,
                                                      "</br> Color: ", color,
                                                      "</br> Clarity: ", clarity,
                                                      "</br> Length: ", x,
                                                      "</br> Width: ", y,
                                                      "</br> Depth: ", z,
                                                      "</br> Price: ", price)) %>%
      layout(title = "A quick look over a 500 random samples from the data set")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

