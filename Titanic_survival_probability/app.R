library(shiny)
library(shinythemes)
data("Titanic")
# theme options:"cerulean",  "sandstone", "simplex", "yeti", "cosmo", "lumen", "flatly"

ui <- fluidPage(
      theme = shinytheme("simplex"),
      # Put some title
      titlePanel(strong("TITANIC SURVIVAL PROBABILITY RATE")),
   # Make the layout of the app
   sidebarLayout(
      sidebarPanel(
            # make the firs radio button
         radioButtons("sex",
                      label = "Sex:",
                      choices = list("Male" = "Male", 
                                     "Female" = "Female"),
                      selected = "Male",
                      inline = TRUE),
         hr(), # put a horizontal line
         # make the second radion button
         radioButtons("age",
                      label = "Age:",
                      choices = list("Child" = "Child",
                                     "Adult" = "Adult"),
                      selected = "Adult",
                      inline = TRUE),
         hr(), # put a horizontal line
         # make the third radio button
         radioButtons("class",
                      label = "Crew / Passenger:",
                      list("1st Class Passenger" = "1st",
                           "2nd Class Passenger" = "2nd",
                           "3rd Class Passenger" = "3rd",
                           "Crew Member" = "Crew"),
                      selected = "1st")
      ),
      # set what will be displayed in main main panel
      mainPanel(
           h4(helpText("Survival Probability:")),
            h2(strong(textOutput("prob"))),
           hr(),
           em("*This is a probability estimation for the survivals on Titanic.
             It is based on logistic regression model over the historical data.
             They can be obtained from the data set Titanic in R.")
            
      )
   )
)


server <- function(input, output) {
      
      
      # set the table Titanic to data frame
      titanic <- as.data.frame(Titanic)
      # make the regression model
      titanic_glm <- glm(Survived ~ Class + Sex + Age, family = binomial, data = titanic, weights = titanic$Freq)
      
      
      # set the function for the prediction and the new data
      predictionTitanic <- function(class, sex, age) {
            inputdata <- c(class, sex, age)
            predictionData <- as.data.frame(t(inputdata))
            colnames(predictionData) <- c("Class", "Sex", "Age")
            survival_prob <- predict(titanic_glm, predictionData, type = "response")
            return(survival_prob)
      }
      # take the user's choices
            output$prob <- renderText({
                  predictionTitanic(input$class, input$sex, input$age)
            })
            
}

shinyApp(ui = ui, server = server)

