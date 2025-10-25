library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  #Title Panel
  titlePanel("Bike Sharing Exploration"),
  
  #Sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      h2("Select Numeric Variables:"),
      
      #Numeric Variable Selector
      selectInput("num_var", "Numerical Variable:", choices = c("Rented Bike Count", "Temperature(°C)"), selected = "Rented Bike Count"),
      
      br(),
      
      #Categorical Variable Subsets
      #Seasons Buttons
      radioButtons("seas", "Seasons", choiceNames = c("All", "Winter", "Autumn", "Spring", "Summer"), choiceValues = c("all", "Winter", "Autumn", "Spring", "Summer")), 
      
      #Holidays Buttons
      radioButtons("hol", "Holiday", choiceNames = c("All", "Holiday", "Not Holiday"), choiceValues = c("all", "Holiday", "No Holiday")), 

      
      #Sample Size Slider
      h2("Select a Sample Size"),
      sliderInput("corr_n", label = NULL, min = 20, max = 500, value = 20),
      actionButton("bike_sample","Get a Sample!")
    ),
    
    #Code for Main Panel with plot and correlation guessing activity
    mainPanel(
      plotOutput("corr_plot"),
      conditionalPanel("input.bike_sample > 0",
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

my_sample <- readRDS("my_sample_temp.rds")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  #This code makes sure that numerical and categorical subsets of the data respect choices made
  observeEvent(input$apply_filters, {
    filtered_data$df <- bike |>
      filter(
        # Handle seasons filter
        if (input$seas == "all") TRUE else Seasons == input$seas,
        
        # Handle holiday filter
        if (input$hol == "all") TRUE else Holiday == input$hol,
        
        # Handle numeric filters
        between(.data[[input$num_var1]], input$num_range1[1], input$num_range1[2]),
        between(.data[[input$num_var2]], input$num_range2[1], input$num_range2[2])
      )
  })
  
  
  #creating sample_corr, a reactiveValues object that has two elements that default to null
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  
    corr_vars <- c(input$corr_x, input$corr_y)
    
  
    index <- sample(1:nrow(subsetted_data),
                    size = input$corr_n,
                    replace = TRUE,
                    prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    
    #updating the sample_corr reactive value object
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(corr_vars))[1,2]
  })
  
  
  #Create a renderPlot() object to output a scatter plot
  output$corr_plot <- renderPlot({
    #The code below validates that data exists
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    #code for plot
    ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
      geom_point()
  })
  
  #This code does the correlation guessing game!
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", 
                        round(sample_corr$corr_truth, 4), 
                        "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
