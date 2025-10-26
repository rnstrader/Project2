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
      
      #1st Numeric Variable Selector
      selectInput("num_var1", "Numerical Variable 1:", choices = numeric_vars, selected = "Rented Bike Count"),
      
      uiOutput("num_filter1"),
      
      tags$hr(),
      
      #2nd Numeric Variable Selector
      selectInput("num_var2", "Numerical Variable 2:", choices = numeric_vars, selected = "Temperature(°C)"),
      
      uiOutput("num_filter2"),
      
      tags$hr(),
      
      br(),
      
      #Categorical Variable Subsets
      #Seasons Buttons
      radioButtons("seas", "Seasons", choiceNames = c("All", "Winter", "Autumn", "Spring", "Summer"), choiceValues = c("all", "Winter", "Autumn", "Spring", "Summer")), 
      
      #Holidays Buttons
      radioButtons("hol", "Holiday", choiceNames = c("All", "Holiday", "Not Holiday"), choiceValues = c("all", "Holiday", "No Holiday")), 

      #Apply Button
      actionButton("apply_filters", "Apply Filters", class = "btn-primary")
    ),
    
  #LEFT OFF HERE  
  
    #Code for Main Panel with plot and correlation guessing activity
    mainPanel(
      plotOutput("corr_plot"),
),

my_sample <- readRDS("my_sample_temp.rds"),

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #Dynamic Sliders for numeric variables
  output$num_filter1 <- renderUI({
    req(input$num_var1)
    var_data <- bike[[input$num_var1]]
    sliderInput("num_range1",
                label = paste("Subset", input$num_var1),
                min = floor(min(var_data, na.rm = TRUE)),
                max = ceiling(max(var_data, na.rm = TRUE)),
                value = c(floor(min(var_data, na.rm = TRUE)),
                          ceiling(max(var_data, na.rm = TRUE))))
  })
  
  output$num_filter2 <- renderUI({
    req(input$num_var2)
    var_data <- bike[[input$num_var2]]
    sliderInput("num_range2",
                label = paste("Subset", input$num_var2),
                min = floor(min(var_data, na.rm = TRUE)),
                max = ceiling(max(var_data, na.rm = TRUE)),
                value = c(floor(min(var_data, na.rm = TRUE)),
                          ceiling(max(var_data, na.rm = TRUE))))
  })
  
  #creating a reactiveValues object stores filtered data
  filtered_data <- reactiveValues(df = bike)
  
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

)

# Run the application 
shinyApp(ui = ui, server = server)
