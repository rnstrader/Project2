library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)
library(shinycssloaders)

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
    
    #Code for Main Panel with tabs
    mainPanel(
      tabsetPanel(
        
        #About Tab
        tabPanel("About", 
                 h3("About This App"),
                 p("This app allows the user to explore data collected on bike sharing dataset. Subsetting the data from options on the sidebar allow the user to examine patterns based on weather phenomena, seasonal differences, and holidays."),
                 
                 h4("About the Data"),
                 p("The dataset used here contains hourly bike rental information with weather and calendar information for the city of Seoul, South Korea."),
                 
                 tags$a(href = "https://archive.ics.uci.edu/ml/datasets/Seoul+Bike+Sharing+Demand",
                        "Click here for more information on the dataset.", target = "_blank"),
                 
                 h4("Using the App"),
                 tags$ul(
                   tags$li("Use the sidebar to subset data by season, holiday, or numeric variables."),
                   tags$li("The Data Download tab allows the user to view the entire dataset or to subset it based on choices described previously. The data can also be saved as a file."),
                   tags$li("The Data Exploration tab allows the user to obtain numerical and graphical summaries of the data.")
                 ),
                 
                 img(src = "bikes.jpg", height = "250px", alt = "Bike Sharing Image")
                 ),
        
        #Data Download Tab
        tabPanel("Data Download",
                 h4("View and Download Filtered Data"),
                 p("Below is your filtered dataset. Download it using the button below."),
                 DT::dataTableOutput("filtered_table"),
                 br(),
                 downloadButton("download_data", "Download Filtered Data", class = "btn-success")
                 ),
        
        #Data Exploration Tab
        tabPanel("Data Exploration",
                 
                 radioButtons("summary_type", "Choose Summary Type:", choices = c("Categorical Summaries", "Numeric Summaries"), inline = TRUE),
                 
                 conditionalPanel(
                   condition = "input.sumary_type == 'Categorical Summaries'",
                   h4("Categorical Summaries"),
                   selectInput("cat_var1", "Categorical Variable 1:",
                               choices = c("Seasons", "Holiday", "Functioning Day"), selected = "Seasons"),
                   selectInput("cat_var2", "Categorical Variable 2 (optional):",
                               choices = c("None", "Seasons", "Holiday", "Functioning Day"), selected = "None"),
                   withSpinner(plotOutput("cat_plot")),
                   withSpinner(plotOutput("cat_table"))
                 ),
                 
                 
                 )
      )
    )
  )
)

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
  
  #Update the dataset when Apply Filters is clicked
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
}


# Run the application 
shinyApp(ui = ui, server = server)
