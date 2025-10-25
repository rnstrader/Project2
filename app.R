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
      h2("Select Variables:"),
      
      #X variable selector
    #  selectizeInput("corr_x", "x Variable", selected = numeric_vars[1], choices = numeric_vars, multiple = FALSE),
      
      #Y variable selector
     # selectizeInput("corr_y", "y Variable", selected = numeric_vars[2], choices = numeric_vars, multiple = FALSE),
      
      
      br(),
      
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
  
  
  ##Correlation tab
  #This code makes sure the select boxes update so they can't select the same variable in both!
  #Preventing the input for x and y variable being identical
  observeEvent(input$corr_x, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices,
                           selected = ifelse(corr_y %in% choices, corr_y, choices[1]))
    }
  })
  
  observeEvent(input$corr_y, {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x != corr_y){
      choices <- choices[-which(choices == corr_y)]
      updateSelectizeInput(session,
                           "corr_x",
                           choices = choices,
                           selected = ifelse(corr_x %in% choices, corr_x, choices[1]))
    }
  })
  
  
  #creating sample_corr, a reactiveValues object that has two elements that default to null
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  
  #Code to look for the action button (bike_sample)
  
  observeEvent(input$bike_sample, {
    if(input$seas == "all"){
      Seas_sub <- SeasVals
    } else if(input$seas == "Winter"){
      Seas_sub <- SeasVals["1"]
    } else if(input$seas == "Autumn"){
      Seas_sub <- SeasVals["2"]
    } else if(input$seas == "Spring"){
      Seas_sub <- SeasVals["3"]
    } else if(input$seas == "Summer"){
      Seas_sub <- SeasVals["4"]
    }
    
    if(input$hol == "all"){
      hol_sub <- HolVals
    } else if(input$hol == "Holiday"){
      hol_sub <- HolVals["1"]
    } else {
      hol_sub <- HolVals["2"]
    }
    
    
    corr_vars <- c(input$corr_x, input$corr_y)
    
    subsetted_data <- my_sample |>
      filter(#cat vars first
        Seas_fac %in% Seas_sub,
        Hol_fac %in% hol_sub,
      ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}
    
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
