library(shiny)
library(tidyverse)
library(ggplot2)

source("helper_plot_new.R")

# Load data ----
data <- readRDS("data/plot_data.rds")

ui <- fluidPage(
  
  # Application title
  titlePanel("Spectator Attendance in European Football"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(h2("Introduction"),
                 p("In this shiny app the home advantage of professional european football leagues is investigated. 
                       Multiple variables are used to display the home advantage in terms of a difference in 
                       e.g. goals between the home and away team. In this way a value of 0 indicates an equal value 
                       (same amount of goals) for the home and away team. 
                       Of main interest is the period between season 10 and 12 as in this time the spectator attendance
                       due to the COVID-19 pandemic was limited. Moreover, the interactive feature offers the chance to see
                       if there are differences in the trend before the COVID-19 pandemic. This offers the unique chance of a natural experiment
                       to investigate the influence of spectator attendance for the home team advantage."),
                 br(),
                 checkboxGroupInput("dvs",
                                    h3("Dependent variables"), 
                                    choices = list("Points" = "Points", 
                                                   "Goals" = "Goals", 
                                                   "Expected Points" = "Expected Points",
                                                   "Shots" = "Shots", 
                                                   "Shots on Target" = "Shots on Target", 
                                                   "Fouls" = "Fouls",
                                                   "Yellow" = "Yellow"),
                                    selected = c("Points","Fouls")),
                 
                 sliderInput("season",
                             h3("Seasons:"),
                             min = 1,
                             max = 12,
                             value = c(1,12)),
                 
                 radioButtons("confidence",
                              h3("Confidence interval"),
                              choices = list("Include" = "Include", "Exclude" = "Exclude"), 
                              selected = "Include"),
                 
                 radioButtons("slope",
                              h3("Slope"), 
                              choices = list("Include" = "Include", 
                                             "Exclude"= "Exclude"),
                              selected = "Include"),
                 radioButtons("covid",
                              h3("Covid"), 
                              choices = list("Include" = "Include", 
                                             "Exclude"= "Exclude"),
                              selected = "Include")
                                           ),
    mainPanel(plotOutput("spectators"))
    )
  )

server <- function(input, output) {
  
  filtered_df <- reactive({
    
    data <- data %>% 
      filter(season >= input$season[1] & season <= input$season[2]) %>%
      filter(dv %in% c(input$dvs[1], input$dvs[2], input$dvs[3], input$dvs[4], 
                     input$dvs[5], input$dvs[6], input$dvs[7]))
    })
  
  output$spectators <- renderPlot({
    plot_function_new(filtered_df(),input$confidence,input$slope,input$covid,input$season[1],input$season[2])
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
