# Covid Assesment Tool in Venezuela

# Functions for Visualiation
# Made by Samir
source("daily_visualizations.R") 
# Made by Luis 
source("Trend_Lines_Functions.R")


# Script to input data from Google Sheets
source("Input_Data_Script.R")



# Loading Libraries
library(shinydashboard)
library(shiny)
library(tidyverse)
library(plotly)


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      
      selectInput(inputId = 'State',
                  "State",
                  choices = covid_table %>% select(federal_entity) %>% 
                    distinct() %>% 
                    arrange(federal_entity) %>% 
                    drop_na())
      
    ),
    dashboardBody(
      fluidRow(box(plotlyOutput('bar_cases')))
      
      
    )
)

server <- function(input, output) { 
  
  output$bar_cases <- renderPlotly(
    
    
    bar_one_state_visz(covid_table, input$State)
    
    
  )
  
  
  }

shinyApp(ui, server)