# Covid Assesment Tool in Venezuela


library(shinydashboard)
library(shiny)


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