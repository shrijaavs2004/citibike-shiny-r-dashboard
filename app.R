library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(bslib)

# Load data
df <- read_csv("data/201306-citibike-tripdata.csv")

df <- df %>%
  mutate(start_hour = hour(starttime))

# UI
ui <- page_fluid(
  
  titlePanel("Citi Bike Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(
        "start_hour",
        "Select Start Hour",
        min = 0,
        max = 23,
        value = c(0,23)
      )
      
    ),
    
    mainPanel(
      
      uiOutput("avg_trip"),
      
      plotOutput("hour_plot")
      
    )
  )
)

# Server
server <- function(input, output, session){
  
  filtered_df <- reactive({
    
    df %>%
      filter(start_hour >= input$start_hour[1],
             start_hour <= input$start_hour[2])
    
  })
  
  # Value box
  output$avg_trip <- renderUI({
    
    d <- filtered_df()
    
    avg <- mean(d$tripduration, na.rm = TRUE)/60
    
    value_box(
      title = "Average Trip Duration",
      value = paste0(round(avg,1), " mins")
    )
    
  })
  
  # Plot
  output$hour_plot <- renderPlot({
    
    d <- filtered_df()
    
    d %>%
      count(start_hour) %>%
      ggplot(aes(start_hour, n)) +
      geom_col(fill = "#6C5CE7") +
      labs(
        title = "Trips by Start Hour",
        x = "Start Hour",
        y = "Trip Count"
      ) +
      theme_minimal()
    
  })
  
}

shinyApp(ui, server)