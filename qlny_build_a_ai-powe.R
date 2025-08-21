# Load necessary libraries
library(shiny)
library(tidyverse)
library(caret)
library(plotly)
library prophet)

# Define data model for web app monitoring
web_app_data <- list(
  url = c("https://example.com", "https://example.io", "https://example.net"),
  response_time = c(200, 150, 300),
  response_code = c(200, 404, 200),
  cpu_usage = c(0.5, 0.3, 0.7),
  memory_usage = c(0.2, 0.1, 0.4),
  traffic = c(100, 50, 200),
  timestamp = c(Sys.time(), Sys.time() - 1, Sys.time() - 2)
)

# Define AI model for anomaly detection
ai_model <- function(data) {
  # Train prophet model on historical data
  prophet_model <- prophet(data, seasonality = "daily")
  
  # Make predictions on new data
  future <- make_future_dataframe(prophet_model, periods = 10)
  forecast <- predict(prophet_model, future)
  
  # Identify anomalies based on predicted values
  anomalies <- forecast %>%
    mutate(anomaly = ifelse(yhat > yhat_upper, 1, 0))
  
  return(anomalies)
}

# Define shiny app for monitoring
ui <- fluidPage(
  titlePanel("Web App Monitor"),
  sidebarLayout(
    sidebarPanel(
      selectInput("url", "Select URL:", web_app_data$url),
      dateInput("date", "Select Date:", value = Sys.Date())
    ),
    mainPanel(
      plotlyOutput("response_time_plot"),
      plotlyOutput("cpu_usage_plot"),
      plotlyOutput("memory_usage_plot"),
      tableOutput("anomaly_table")
    )
  )
)

server <- function(input, output) {
  # Filter data based on user input
  filtered_data <- reactive({
    web_app_data %>%
      filter(url == input$url, timestamp >= input$date)
  })
  
  # Create response time plot
  output$response_time_plot <- renderPlotly({
    filtered_data() %>%
      ggplot(aes(x = timestamp, y = response_time)) + 
      geom_line() + 
      theme_classic()
  })
  
  # Create cpu usage plot
  output$cpu_usage_plot <- renderPlotly({
    filtered_data() %>%
      ggplot(aes(x = timestamp, y = cpu_usage)) + 
      geom_line() + 
      theme_classic()
  })
  
  # Create memory usage plot
  output$memory_usage_plot <- renderPlotly({
    filtered_data() %>%
      ggplot(aes(x = timestamp, y = memory_usage)) + 
      geom_line() + 
      theme_classic()
  })
  
  # Create anomaly table
  output$anomaly_table <- renderTable({
    ai_model(filtered_data()) %>%
      filter(anomaly == 1) %>%
      select(timestamp, response_time, cpu_usage, memory_usage)
  })
}

# Run shiny app
shinyApp(ui = ui, server = server)