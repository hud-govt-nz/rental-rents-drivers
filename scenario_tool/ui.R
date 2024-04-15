library(shiny)  
library(tidyverse)  
library(forecast)  
library(ggplot2)  

### Read in data  
combined_nz <- readRDS('C:/Working/rental-rents-drivers/data/scenario_tool/combined_df.RDS')  
combined_nz_quarterly <- readRDS('C:/Working/rental-rents-drivers/data/scenario_tool/combined_df_quarterly.RDS')  

### Load models  
load('C:/Working/rental-rents-drivers/data/scenario_tool/models.RData')  

# # UI  
ui <- fluidPage(
  titlePanel("RPI Change Prediction Tool"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("earnings_growth_rate", "Annual earnings Growth Rate:",
                  min = -0.4, max = 0.4, value = 0.04, step = 0.01),
      sliderInput("pop_growth_rate", "Annual population Growth Rate:",
                  min = -0.4, max = 0.4, value = 0.04, step = 0.01),
      sliderInput("dwells_growth_rate", "Annual dwellings Growth Rate:",
                  min = -0.4, max = 0.4, value = 0.04, step = 0.01),
      sliderInput("mrate_growth_rate", "Annual 2 year average Mortgage Rate Growth Rate:",
                  min = -0.4, max = 0.4, value = 0.04, step = 0.01),
      sliderInput("cpi_exRent_growth_rate", "Annual CPI Excluding Rents Growth Rate:",
                  min = -0.4, max = 0.4, value = 0.04, step = 0.01),
      sliderInput("unemp_growth_rate", "Annual unemployment Growth Rate:",
                  min = -0.4, max = 0.4, value = 0.04, step = 0.01)
    ),

    mainPanel(
      plotOutput("rpi_change_plot"),
      plotOutput("predictor_variables_plot")
    )
  )
)



# Server  
server <- function(input, output) {  
  
  annual_to_quarterly <- function(annual_rate) {  
    return((1 + annual_rate)^(1/4) - 1)  
  }  
  
  rpi_change_plot <- reactive({  
    input$update  
    
    scenario_growth_rates <- c(annual_to_quarterly(input$earnings_growth_rate),
                               annual_to_quarterly(input$pop_growth_rate),
                               annual_to_quarterly(input$dwells_growth_rate),
                               annual_to_quarterly(input$mrate_growth_rate),
                               annual_to_quarterly(input$cpi_exRent_growth_rate),
                               annual_to_quarterly(input$unemp_growth_rate))

  # scenario_growth_rates <- c(annual_to_quarterly(0.03) , 
  #                            annual_to_quarterly(0.02),
  #                            annual_to_quarterly(0.02),
  #                            annual_to_quarterly(0),
  #                            annual_to_quarterly( 0.004),
  #                            annual_to_quarterly(0.03))
    
    # Get the last observed values of the predictor variables  
    last_values <- as.numeric(combined_nz_quarterly[nrow(combined_nz_quarterly), predictor_vars])  
    
    # Generate future predictor values for the scenario  
    scenario_data <- create_scenario_data(last_values, scenario_growth_rates)  
    colnames(scenario_data) <- predictor_vars
    # Forecast RPI change for the scenario  
    scenario_forecasts_df <- data.frame(scenario_data, date = forecast_dates)  
    colnames(scenario_forecasts_df) <- predictor_vars  
    scenario_rpi_change <- forecast_rpi_change(scenario_forecasts_df, horizon)  
    
    # Combine real and predicted RPI change values  
    real_values <- data.frame(date = combined_nz_quarterly$date,  
                              rpi_change = combined_nz_quarterly$rpi.change,  
                              type = "Real")  
    
    predicted_values_scenario <- data.frame(date = forecast_dates,  
                                            rpi_change = scenario_rpi_change,  
                                            type = "Scenario")  
    
    combined_rpi_changes <- rbind(real_values, predicted_values_scenario)  
    
    # Extract the last real data point  
    last_real_point <- real_values[nrow(real_values), ]  
    
    # Create a helper function to add a segment between the last real data point and the first forecasted data point  
    add_line_segment <- function(data, color, linetype) {  
      geom_segment(aes(x = last_real_point$date, xend = data$date[1],  
                       y = last_real_point$rpi_change, yend = data$rpi_change[1]),  
                   color = color, linetype = linetype)  
    }  
    # Plot the real and predicted RPI change values with line segments  
    ggplot(combined_rpi_changes, aes(x = date, y = rpi_change, color = type)) +  
      geom_line() +  
      add_line_segment(predicted_values_scenario, "red", "dashed") +  
      labs(title = "Real and Predicted RPI Change Values",  
           x = "Date",  
           y = "RPI Change",  
           color = "Type") +  
      scale_color_manual(values = c("Real" = "blue", "Scenario" = "red")) +  
      theme_minimal()  
  })  
  
  predictor_variables_plot <- reactive({  
    input$update  
    
    scenario_growth_rates <- c(annual_to_quarterly(input$earnings_growth_rate),  
      annual_to_quarterly(input$pop_growth_rate),  
      annual_to_quarterly(input$dwells_growth_rate),  
      annual_to_quarterly(input$mrate_growth_rate),  
      annual_to_quarterly(input$cpi_exRent_growth_rate),  
      annual_to_quarterly(input$unemp_growth_rate))   
    
    # Get the last observed absolute values of the predictor variables  
    absolute_predictor_vars <- gsub(".change", "", predictor_vars)  
    last_values <- as.numeric(combined_nz_quarterly[nrow(combined_nz_quarterly), absolute_predictor_vars])    
    
    # Generate future predictor values for the scenario    
    scenario_data <- create_scenario_data(last_values, scenario_growth_rates)    
    colnames(scenario_data) <- absolute_predictor_vars
    
    # Add a date column to the scenario data  
    scenario_data_with_dates <- data.frame(scenario_data, date = forecast_dates)  
    
    # Reshape the data to a long format for the facet plot  
    scenario_data_long <- scenario_data_with_dates %>%  
      pivot_longer(cols = -date, names_to = "variable", values_to = "value")  
    
    # Create a facet plot for the predictor variables  
    ggplot(scenario_data_long, aes(x = date, y = value)) +  
      geom_line() +  
      facet_wrap(~ variable, scales = "free_y") + 
      labs(title = "Forecasted Values of Predictor Variables",
           subtitle = "Quarterly values given chosen rate of change",
           x = "Date",  
           y = "Value") +  
      theme_minimal()        
  })  
  
  
  output$rpi_change_plot <- renderPlot({  
    rpi_change_plot()  
  })   
  
  output$predictor_variables_plot <- renderPlot({    
    predictor_variables_plot()    
  })  
}  

# Run the app  
shinyApp(ui = ui, server = server, options = list(height = 1080)) 