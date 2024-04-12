
# Drivers of rents predictive tool using the linear models from the Rents Paper
# Mariona, 2024
# This script is going to use the models specified and trained in the script 3b.final.R to create predictions for the variable rpi.change

library(tidyverse)
library(forecast)
library(ggplot2)

### Read in data
combined_nz <- readRDS('data/scenario_tool/combined_df.RDS')
combined_nz_quarterly <- readRDS('data/scenario_tool/combined_df_quarterly.RDS')

### Load models
load('data/scenario_tool/models.RData')

### Create predictions using one of the models -- all models have been trained with quarterly data
### 1 - Check the predictive power of the model 
i = 1
## model1f (we may extend it to the rest of the models later)
model1f

new_data <- combined_nz_quarterly %>%   
  dplyr::select(rpi.change, earnings.change, pop.change, dwells_nz_stats.change, mrate.change, cpi_exRent.change, unemp.change)  

#  Use the predict() function to make one point predictions with new data:
predictions <- predict(model1f, newdata = new_data) 

# Display the predictions:
results_with_date <- data.frame(date = combined_nz_quarterly$date,  
                                actual = combined_nz_quarterly$rpi.change,  
                                predicted = predictions)  

ggplot(results_with_date, aes(x = date)) +  
  geom_line(aes(y = actual, color = "Actual")) +  
  geom_line(aes(y = predicted, color = "Predicted")) +  
  labs(title = "Actual and Predicted Values Over Time",  
       x = "Date",  
       y = "Values") +  
  scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +  
  theme_minimal()  

### 2 - Create forecasts of rpi.change for 10 years ahead
# Set the forecasting horizon (10 years = 40 quarters)  
horizon <- 20  

## 2.A - Create forecasted values of predictor variables using an ARIMA forecast
# Function to forecast a time series using ARIMA  
forecast_arima <- function(series, horizon) {  
  model <- auto.arima(series)  
  forecast(model, h = horizon)$mean  
}  

# Forecast each predictor variable  
predictor_vars <- c("earnings.change",
                    "pop.change",
                    "dwells_nz_stats.change",
                    "mrate.change", 
                    "cpi_exRent.change",
                    "unemp.change")

forecasts_list <- lapply(combined_nz_quarterly[predictor_vars], forecast_arima, horizon = horizon)  

# Generate a sequence of quarterly dates for the forecasts  
forecast_dates <- seq.Date(from = max(combined_nz_quarterly$date) + lubridate::days(90), by = "quarter", length.out = horizon)  

# Combine forecasts into a data frame  
forecasts_df <- as.data.frame(forecasts_list)  
rownames(forecasts_df) <- forecast_dates  

# Convert the forecasts_df to a long format for plotting  
forecasts_df$Date <- rownames(forecasts_df)  
forecasts_df$Date <- as.Date(forecasts_df$Date)  
forecasts_long <- gather(forecasts_df, key = "Variable", value = "Forecast", -Date)  

# Plot the forecasted values for each predictor variable  
ggplot(forecasts_long, aes(x = Date, y = Forecast)) +  
  geom_line() +  
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +  
  labs(title = "Forecasted Values of Predictor Variables",  
       x = "Date",  
       y = "Forecasted Value") +  
  theme_minimal()  

# Or plot each forecasted variable individually
plot(forecast(auto.arima(combined_nz_quarterly$earnings.change)), main = 'Earnings change')
plot(forecast(auto.arima(combined_nz_quarterly$pop.change)), main = 'Population change')
plot(forecast(auto.arima(combined_nz_quarterly$dwells_nz_stats.change)), main = 'Dwellings change')
plot(forecast(auto.arima(combined_nz_quarterly$mrate.change)), main = 'M rate change')
plot(forecast(auto.arima(combined_nz_quarterly$cpi_exRent.change)), main = 'CPI excluding rents change')
plot(forecast(auto.arima(combined_nz_quarterly$unemp.change)), main = 'Unemployment change')

# Run the predictor model using the forecasted predictor variables and using the lag of RPI
detach(package:dplyr, unload = TRUE)  # need to detach dplyr because it interferes with the predict() function

# Initialize a vector to store RPI change predictions  
rpi_change_predictions <- numeric(horizon)  

# Iteratively make predictions for RPI change using model1f  
for (t in 1:horizon) {  
  if (t == 1) {  
    # For the first prediction, use the last observed value of RPI change as the lag  
    lag_rpi_change <- combined_nz_quarterly$rpi.change[length(combined_nz_quarterly$rpi.change)]  
  } else {  
    # For subsequent predictions, use the previous prediction as the lag  
    lag_rpi_change <- rpi_change_predictions[t - 1]  
  }   # Add the lag of RPI change to the forecasts_df  
  forecasts_df$rpi.change <- lag_rpi_change  
  
  # Make a prediction for RPI change using model1f and the current values of the predictor variables  
  rpi_change_predictions[t] <- predict(model1f, newdata = forecasts_df[t,])  
}  

# Plot the predictions 
# Combine real and predicted RPI change values  
real_values <- data.frame(date = combined_nz_quarterly$date,  
                          rpi_change = combined_nz_quarterly$rpi.change,  
                          type = "Real")  

predicted_values <- data.frame(date = forecast_dates,  
                               rpi_change = rpi_change_predictions,  
                               type = "Predicted")  

combined_rpi_changes <- rbind(real_values, predicted_values)  

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
  add_line_segment(predicted_values, "red", "dashed") +  
  labs(title = "Real and Predicted RPI Change Values",  
       x = "Date",  
       y = "RPI Change",  
       color = "Type") +  
  scale_color_manual(values = c("Real" = "blue", "Predicted" = "red")) +  
  theme_minimal()  

### 2.B - Create forecasted values of predictor variables using two made-up scenarios  
scenario1_growth_rates <- c(0.01, 0.02, 0.03, 0.02, 0.01, -0.01)  # Example growth rates for scenario 1  
scenario2_growth_rates <- c(0.02, 0.03, 0.01, 0.03, 0.02, -0.02)  # Example growth rates for scenario 2  

# Function to create future predictor values based on a scenario  
create_scenario_data <- function(last_values, growth_rates) {  
  future_values <- matrix(0, nrow = horizon, ncol = length(last_values))  
  for (i in 1:length(last_values)) {  
    future_values[, i] <- last_values[i] * cumprod(1 + rep(growth_rates[i], horizon))  
  }  
  return(future_values)  
}  

# Get the last observed values of the predictor variables  
last_values <- as.numeric(combined_nz_quarterly[nrow(combined_nz_quarterly), predictor_vars])  

# Generate future predictor values for each scenario  
scenario1_data <- create_scenario_data(last_values, scenario1_growth_rates)  
scenario2_data <- create_scenario_data(last_values, scenario2_growth_rates)  

# Combine scenario data into a list  
scenario_data_list <- list(scenario1 = scenario1_data, scenario2 = scenario2_data)  

# Convert the scenario data to a long format for plotting  
plot_data <- lapply(names(scenario_data_list), function(scenario_name) {  
  data <- as.data.frame(scenario_data_list[[scenario_name]])  
  colnames(data) <- predictor_vars  
  data$Quarter <- 1:horizon  
  data$Scenario <- scenario_name  
  return(data)  
})  
plot_data <- do.call(rbind, plot_data)  
plot_data_long <- gather(plot_data, key = "Variable", value = "Value", -Quarter, -Scenario)  

# Plot the scenario-based predictor variable values  
ggplot(plot_data_long, aes(x = Quarter, y = Value, color = Scenario)) +  
  geom_line() +  
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +  
  labs(title = "Scenario-based Predictor Variable Values",  
       x = "Quarters Ahead",  
       y = "Value",  
       color = "Scenario") +  
  theme_minimal()  

# Function to forecast RPI change using model1f and a specific scenario  
forecast_rpi_change <- function(forecasts_df, horizon) {  
  # Initialize a vector to store RPI change predictions  
  rpi_change_predictions <- numeric(horizon)  
  
  # Iteratively make predictions for RPI change using model1f  
  for (t in 1:horizon) {  
    if (t == 1) {  
      # For the first prediction, use the last observed value of RPI change as the lag  
      lag_rpi_change <- combined_nz_quarterly$rpi.change[length(combined_nz_quarterly$rpi.change)]  
    } else {  
      # For subsequent predictions, use the previous prediction as the lag  
      lag_rpi_change <- rpi_change_predictions[t - 1]  
    }  
    # Add the lag of RPI change to the forecasts_df  
    forecasts_df$rpi.change <- lag_rpi_change  
    
    # Make a prediction for RPI change using model1f and the current values of the predictor variables  
    rpi_change_predictions[t] <- predict(model1f, newdata = forecasts_df[t, ])  
  }  
  
  return(rpi_change_predictions)  
}  

# Forecast RPI change for each scenario  
scenario1_forecasts_df <- data.frame(scenario1_data, date = forecast_dates)  
colnames(scenario1_forecasts_df) <- predictor_vars  
scenario1_rpi_change <- forecast_rpi_change(scenario1_forecasts_df, horizon)  

scenario2_forecasts_df <- data.frame(scenario2_data, date = forecast_dates)  
colnames(scenario2_forecasts_df) <- predictor_vars  
scenario2_rpi_change <- forecast_rpi_change(scenario2_forecasts_df, horizon)  

# Combine the RPI change forecasts for both scenarios into a data frame  
rpi_change_scenarios <- data.frame(  
  Quarter = 1:horizon,  
  Scenario1 = scenario1_rpi_change,  
  Scenario2 = scenario2_rpi_change  
)  

# Convert the data frame to a long format for plotting  
rpi_change_scenarios_long <- gather(rpi_change_scenarios, key = "Scenario", value = "RPI_Change", -Quarter)  

# Plot the RPI change forecasts for both scenarios  
ggplot(rpi_change_scenarios_long, aes(x = Quarter, y = RPI_Change, color = Scenario)) +  
  geom_line() +  
  labs(title = "Forecasted RPI Change for Scenario 1 and Scenario 2",  
       x = "Quarters Ahead",  
       y = "RPI Change",  
       color = "Scenario") +  
  theme_minimal()  

# Plot real data vs scenario data:
# Combine real and predicted RPI change values  
real_values <- data.frame(date = combined_nz_quarterly$date,  
                          rpi_change = combined_nz_quarterly$rpi.change,  
                          type = "Real")  

predicted_values_scenario1 <- data.frame(date = forecast_dates,  
                                         rpi_change = scenario1_rpi_change,  
                                         type = "Scenario 1")  

predicted_values_scenario2 <- data.frame(date = forecast_dates,  
                                         rpi_change = scenario2_rpi_change,  
                                         type = "Scenario 2")  

combined_rpi_changes <- rbind(real_values, predicted_values_scenario1, predicted_values_scenario2)  

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
  add_line_segment(predicted_values_scenario1, "red", "dashed") +  
  add_line_segment(predicted_values_scenario2, "green", "dashed") +  
  labs(title = "Real and Predicted RPI Change Values",  
       x = "Date",  
       y = "RPI Change",  
       color = "Type") +  
  scale_color_manual(values = c("Real" = "blue", "Scenario 1" = "red", "Scenario 2" = "green")) +  
  theme_minimal()  

# Plot the real and predicted RPI change values  
ggplot(combined_rpi_changes, aes(x = date, y = rpi_change, color = type)) +  
  geom_line() +  
  labs(title = "Real and Predicted RPI Change Values",  
       x = "Date",  
       y = "RPI Change",  
       color = "Type") +  
  scale_color_manual(values = c("Real" = "blue", "Scenario 1" = "red", "Scenario 2" = "green")) +  
  theme_minimal()  
