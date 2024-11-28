# Load the yfR package
library(yfR)

library(tidyverse)
library(lmtest)
library(tseries)
library(urca)
library(ggplot2)
library(dplyr)

# libraries for ARIMA
library(forecast)

# Set options for algorithm
my_tickers <- c('SPY', '^NDX')  # Add both SPY and NASDAQ-100 index
first_date <- as.Date("2019-09-19")  # 5 years ago
last_date <- as.Date("2024-09-19")

# Fetch data for both SPY and NASDAQ-100
df_yf <- yf_get(tickers = my_tickers, 
                first_date = first_date,
                last_date = last_date)

# View the structure of the dataframe
head(df_yf)

# Separate the data for SPY and NASDAQ
df_spy <- df_yf %>%
  filter(ticker == 'SPY') %>%
  select(ref_date, price_adjusted) %>%
  rename(SPY = price_adjusted)

df_nasdaq <- df_yf %>%
  filter(ticker == '^NDX') %>%
  select(ref_date, price_adjusted) %>%
  rename(NASDAQ = price_adjusted)

# Merge the two datasets by date (ref_date)
df_yf_clean <- df_spy %>%
  inner_join(df_nasdaq, by = "ref_date") %>%
  rename(date = ref_date)

# Inspect the cleaned dataframe
head(df_yf_clean)

# Log-transform the prices to stabilize variance and interpret as growth rates
df_yf_clean <- df_yf_clean %>%
  mutate(log_SPY = log(SPY),
         log_NASDAQ = log(NASDAQ))

# Remove any rows with missing values (NAs)
df_yf_clean <- df_yf_clean %>%
  drop_na(log_SPY, log_NASDAQ)

# Ensure that the differenced columns have the same length as the original
df_yf_clean <- df_yf_clean %>%
  mutate(d_log_SPY = c(NA, diff(log_SPY)),
         d_log_NASDAQ = c(NA, diff(log_NASDAQ))) %>%
  drop_na()  # Drop the NAs created by the differencing process

# Inspect the transformed dataframe
head(df_yf_clean)

df_yf_clean$t_trend = 1:nrow(df_yf_clean)

# Check for stationarity using the Augmented Dickey-Fuller (ADF) test
adf_SPY <- ur.df(df_yf_clean$log_SPY, type = "trend", lags = 3)
adf_NASDAQ <- ur.df(df_yf_clean$log_NASDAQ, type = "trend", lags = 3)

# Display results of ADF tests
summary(adf_SPY)
summary(adf_NASDAQ)


##############################################
### ADL - Autoregressive Distributed Lag #####
##############################################

# Difference the log-transformed SPY and NASDAQ to make them stationary
df_yf_clean <- df_yf_clean %>%
  mutate(d_log_SPY = c(NA, diff(log_SPY)),   # First difference of log_SPY
         d_log_NASDAQ = c(NA, diff(log_NASDAQ))) # First difference of log_NASDAQ

# Remove NA rows after differencing
df_yf_clean <- df_yf_clean %>% drop_na()

# Check stationarity again using the Augmented Dickey-Fuller (ADF) test for differenced data
adf_d_log_SPY <- ur.df(df_yf_clean$d_log_SPY, type = "trend", lags = 3)
summary(adf_d_log_SPY)

adf_d_log_NASDAQ <- ur.df(df_yf_clean$d_log_NASDAQ, type = "trend", lags = 3)
summary(adf_d_log_NASDAQ)


##############################################
### Optimization Function for ARIMA degree ###
##############################################

optimize_arima_2 <- function(data, ar_vec, ma_vec, lag_error_test = 10, alpha = 0.05) {
  
  AR_val <- 1  # Best AR value
  MA_val <- 1  # Best MA value
  i_val <- 0   # Differencing degree
  differenced_series <- data  # Initial data series
  
  # Perform the Augmented Dickey-Fuller (ADF) test to check for stationarity
  adf_test <- adf.test(differenced_series, alternative = "stationary")
  
  # Check if differencing is needed
  if (adf_test$p.value > alpha) {
    # Perform differencing until the series becomes stationary
    while(adf_test$p.value > alpha) {
      i_val <- i_val + 1
      differenced_series <- diff(data, differences = i_val)
      adf_test <- adf.test(differenced_series, alternative = "stationary")
    }
  }
  
  # Initialize the minimum AIC with a default ARIMA(1, i_val, 1)
  Min_AIC <- AIC(arima(data, order = c(1, i_val, 1)))
  
  # Iterate over the specified AR and MA vectors to find the best combination
  for (i in ar_vec) {
    for (j in ma_vec) {
      model_ARIMA <- arima(data, order = c(i, i_val, j))  # Fit the ARIMA model
      AIC_ARIMA <- model_ARIMA$aic  # Calculate AIC
      
      # Check if the current model has a lower AIC
      if (AIC_ARIMA < Min_AIC) {
        Min_AIC <- AIC_ARIMA
        AR_val <- i
        MA_val <- j
      }
    }
  }
  
  # Fit the best ARIMA model
  best_model <- arima(data, order = c(AR_val, i_val, MA_val))
  
  # Perform the Ljung-Box test to check residual noise
  lb_test <- Box.test(best_model$residuals, lag = lag_error_test, type = "Ljung-Box")
  
  # If the residuals are sufficiently white noise, return the best model details
  if (lb_test$p.value >= alpha) {
    return(paste("The minimum AIC value is AIC = ", Min_AIC,
                 " and the ARIMA degree is AR = ", AR_val,
                 " I = ", i_val, " and MA = ", MA_val, ".", sep = ""))
  } else {
    return("Residuals are not white noise. Consider testing more AR and MA lags.")
  }
}

# Example: Running the optimize_arima_2 function on d_log_NASDAQ

ar_vec <- 0:5  # Range of AR values
ma_vec <- 0:5  # Range of MA values

# Run the optimization function on the differenced log NASDAQ data
result <- optimize_arima_2(df_yf_clean$d_log_NASDAQ, ar_vec, ma_vec, lag_error_test = 10, alpha = 0.05)

# Output the result
cat(result, "\n")
# The minimum AIC value is AIC = -6851.39292892914 
# and the ARIMA degree is AR = 5 I = 0 and MA = 4. 


##############################################
############### Plotting ACF PACF ############
############### Forecast ARMA     ############
##############################################

# Fit the ARMA(5, 4) model (since I = 0 from the ARIMA result)
arma_model <- arima(df_yf_clean$d_log_NASDAQ, order = c(5, 0, 4))

# Summary of the ARMA model
summary(arma_model)

### Plot ACF and PACF of the ARMA residuals ###
par(mfrow = c(1, 2))  # Set plotting layout to show 2 plots side by side

acf(arma_model$residuals, main = 'ACF of ARMA Residuals')
pacf(arma_model$residuals, main = 'PACF of ARMA Residuals')

par(mfrow = c(1, 1))  # Reset to default plotting layout

### Forecasting the last 7 observations ###

# Split the data: keep first (n-7) for fitting and forecast the last 7
n <- nrow(df_yf_clean)
train_data <- df_yf_clean$d_log_NASDAQ[1:(n-7)]  # Training data (exclude last 7 observations)
test_data <- df_yf_clean$d_log_NASDAQ[(n-6):n]  # Actual last 7 observations for comparison
dates <- df_yf_clean$date[(n-6):n]  # Dates for the last 7 observations

# Fit the ARMA model using the training data
arma_model_train <- arima(train_data, order = c(5, 0, 4))

# Forecast the next 7 values
forecast_arma <- predict(arma_model_train, n.ahead = 7)

# Extract the predicted and actual values
predicted <- forecast_arma$pred  # Predicted values
actual <- test_data  # Actual values from the dataset

### Plotting the predicted values compared to the actual observations with tilted dates ###

# Create a data frame to combine actual and predicted values
forecast_df <- data.frame(Date = dates, Actual = actual, Predicted = predicted)

# Set up the plotting area with dates on the x-axis and tilted labels
plot(forecast_df$Date, forecast_df$Actual, col = "blue", pch = 19, type = "b", lwd = 2, 
     ylim = range(c(forecast_df$Actual, forecast_df$Predicted)), 
     xlab = "Date", ylab = "Values", main = "Actual vs Predicted (Last 7 Observations)", xaxt = "n")

# Add predicted values in red
points(forecast_df$Date, forecast_df$Predicted, col = "red", pch = 17, type = "b", lwd = 2)

# Format the x-axis to display dates and tilt the labels
axis(1, at = forecast_df$Date, labels = format(forecast_df$Date, "%Y-%m-%d"), las = 2)

# Add a legend
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), pch = c(19, 17), lwd = 2)
