# Load necessary libraries
library(quantreg)
library(ggplot2)
library(dplyr)
library(reshape2)

# Load the dataset
CVD <- read.csv("C:/Users/user/Desktop/CVD_new.csv")
CVD$General_Health <- as.factor(CVD$General_Health)
CVD$Checkup <- as.factor(CVD$Checkup)
CVD$Exercise <- as.factor(CVD$Exercise)
CVD$Heart_Disease <- as.factor(CVD$Heart_Disease)
CVD$Skin_Cancer <- as.factor(CVD$Skin_Cancer)
CVD$Other_Cancer <- as.factor(CVD$Other_Cancer)
CVD$Depression <- as.factor(CVD$Depression)
CVD$Diabetes <- as.factor(CVD$Diabetes)
CVD$Arthritis <- as.factor(CVD$Arthritis)
CVD$Sex <- as.factor(CVD$Sex)
CVD$Age_Category <- as.factor(CVD$Age_Category)
CVD$Smoking_History <- as.factor(CVD$Smoking_History)

# Display the first few rows of the dataset
head(CVD)

# Define the formula for the regression model
formula <- BMI ~ General_Health + Checkup  + Exercise + Heart_Disease + Skin_Cancer + Other_Cancer + Depression + Diabetes + Arthritis + Sex + Age_Category + Height_.cm. + Weight_.kg. + Smoking_History + Alcohol_Consumption + Fruit_Consumption + Green_Vegetables_Consumption + FriedPotato_Consumption 

# Fit the OLS model
ols_model <- lm(formula, data = CVD)
summary(ols_model)

# Now we delete features for better explaining the data
# We delete all features with big p_value 

formula <- BMI ~ Exercise + Depression + Sex + Height_.cm. + Weight_.kg. + Fruit_Consumption + Green_Vegetables_Consumption

ols_model <- lm(formula, data = CVD)

# Display the summary of the OLS model
summary(ols_model)


# Fit quantile regression models for different quantiles

tau_values <- c(0.1, 0.25, 0.5, 0.75, 0.9)
qr_models <- lapply(tau_values, function(tau) rq(formula, data = CVD, tau = tau))

# Display the summary of each quantile regression model

qr_summaries <- lapply(qr_models, function(model) summary(model, se = "boot"))
names(qr_summaries) <- paste0("tau_", tau_values)
qr_summaries

# Extract OLS coefficients and convert to a data frame with a 'tau' column
ols_coef <- coef(ols_model)
ols_coef_df <- as.data.frame(t(ols_coef))
ols_coef_df$tau <- "OLS"

# Extract quantile regression coefficients and convert to a data frame with a 'tau' column
qr_coefs <- lapply(qr_models, coef)
qr_coef_df <- do.call(rbind, lapply(1:length(qr_coefs), function(i) {
  data.frame(t(qr_coefs[[i]]), tau = tau_values[i])
}))
colnames(qr_coef_df) <- c(names(ols_coef), "tau")

# Combine OLS and quantile regression coefficients for comparison
coef_comparison <- rbind(ols_coef_df, qr_coef_df)

# Melt the data for plotting
coef_melt <- melt(coef_comparison, id.vars = "tau")

# Plot the coefficients
ggplot(coef_melt, aes(x = tau, y = value, color = variable, group = variable)) +
  geom_line() +
  labs(title = "OLS vs Quantile Regression Coefficients", x = "Quantile/OLS", y = "Coefficient") +
  theme_minimal()

# Explanation:
# This plot compares the coefficients of the OLS model with those from the quantile regression models across different quantiles.
# Each line represents the change in the coefficient of a specific predictor across the quantiles.

# Calculate residuals for the OLS model
ols_residuals <- resid(ols_model)

# Calculate residuals for the median regression model
qr_residuals <- resid(qr_models[[3]])

# Plot residuals for OLS
ggplot(CVD, aes(x = predict(ols_model), y = ols_residuals)) +
  geom_point() +
  labs(title = "OLS Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Explanation:
# This plot shows the residuals of the OLS model against the fitted values.
# Ideally, the residuals should be randomly scattered around zero, indicating a good fit.

# Plot residuals for quantile regression
ggplot(CVD, aes(x = predict(qr_models[[3]]), y = qr_residuals)) +
  geom_point() +
  labs(title = "Quantile Regression Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Explanation:
# This plot shows the residuals of the median quantile regression model against the fitted values.
# Similar to the OLS residual plot, a good fit is indicated by residuals scattered randomly around zero.


# Predict values for OLS and quantile regression
ols_predictions <- predict(ols_model)
qr_predictions <- sapply(qr_models, predict)

# Combine predictions with the original data
CVD_pred <- cbind(CVD, ols_predictions, qr_predictions)
colnames(CVD_pred)[(ncol(CVD)+1):(ncol(CVD)+length(tau_values)+1)] <- c("OLS", paste0("Q", tau_values))

# Melt the data for plotting
CVD_melt <- melt(CVD_pred, id.vars = colnames(CVD))

# Plot predictions
ggplot(CVD_melt, aes(x = BMI, y = value, color = variable)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("OLS" = "blue", "Q0.1" = "red", "Q0.25" = "orange", "Q0.5" = "green", "Q0.75" = "purple", "Q0.9" = "brown")) +
  labs(title = "OLS vs Quantile Regression Predictions", x = "Actual Values", y = "Predicted Values", color = "Model") +
  theme_minimal()

# Explanation:
# This plot compares the actual values of the response variable (medv) with the predicted values from 
# both the OLS and quantile regression models. It helps to visualize the performance of the models.


# Perform Wald tests for quantile regression coefficients
qr_tests <- lapply(qr_models, function(model) {
  summary(model, se = "boot")$coefficients
})



# Combine the coefficients and p-values into a data frame
qr_pvals_df <- do.call(rbind, lapply(1:length(qr_tests), function(i) {
  coefs <- qr_tests[[i]]
  data.frame(tau = tau_values[i], variable = rownames(coefs), coef = coefs[, 1], p_value = coefs[, 4])
}))

# Plot the p-values
ggplot(qr_pvals_df, aes(x = tau, y = p_value, color = variable, group = variable)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dashed") +
  labs(title = "P-values of Quantile Regression Coefficients", x = "Quantile", y = "P-value") +
  theme_minimal()

# Explanation:
# This plot shows the p-values of the quantile regression coefficients across different quantiles.
# The dashed line at y = 0.05 indicates the common significance level. 
# P-values below this line suggest that the coefficients are statistically significant at that quantile.