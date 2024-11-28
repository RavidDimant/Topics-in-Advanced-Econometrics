# Load necessary libraries
install.packages("VGAM")
install.packages("sampleSelection")
install.packages("pscl")
install.packages("truncreg")

library(truncreg)
library(VGAM)
library(sampleSelection)
library(pscl)
library(ggplot2)

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

# Overview of the dataset
head(CVD)
summary(CVD)

# We'll focus on the variable 'BMI' as the dependent variable and 'Exercise', 'Depression', 
# 'Sex', 'Height_.cm.', 'Weight_.kg.', 'Fruit_Consumption', 'Green_Vegetables_Consumption' as the independent variables

# Create a truncated variable
CVD_truncated <- CVD[CVD$BMI < 35, ]
CVD_truncated$BMI_truncated <- CVD_truncated$BMI


# Fit a truncated regression model
truncated_model <- truncreg(BMI_truncated ~ Exercise + Depression + Sex + Height_.cm. + Weight_.kg.
                            + Fruit_Consumption + Green_Vegetables_Consumption , 
                            data = CVD_truncated, point = 35, direction = "right")
summary(truncated_model)



# Calculate residuals for the truncated model
truncated_residuals <- resid(truncated_model)

# Plot residuals for truncated model
ggplot(CVD_truncated, aes(x = predict(truncated_model), y = truncated_residuals)) +
  geom_point() +
  labs(title = "Truncated Model Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()


# second truncation 
# Create a truncated variable
CVD_truncated <- CVD[CVD$BMI < 25, ]
CVD_truncated$BMI_truncated <- CVD_truncated$BMI


# Fit a truncated regression model
truncated_model <- truncreg(BMI_truncated ~ Exercise + Depression + Sex + Height_.cm. + Weight_.kg.
                            + Fruit_Consumption + Green_Vegetables_Consumption , 
                            data = CVD_truncated, point = 25, direction = "right")
summary(truncated_model)


# Explanation of Truncated Model Output
# Similar to the Tobit model, this output provides coefficients, standard errors, z-values, and p-values for each predictor.
# The significant predictors help identify which variables are associated with the survival time in the truncated sample.
# Sigma (σ): The standard deviation of the error term. In this example, 𝜎~~  0 , 
# indicating the average deviation of the observed values from the predicted values is approximately 0.

# Plot the truncated data
ggplot(CVD_truncated, aes(x = Green_Vegetables_Consumption, y = BMI_truncated)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Truncated Data: BMI vs. Green_Vegetables_Consumption",
       x = "Green_Vegetables_Consumption",
       y = "BMI") +
  theme_minimal()
