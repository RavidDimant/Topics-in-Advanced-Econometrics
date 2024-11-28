library(plm)
library(Ecdat)
library(ggplot2)

# Load the Cigar dataset from the Ecdat package
data("Cigar", package = "Ecdat")

# View the first few rows of the dataset
head(Cigar)

# Explanation of the variables in the dataset as explained in the lecture:
# - state: state abbreviation
# - year:the year
# - price: price per pack of cigarettes
# - pop: population
# - pop16: population above the age of 16
# - cpi: consumer price index (1983=100)
# - ndi: per capita disposable income
# - sales: cigarette sales in packs per capita
# - pimin: minimum price in adjoining states per pack of cigarettes

# Convert year to a factor to create time dummies
Cigar$year <- factor(Cigar$year)

# Define the model formula including time dummies
# The formula is adjusted based on available variables in the Cigar dataset

formula <- log(sales) ~ log(price) + log(pimin) + log(ndi) + log(pop) + year

# Estimate the fixed effects model
fixed_effects_model <- plm(formula, data = Cigar, index = c("state", "year"), model = "within")

# Summary of the fixed effects model to view coefficients and statistical significance
summary(fixed_effects_model)

# Estimate the random effects model for comparison
random_effects_model <- plm(formula, data = Cigar, index = c("state", "year"), model = "random")

# Perform the Hausman test to compare fixed and random effects models
hausman_test <- phtest(fixed_effects_model, random_effects_model)

# Display the results of the Hausman test
hausman_test


# Generate Graphs

random_effects <- ranef(random_effects_model)

# Convert the random effects to a data frame for plotting
random_effects_df <- data.frame(State = names(random_effects), Effect = as.numeric(random_effects))

ggplot(random_effects_df, aes(x = reorder(State, Effect), y = Effect)) +
  geom_point() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random Effects by State", x = "State", y = "Random Effect")


# Plot residuals to check the distribution of residuals from the fixed effects model

residuals <- resid(fixed_effects_model)
residuals_df <- data.frame(Residuals = residuals)
ggplot(residuals_df, aes(x = Residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")