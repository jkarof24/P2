# Load necessary libraries
library(dplyr)
library(moments)
library(ggplot2)

# Set working directory and read data
setwd("C:/Users/jonat/Documents/GitHub/P2")
data <- read.csv("auto-mpg.csv", na.strings = ".")

# Select numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Calculate moments for all columns
moments <- numeric_data %>%
  summarise(across(everything(), list(
    mean = ~ mean(.),
    variance = ~ var(.),
    skewness = ~ skewness(.),
    kurtosis = ~ kurtosis(.)
  )))

# Function to generate data matching the target distribution
generate_data <- function(size, mean_target, variance_target, skewness_target, kurtosis_target) {
  # Generate normal data
  data <- rnorm(size)
  
  # Adjust mean and variance
  data <- (data - mean(data)) / sd(data) * sqrt(variance_target) + mean_target
  
  # Adjust skewness and kurtosis (using a simple transformation)
  data <- sign(data) * abs(data)^(1 + skewness_target / 3)
  data <- data * (1 + kurtosis_target / 10)
  
  return(data)
}

# Generate new data for each column
generated_data <- numeric_data %>%
  mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))

# Fit polynomial regression model
fit_polynomial_regression <- function(data) {
  # Use the first column as the response variable and the rest as predictors
  response_var <- names(data)[1]
  predictor_vars <- names(data)[-1]
  
  # Create the formula for polynomial regression
  formula <- as.formula(paste(response_var, "~ poly(", paste(predictor_vars, collapse = ", 2) + poly("), ", 2)"))
  
  model <- lm(formula, data = data)
  return(model)
}

# Perform Monte Carlo simulation
set.seed(123)
n_simulations <- 1000
results <- replicate(n_simulations, {
  simulated_data <- numeric_data %>%
    mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))
  model <- fit_polynomial_regression(simulated_data)
  coef(model)
})

# Analyze results
results_df <- as.data.frame(t(results))
colnames(results_df) <- names(coef(fit_polynomial_regression(numeric_data)))

# Inspect column names in results_df
print(colnames(results_df))

# Plot distribution of coefficients using the correct column names
ggplot(results_df, aes(x = `(Intercept)`)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Intercept Coefficient", x = "Intercept", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(cylinders, 2)1`)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  labs(title = "Distribution of Cylinders Coefficient (1st Degree)", x = "Cylinders Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(cylinders, 2)2`)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  labs(title = "Distribution of Cylinders Coefficient (2nd Degree)", x = "Cylinders Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(displacement, 2)1`)) +
  geom_histogram(bins = 30, fill = "purple", alpha = 0.7) +
  labs(title = "Distribution of Displacement Coefficient (1st Degree)", x = "Displacement Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(displacement, 2)2`)) +
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
  labs(title = "Distribution of Displacement Coefficient (2nd Degree)", x = "Displacement Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(weight, 2)1`)) +
  geom_histogram(bins = 30, fill = "yellow", alpha = 0.7) +
  labs(title = "Distribution of Weight Coefficient (1st Degree)", x = "Weight Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(weight, 2)2`)) +
  geom_histogram(bins = 30, fill = "pink", alpha = 0.7) +
  labs(title = "Distribution of Weight Coefficient (2nd Degree)", x = "Weight Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(acceleration, 2)1`)) +
  geom_histogram(bins = 30, fill = "cyan", alpha = 0.7) +
  labs(title = "Distribution of Acceleration Coefficient (1st Degree)", x = "Acceleration Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(acceleration, 2)2`)) +
  geom_histogram(bins = 30, fill = "magenta", alpha = 0.7) +
  labs(title = "Distribution of Acceleration Coefficient (2nd Degree)", x = "Acceleration Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(model.year, 2)1`)) +
  geom_histogram(bins = 30, fill = "brown", alpha = 0.7) +
  labs(title = "Distribution of Model Year Coefficient (1st Degree)", x = "Model Year Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(model.year, 2)2`)) +
  geom_histogram(bins = 30, fill = "gray", alpha = 0.7) +
  labs(title = "Distribution of Model Year Coefficient (2nd Degree)", x = "Model Year Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(origin, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Origin Coefficient (1st Degree)", x = "Origin Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal()

ggplot(results_df, aes(x = `poly(origin, 2)2`)) +
  geom_histogram(bins = 30, fill = "white", alpha = 0.7) +
  labs(title = "Distribution of Origin Coefficient (2nd Degree)", x = "Origin Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal()