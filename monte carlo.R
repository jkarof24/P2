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
# Calculate the mean and SD for each coefficient
mean_values <- colMeans(results_df)
sd_values <- apply(results_df, 2, sd)

# Plot distribution of coefficients using the correct column names with annotations
ggplot(results_df, aes(x = `(Intercept)`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Intercept Coefficient", x = "Intercept", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[1], 2), "\nSD:", round(sd_values[1], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(cylinders, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Cylinders Coefficient (1st Degree)", x = "Cylinders Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[2], 2), "\nSD:", round(sd_values[2], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(cylinders, 2)2`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Cylinders Coefficient (2nd Degree)", x = "Cylinders Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[3], 2), "\nSD:", round(sd_values[3], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(displacement, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Displacement Coefficient (1st Degree)", x = "Displacement Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[4], 2), "\nSD:", round(sd_values[4], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(displacement, 2)2`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Displacement Coefficient (2nd Degree)", x = "Displacement Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[5], 2), "\nSD:", round(sd_values[5], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(weight, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Weight Coefficient (1st Degree)", x = "Weight Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[6], 2), "\nSD:", round(sd_values[6], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(weight, 2)2`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Weight Coefficient (2nd Degree)", x = "Weight Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[7], 2), "\nSD:", round(sd_values[7], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(acceleration, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Acceleration Coefficient (1st Degree)", x = "Acceleration Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[8], 2), "\nSD:", round(sd_values[8], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(acceleration, 2)2`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Acceleration Coefficient (2nd Degree)", x = "Acceleration Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[9], 2), "\nSD:", round(sd_values[9], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(model.year, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Model Year Coefficient (1st Degree)", x = "Model Year Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[10], 2), "\nSD:", round(sd_values[10], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(model.year, 2)2`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Model Year Coefficient (2nd Degree)", x = "Model Year Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[11], 2), "\nSD:", round(sd_values[11], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(origin, 2)1`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Origin Coefficient (1st Degree)", x = "Origin Coefficient (1st Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[12], 2), "\nSD:", round(sd_values[12], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

ggplot(results_df, aes(x = `poly(origin, 2)2`)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of Origin Coefficient (2nd Degree)", x = "Origin Coefficient (2nd Degree)", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_values[13], 2), "\nSD:", round(sd_values[13], 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

# Calculate the mean of the coefficients from the Monte Carlo simulation
best_coefficients <- colMeans(results_df)

print(best_coefficients)

# Fit the final regression model using the best coefficients
final_model <- fit_polynomial_regression(numeric_data)

# Predict using the final model
X_final_poly <- model.matrix(final_model)
y_final_pred <- X_final_poly %*% best_coefficients

# Plot the final regression model
ggplot(data.frame(Actual = numeric_data[[1]], Predicted = y_final_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  labs(title = "Final Regression Model", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()