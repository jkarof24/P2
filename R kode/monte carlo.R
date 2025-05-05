library(dplyr)
library(moments)
library(ggplot2)
library(PearsonDS)  
library(gridExtra)
library(reshape2)
library(tidyr)
library(caret)
library(fastDummies)

#install.packages("dplyr")
#install.packages("moments")
#install.packages("ggplot2")
#install.packages("PearsonDS")
#install.packages("gridExtra")
#install.packages("reshape2")
#install.packages("tidyr")
#install.packages("caret")
#install.packages("fastDummies")

#install.packages(c("dplyr", "moments", "ggplot2", "PearsonDS", "gridExtra", "reshape2", "tidyr", "caret", "fastDummies"))

# Set working directory and read data
setwd("C:/Users/Jonathan/Documents/GitHub/P2")
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

generate_data <- function(size, mean_target, variance_target, skewness_target, kurtosis_target) {
  # Generate normal data
  data <- rnorm(size)
  
  # Adjust mean and variance
  data <- (data - mean(data)) / sd(data) * variance_target + mean_target
  
  # Adjust skewness and kurtosis using the Pearson system
  data <- rpearson(size, moments = c(mean_target, variance_target, skewness_target, kurtosis_target))
  
  return(data)
}

# Generate new data for each column
generated_data <- numeric_data %>%
  mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))

fit_polynomial_regression <- function(data) {
  response_var <- names(data)[1]
  predictor_vars <- names(data)[-1]
  
  # Corrected formula generation
  formula <- as.formula(paste(response_var, "~", paste(sapply(predictor_vars, function(var) paste("poly(", var, ", 2)")), collapse = " + ")))
  
  model <- lm(formula, data = data)
  return(model)
}

set.seed(421)
n_simulations <- 100
results <- replicate(n_simulations, {
  simulated_data <- numeric_data %>%
    mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))
  model <- fit_polynomial_regression(simulated_data)
  coefficients <- coef(model)
  r_squared <- summary(model)$r.squared
  c(coefficients, r_squared)
})

# Convert results to a data frame
results_df <- as.data.frame(t(results))
colnames(results_df) <- c(names(coef(fit_polynomial_regression(numeric_data))), "r_squared")

# Analyze results
mean_values <- colMeans(results_df)
sd_values <- apply(results_df, 2, sd)

# Visualize the distribution of R-squared values
ggplot(results_df, aes(x = r_squared)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of R-squared Values", x = "R-squared", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean(results_df$r_squared), 2), "\nSD:", round(sd(results_df$r_squared), 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)

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

# Calculate R-squared
actual_values <- numeric_data[[1]]
ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - y_final_pred)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print R-squared value
cat("R-squared:", r_squared, "\n")

# Plot the final regression model
ggplot(data.frame(Actual = actual_values, Predicted = y_final_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  labs(title = paste("Final Regression Model (R-squared:", round(r_squared, 20), ")"), 
       x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

plots <- lapply(names(generated_data), function(col) {
  n_bins <- ceiling(log2(length(generated_data[[col]])) + 1)
  
  ggplot(generated_data, aes_string(x = col)) +
    geom_histogram(bins = 100, fill = "blue", color = "black") +
    ggtitle(paste("Histogram of generated", col)) +
    theme_minimal()
})

# Arrange all plots in a grid
do.call(grid.arrange, c(plots, ncol = 3))


# Create a list of polynomial terms for numerical columns
poly_terms <- paste("poly(", c("cylinders", "displacement", "weight", "acceleration", "model.year", "origin"), ", 2)", collapse = " + ")

# Use reformulate to create the formula without car.name dummy variables
formula <- reformulate(poly_terms, response = "mpg")

# Fit the polynomial regression model
model <- lm(formula, data = generated_data)

# Display the summary of the model
summary(model)

summary(generated_data)

summary(numeric_data)

ggplot(results_df, aes(x = r_squared)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.7) +
  labs(title = "Distribution of R-squared Values", x = "R-squared", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean(results_df$r_squared), 2), "\nSD:", round(sd(results_df$r_squared), 2)), 
           hjust = 1.1, vjust = 2, color = "black", size = 4)
