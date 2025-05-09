# Load necessary libraries
library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(gridExtra)
library(boot)
library(car)

# Set working directory and read data
setwd("C:/Users/jonat/Documents/GitHub/P2/R kode")
data <- read.csv("auto-mpg.csv", na.strings = ".")

# Convert horsepower to numeric, coercing non-numeric values to NA
data$horsepower <- as.numeric(data$horsepower)

# Remove rows with any NA values
data <- data %>% na.omit()

# Select numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Function to calculate VIF and remove predictors with high VIF
remove_multicollinearity <- function(data, threshold = 5) {
  predictors <- setdiff(names(data), "mpg")
  formula <- as.formula(paste("mpg ~", paste(predictors, collapse = " + ")))
  model <- lm(formula, data = data)
  vif_values <- vif(model)
  
  while (any(vif_values > threshold)) {
    high_vif <- names(vif_values)[which.max(vif_values)]
    predictors <- setdiff(predictors, high_vif)
    formula <- as.formula(paste("mpg ~", paste(predictors, collapse = " + ")))
    model <- lm(formula, data = data)
    vif_values <- vif(model)
  }
  
  data <- data %>% select(all_of(c("mpg", predictors)))
  return(data)
}

# Remove multicollinearity
numeric_data <- remove_multicollinearity(numeric_data)

# Function to perform Monte Carlo bootstrapping
monte_carlo_bootstrap <- function(data, sample_size) {
  data %>%
    sample_n(sample_size, replace = TRUE)
}

# Fit polynomial regression function
fit_polynomial_regression <- function(data, indices) {
  resampled_data <- data[indices, ]
  model <- lm(mpg ~ poly(acceleration, 2) + poly(cylinders, 2) + poly(model.year, 2) + poly(origin, 2), data = resampled_data)
  return(model)
}

# Apply coefficients function
apply_coefficients <- function(model, coefficients) {
  model$coefficients <- coefficients[1:length(model$coefficients)]
  return(model)
}

# Run simulations using boot
run_simulations <- function(n_simulations, numeric_data) {
  boot_results <- boot(data = numeric_data, statistic = function(data, indices) {
    model <- fit_polynomial_regression(data, indices)
    c(coef(model), summary(model)$r.squared)
  }, R = n_simulations)
  
  results_df <- as.data.frame(boot_results$t)
  colnames(results_df) <- c(names(coef(fit_polynomial_regression(numeric_data, 1:nrow(numeric_data)))), "r_squared")
  
  return(results_df)
}

# Set seed and run simulations
set.seed(211)
n_simulations <- 10000
sample_size <- 300
results_df <- run_simulations(n_simulations, numeric_data)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)

# Calculate the median of the coefficients from the Monte Carlo simulation
best_coefficients <- apply(results_df, 8, median)

# Fit the final model using the original data
final_model <- fit_polynomial_regression(numeric_data, 1:nrow(numeric_data))

# Apply the median coefficients to the final model
final_model <- apply_coefficients(final_model, best_coefficients)


y_final_pred <- predict(final_model, newdata = numeric_data)

# Summarize models
klassisk_model <- lm(mpg ~ poly(acceleration, 2) + poly(cylinders, 2) + poly(model.year, 2) + poly(origin, 2), data = data)
summary(final_model)
summary(klassisk_model)
