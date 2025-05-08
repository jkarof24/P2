library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)
library(gridExtra)
library(boot)
library(car)

# Set working directory and read data
setwd("C:/Users/Jonathan/Documents/GitHub/P2/R kode")
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
  predictors <- setdiff(names(data), "mpg")
  formula <- reformulate(termlabels = paste0("poly(", predictors, ", 2)"), response = "mpg")
  model <- lm(formula, data = resampled_data)
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
set.seed(210)
n_simulations <- 1000
sample_size <- 300
results_df <- run_simulations(n_simulations, numeric_data)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)

# Create histograms
create_histograms <- function(df) {
  plots <- lapply(names(df), function(col) {
    mean_value <- mean(df[[col]])
    sd_value <- sd(df[[col]])
    
    ggplot(df, aes_string(x = col)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      geom_vline(aes(xintercept = mean_value), color = "green", linetype = "dashed", size = 1) +
      ggtitle(paste("Density, Mean and SD of", col)) +
      theme_minimal() +
      annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_value, 2), "\nSD:", round(sd_value, 2)), 
               hjust = 1.1, vjust = 2, color = "black", size = 4)
  })
  
  grid.arrange(grobs = plots, ncol = 2)
}

# Generate histograms for the simulation results
create_histograms(results_df)

# Calculate the mean of the coefficients from the Monte Carlo simulation
best_coefficients <- colMeans(results_df)
best_coefficients <- setNames(best_coefficients, names(coef(fit_polynomial_regression(numeric_data, 1:nrow(numeric_data)))))

# Fit the final model using the original data
final_model <- fit_polynomial_regression(numeric_data, 1:nrow(numeric_data))

# Apply the averaged coefficients to the final model
final_model <- apply_coefficients(final_model, best_coefficients)

# Predict using the final model
y_final_pred <- predict(final_model, newdata = numeric_data)

# Calculate R-squared
actual_values <- numeric_data$mpg
ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - y_final_pred)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print R-squared value
cat("R-squared:", r_squared, "\n")

# Plot the final regression model
ggplot(data.frame(Actual = actual_values, Predicted = y_final_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  labs(title = paste("Final Regression Model (R-squared:", round(r_squared, 2), ")"), 
       x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Summarize models
klassisk_model <- fit_polynomial_regression(numeric_data, 1:nrow(numeric_data))
summary(final_model)
summary(klassisk_model)
