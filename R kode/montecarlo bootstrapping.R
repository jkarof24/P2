library(dplyr)
library(ggplot2)
library(foreach)
library(doParallel)

# Set working directory and read data
setwd("C:/Users/Jonathan/Documents/GitHub/P2/R kode")
data <- read.csv("auto-mpg.csv", na.strings = ".")

# Convert horsepower to numeric, coercing non-numeric values to NA
data$horsepower <- as.numeric(data$horsepower)

# Remove rows with any NA values
data <- cleaned_data %>% na.omit()

# Select numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Function to perform Monte Carlo bootstrapping
monte_carlo_bootstrap <- function(data, sample_size) {
  data %>%
    sample_n(sample_size, replace = TRUE)
}

fit_polynomial_regression <- function(data) {
  formula <- as.formula(paste("mpg ~", paste(sapply(setdiff(names(data), "mpg"), function(var) paste("poly(", var, ", 2)")), collapse = " + ")))
  lm(formula, data = data)
}

run_simulations <- function(n_simulations, numeric_data, sample_size) {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("numeric_data", "fit_polynomial_regression", "monte_carlo_bootstrap", "sample_size"))
  clusterEvalQ(cl, library(dplyr))
  
  results <- foreach(i = 1:n_simulations, .combine = rbind, .packages = "dplyr") %dopar% {
    resampled_data <- monte_carlo_bootstrap(numeric_data, sample_size)
    model <- fit_polynomial_regression(resampled_data)
    c(coef(model), summary(model)$r.squared)
  }
  
  stopCluster(cl)
  
  results_df <- as.data.frame(results)
  colnames(results_df) <- c(names(coef(fit_polynomial_regression(numeric_data))), "r_squared")
  
  return(results_df)
}

set.seed(200)
n_simulations <- 100
sample_size <- 300
results_df <- run_simulations(n_simulations, numeric_data, sample_size)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)

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

print(best_coefficients)

# Fit the final regression model using the best coefficients
final_model <- lm(mpg ~ ., data = numeric_data)

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