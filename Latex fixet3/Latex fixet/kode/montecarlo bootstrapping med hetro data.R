# Load necessary libraries
library(dplyr)
library(doParallel)
library(foreach)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(dplyr) # Used for data manipulation
library(boot) # Used for bootstrap analysis

# Set seed for reproducibility
set.seed(16)

# Generate independent variables
n <- 50
x1 <- rnorm(n, mean = 20, sd = 1)
x2 <- rnorm(n, mean = 15, sd = 4)
x3 <- rnorm(n, mean = 10, sd = 3)
x4 <- rnorm(n, mean = 5, sd = 5)

# Generate dependent variable with a polynomial relationship
y <- 3 + 0.8*x1^2 + 0.8*x2^3 + 0.0053*x3^4 + 0.02*x4^5 + rnorm(n, mean = 0, sd = 5)

# Create a data frame
data <- data.frame(y, x1, x2, x3, x4)

# Add an error term to x variables that scales with the corresponding y value
x1new <- x1 + rnorm(n, mean = 0, sd = 0.1 * abs(y))
x2new <- x2 + rnorm(n, mean = 0, sd = 0.1 * abs(y))
x3new <- x3 + rnorm(n, mean = 0, sd = 0.1 * abs(y))
x4new <- x4 + rnorm(n, mean = 0, sd = 0.1 * abs(y))

# Create a new data frame
datanew <- data.frame(y, x1new, x2new, x3new, x4new)


# Fit a new polynomial regression model
model_new <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = datanew)

print("klassisk homo")
summary(model)
print("klassisk hetro")
summary(model_new)

# Monte Carlo Bootstrap Function
monte_carlo_bootstrap <- function(data, sample_size) {
  data %>%
    sample_n(sample_size, replace = TRUE)
}

# Fit Polynomial Regression Function
fit_polynomial_regression <- function(data) {
  lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = data)
}

# Run Simulations Function
run_simulations <- function(n_simulations, datanew, sample_size) {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("datanew", "fit_polynomial_regression", "monte_carlo_bootstrap", "sample_size"))
  clusterEvalQ(cl, library(dplyr))
  
  results <- foreach(i = 1:n_simulations, .combine = rbind, .packages = "dplyr") %dopar% {
    resampled_data <- monte_carlo_bootstrap(datanew, sample_size)
    model <- fit_polynomial_regression(resampled_data)
    c(coef(model), summary(model)$r.squared)
  }
  
  stopCluster(cl)
  
  results_df <- as.data.frame(results)
  colnames(results_df) <- c(names(coef(fit_polynomial_regression(datanew))), "r_squared")
  
  return(results_df)
}

set.seed(200)
n_simulations <- 1000000
sample_size <- 50
results_df <- run_simulations(n_simulations, datanew, sample_size)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)

# Create Histograms Function
create_histograms <- function(df, xz) {
  plots <- lapply(names(df), function(col) {
    mean_value <- mean(df[[col]])
    sd_value <- sd(df[[col]])
    
    ggplot(df, aes_string(x = col)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", color = "black", alpha = 0.7) +
      geom_density(color = "red", size = 1) +
      geom_vline(aes(xintercept = mean_value), color = "green", linetype = "dashed", size = 1) +
      ggtitle(paste(col,"and", xz)) +
      theme_minimal() +
      annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_value, 6), "\nSD:", round(sd_value, 6)), 
               hjust = 1, vjust = 1, color = "black", size = 4)
  })
  
  grid.arrange(grobs = plots, ncol = 2)
}

# Generate histograms for the simulation results
create_histograms(results_df, "results")
create_histograms(data, "homo")
create_histograms(datanew, "hetro")

# Calculate the mean of the coefficients from the Monte Carlo simulation
best_coefficients <- colMeans(results_df)

print(best_coefficients)

# Fit the final model using the original data
final_model <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = datanew)

# Create a function to apply averaged coefficients
apply_coefficients <- function(model, coefficients) {
  model$coefficients <- coefficients
  return(model)
}

# Apply the averaged coefficients to the final model
final_model <- apply_coefficients(final_model, best_coefficients)

# Predict using the final model
y_final_pred <- predict(final_model, newdata = datanew)

# Calculate R-squared
actual_values <- datanew$y
ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum((actual_values - y_final_pred)^2)
r_squared <- 1 - (ss_residual / ss_total)

# Print R-squared value
cat("R-squared:", r_squared, "\n")

# Plot the final regression model
ggplot(data.frame(Actual = actual_values, Predicted = y_final_pred), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7) +
  labs(title = paste("Final Regression Model (R-squared:", round(r_squared, 3), ")"), 
       x = "Actual Values", y = "Predicted Values") +
  theme_minimal()

# Summarize models
summary(final_model)
summary(model_new)
