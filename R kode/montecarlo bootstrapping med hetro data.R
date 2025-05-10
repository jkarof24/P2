library(dplyr)
library(doParallel)
library(foreach)
library(ggplot2)
library(gridExtra)
library(lmtest)  # For Breusch-Pagan test

# Set seed for reproducibility
set.seed(203)

# Generate independent variables
n <- 2500
x1 <- rnorm(n, mean = 20, sd = 1)
x2 <- rnorm(n, mean = 15, sd = 4)
x3 <- rnorm(n, mean = 10, sd = 3)
x4 <- rnorm(n, mean = 5, sd = 5)

# Generate dependent variable with a polynomial relationship
y <- 3 + 0.8*x1^2 + 0.15*x2^3 + 0.003*x3^4 + 0.002*x4^5 + rnorm(n, mean = 0, sd = 100)

# Create a data frame
data <- data.frame(y, x1, x2, x3, x4)

# Fit a polynomial regression model
model <- lm(y ~ I(x1^2) + I(x2^3) + I(x3^4) + I(x4^5), data = data)

# Add an error term to x3 that scales with the corresponding y value
x3_new <- x3 + rnorm(n, mean = 0, sd = 0.008 * abs(y))

# Create a new data frame
data_new <- data.frame(y, x1, x2, x3_new, x4)

# Fit a new polynomial regression model
model_new <- lm(y ~ I(x1^2) + I(x2^3) + I(x3_new^4) + I(x4^5), data = data_new)

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
  lm(y ~ I(x1^2) + I(x2^3) + I(x3_new^4) + I(x4^5), data = data)
}

# Run Simulations Function
run_simulations <- function(n_simulations, data_new, sample_size) {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("data_new", "fit_polynomial_regression", "monte_carlo_bootstrap", "sample_size"))
  clusterEvalQ(cl, library(dplyr))
  
  results <- foreach(i = 1:n_simulations, .combine = rbind, .packages = "dplyr") %dopar% {
    resampled_data <- monte_carlo_bootstrap(data_new, sample_size)
    model <- fit_polynomial_regression(resampled_data)
    c(coef(model), summary(model)$r.squared)
  }
  
  stopCluster(cl)
  
  results_df <- as.data.frame(results)
  colnames(results_df) <- c(names(coef(fit_polynomial_regression(data_new))), "r_squared")
  
  return(results_df)
}

set.seed(200)
n_simulations <- 10000
sample_size <- 50
results_df <- run_simulations(n_simulations, data_new, sample_size)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)


bootstrap_se <- sapply(results_df, sd)

cat("Bootstrap Standard Errors:\n")
print(bootstrap_se)

cat("\nStandard OLS Standard Errors (from model_new) for comparison:\n")
# Hent standardfejlene fra summary(model_new)
ols_summary <- summary(model_new)
ols_se <- ols_summary$coefficients[, "Std. Error"]
print(ols_se)

confidence_level <- 0.95
alpha <- 1 - confidence_level


z_critical <- qnorm(1 - alpha/2)


normal_bootstrap_ci_coef <- data.frame(
  Lower = best_coefficients[-length(best_coefficients)] - z_critical * bootstrap_se[-length(bootstrap_se)],
  Upper = best_coefficients[-length(best_coefficients)] + z_critical * bootstrap_se[-length(bootstrap_se)]
)

cat(paste0("\nNormal Bootstrap Confidence Intervals (", confidence_level * 100, "%):\n"))
print(normal_bootstrap_ci_coef)


normal_bootstrap_ci_rsq <- data.frame(
  Lower = best_coefficients["r_squared"] - z_critical * bootstrap_se["r_squared"],
  Upper = best_coefficients["r_squared"] + z_critical * bootstrap_se["r_squared"]
)
cat(paste0("\nNormal Bootstrap Confidence Interval for R-squared (", confidence_level * 100, "%):\n"))
print(normal_bootstrap_ci_rsq)




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
create_histograms(data_new, "hetro")

# Calculate the mean of the coefficients from the Monte Carlo simulation
best_coefficients <- colMeans(results_df)

print(best_coefficients)

# Fit the final model using the original data
final_model <- lm(y ~ I(x1^2) + I(x2^3) + I(x3_new^4) + I(x4^5), data = data_new)

# Create a function to apply averaged coefficients
apply_coefficients <- function(model, coefficients) {
  model$coefficients <- coefficients
  return(model)
}

# Apply the averaged coefficients to the final model
final_model <- apply_coefficients(final_model, best_coefficients)

# Predict using the final model
y_final_pred <- predict(final_model, newdata = data_new)

# Calculate R-squared
actual_values <- data_new$y
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