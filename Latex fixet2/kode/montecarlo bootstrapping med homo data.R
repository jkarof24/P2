library(dplyr)
library(doParallel)
library(foreach)
library(ggplot2)
library(gridExtra)
library(lmtest)  # For Breusch-Pagan test

# Set seed for reproducibility
set.seed(200)

# Generate independent variables
n <- 250
x1 <- rnorm(n, mean = 5, sd = 1)
x2 <- rnorm(n, mean = 10, sd = 4)
x3 <- rnorm(n, mean = 15, sd = 3)
x4 <- rnorm(n, mean = 20, sd = 5)


# Generate dependent variable with a polynomial relationship
y <- 3 + 0.05*x1^2 + 0.015*x2^3 + 0.03*x3^4 + 0.02*x4^5 + 10*rnorm(n, mean = 0, sd = 1000)

# Create a data frame
data <- data.frame(y, x1, x2, x3, x4)

# Fit a polynomial regression model
model <- lm(y ~ I(x1^2) + I(x2^3) + I(x3^4) + I(x4^5), data = data)


# Add an error term to x3 that scales with the corresponding y value
x3_new <- x3 + rnorm(n, mean = 0, sd = 0.0001 * abs(y))


# Create a new data frame
data_new <- data.frame(y, x1 = x1, x2, x3_new, x4)

# Fit a new polynomial regression model
model_new <- lm(y ~ 1 + I(x1^2) + I(x2^3) + I(x3_new^4) + I(x4^5), data = data_new)


print("klassisk homo")
summary(model)
print("klassisk hetro")
summary(model_new)

data <- data_new
numeric_data <- data_new






monte_carlo_bootstrap <- function(data, sample_size) {
  data %>%
    sample_n(sample_size, replace = TRUE)
}

fit_polynomial_regression <- function(data) {
  
  
  formula <- lm(y ~ I(x1^2) + I(x2^3) + I(x3_new^4) + I(x4^5), data = data)
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
n_simulations <- 10000
sample_size <- 50
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
final_model <- lm(y ~ I(x1^2) + I(x2^3) + I(x3_new^4) + I(x4^5), data = data)


final_model$coefficients <- best_coefficients


# Predict using the final model
y_final_pred <- predict(final_model, newdata = numeric_data)

# Calculate R-squared
actual_values <- numeric_data$y
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


print(best_coefficients)
print("boot hetro")
summary(final_model)

