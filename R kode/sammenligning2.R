
library(dplyr)
library(car)     
library(boot)


library(dplyr)
library(doParallel)
library(foreach)
library(ggplot2)
library(gridExtra)
library(lmtest)  # For Breusch-Pagan test

# Set seed for reproducibility
set.seed(176)

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
x1_new <- x1 + rnorm(n, mean = 0, sd = 1 * abs(y))
x2_new <- x2 + rnorm(n, mean = 0, sd = 1 * abs(y))
x3_new <- x3 + rnorm(n, mean = 0, sd = 1 * abs(y))
x4_new <- x4 + rnorm(n, mean = 0, sd = 1 * abs(y))

# Create a new data frame
data_new <- data.frame(y, x1_new, x2_new, x3_new, x4_new)

# Fit a new polynomial regression model
model_new <- lm(y ~ I(x1_new^2) + I(x2_new^3) + I(x3_new^4) + I(x4_new^5), data = data_new)



data_new <- data_new

# 5. Split i trænings- og testdata
set.seed(123)
train_indices <- sample(1:nrow(data_new), size = 0.8 * nrow(data_new))
train_data <- data_new[train_indices, ]
test_data <- data_new[-train_indices, ]

# 6. Definér antal bootstrap-simuleringer
n_simulations <- 1000

# 7. OLS-model
ols_model <- lm(y ~ I(x1_new^2) + I(x2_new^3) + I(x3_new^4) + I(x4_new^5), data = train_data)

ols_preds <- predict(ols_model, newdata = test_data)
actuals <- test_data$y
ols_errors <- ols_preds - actuals
MBE_OLS <- mean(ols_errors)
RMSE_OLS <- sqrt(mean(ols_errors^2))

# 8. Bootstrap-model
bootstrap_predictions <- matrix(NA, nrow = nrow(test_data), ncol = n_simulations)

for (i in 1:n_simulations) {
  indices <- sample(1:nrow(train_data), replace = TRUE)
  boot_model <- lm(y ~ I(x1_new^2) + I(x2_new^3) + I(x3_new^4) + I(x4_new^5), 
                   data = train_data[indices, ])
  bootstrap_predictions[, i] <- predict(boot_model, newdata = test_data)
}

bootstrap_mean_preds <- rowMeans(bootstrap_predictions)
bootstrap_errors <- bootstrap_mean_preds - actuals
MBE_Bootstrap <- mean(bootstrap_errors)
RMSE_Bootstrap <- sqrt(mean(bootstrap_errors^2))
Bias2_Bootstrap <- mean((bootstrap_mean_preds - actuals)^2)
Variance_Bootstrap <- mean(apply(bootstrap_predictions, 1, var))

# 9. Bias² og Varians for OLS via bootstrap
ols_boot_preds <- matrix(NA, nrow = nrow(test_data), ncol = n_simulations)

for (i in 1:n_simulations) {
  indices <- sample(1:nrow(train_data), replace = TRUE)
  model <- lm(y ~ I(x1_new^2) + I(x2_new^3) + I(x3_new^4) + I(x4_new^5), 
              data = train_data[indices, ])
  ols_boot_preds[, i] <- predict(model, newdata = test_data)
}

ols_mean_preds <- rowMeans(ols_boot_preds)
Bias2_OLS <- mean((ols_mean_preds - actuals)^2)
Variance_OLS <- mean(apply(ols_boot_preds, 1, var))

# 10. Udskriv resultater
cat("\n--- OLS Model ---\n")
cat("MBE_OLS: ", round(MBE_OLS, 4), "\n")
cat("RMSE_OLS: ", round(RMSE_OLS, 4), "\n")
cat("Bias²_OLS: ", round(Bias2_OLS, 4), "\n")
cat("Variance_OLS: ", round(Variance_OLS, 4), "\n")

cat("\n--- Bootstrap Model ---\n")
cat("MBE_Bootstrap: ", round(MBE_Bootstrap, 4), "\n")
cat("RMSE_Bootstrap: ", round(RMSE_Bootstrap, 4), "\n")
cat("Bias²_Bootstrap: ", round(Bias2_Bootstrap, 4), "\n")
cat("Variance_Bootstrap: ", round(Variance_Bootstrap, 4), "\n")
