# Load necessary libraries

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



# Add an error term to x variables that scales with the corresponding y value

x1new <- x1 + rnorm(n, mean = 0, sd = 0.2 * abs(y))

x2new <- x2 + rnorm(n, mean = 0, sd = 0.2 * abs(y))

x3new <- x3 + rnorm(n, mean = 0, sd = 0.2 * abs(y))

x4new <- x4 + rnorm(n, mean = 0, sd = 0.2 * abs(y))



# Create a data frame

data <- data.frame(y, x1new, x2new, x3new, x4new)



# 5. Split into training and test data

set.seed(123)

trainindices <- sample(1:nrow(data), size = 0.8 * nrow(data))

traindata <- data[trainindices, ]

testdata <- data[-trainindices, ]



# 6. OLS model

olsmodel <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = traindata)



# Get summary for OLS model to extract coefficient SE and CI

ols_summary <- summary(olsmodel)

ols_coefs <- ols_summary$coefficients



# Calculate Confidence Intervals for OLS coefficients

ols_confint <- confint(olsmodel)



# --- OLS R-squared ---

r_squared <- summary(olsmodel)$r.square



# OLS Prediction Errors

ols_preds_info <- predict(olsmodel, newdata = testdata, se.fit = TRUE, interval = "confidence")

olspreds <- ols_preds_info$fit[, "fit"]

olserrors <- olspreds - testdata$y

MBEOLS <- mean(olserrors)

RMSEOLS <- sqrt(mean(olserrors^2))





# 7. Define number of bootstrap simulations

n_simulations <- 10000



# 8. Bootstrap model for coefficients



bootstrap_r_squared <- numeric(n_simulations)

bootstrappredictions <- matrix(NA, nrow = nrow(testdata), ncol = n_simulations)

bootstrap_coefs <- matrix(NA, nrow = 5, ncol = n_simulations)



for (i in 1:n_simulations) {
  
  indices <- sample(1:nrow(traindata), replace = TRUE)
  
  bootdata <- traindata[indices, ]
  
  
  
  bootmodel <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = bootdata)
  
  
  
  # Store R-squared
  
  bootstrap_r_squared[i] <- summary(bootmodel)$r.squared
  
  
  
  # Store predictions
  
  bootstrappredictions[, i] <- predict(bootmodel, newdata = testdata)
  
  
  
  # Store coefficients
  
  bootstrap_coefs[, i] <- coef(bootmodel)
  
}

boot_results <- boot(data = traindata, statistic = coef_function, R = n_simulations)

bootstrap_coef_se <- apply(boot_results$t, 2, sd)

bootstrap_coef_ci <- t(apply(boot_results$t, 2, quantile, probs = c(0.025, 0.975)))

bootstrapmeanpreds <- rowMeans(bootstrappredictions)

bootstraperrors <- bootstrapmeanpreds - testdata$y

MBEBootstrap <- mean(bootstraperrors)

RMSEBootstrap <- sqrt(mean(bootstraperrors^2))

SEBootstrap_pred <- mean(apply(bootstrappredictions, 1, sd))

CI_Bootstrap_pred <- t(apply(bootstrappredictions, 1, quantile, probs = c(0.025, 0.975)))

mean_r_squared_boot <- mean(bootstrap_r_squared)



# --- Output ---

cat("\n--- OLS Model (Prediction Errors) ---\n")

cat("MBEOLS: ", round(MBEOLS, 4), "\n")

cat("RMSEOLS: ", round(RMSEOLS, 4), "\n")



cat("\n--- OLS Model (Coefficient Estimates) ---\n")

print(ols_coefs)

cat("\n--- OLS Model (95% Confidence Intervals for Coefficients) ---\n")

print(ols_confint)



cat("\n--- OLS Model (R-squared) ---\n")

cat("R-squared: ", round(r_squared, 4), "\n")



cat("\n--- Bootstrap Model (Prediction Errors) ---\n")

cat("MBEBootstrap: ", round(MBEBootstrap, 4), "\n")

cat("RMSEBootstrap: ", round(RMSEBootstrap, 4), "\n")



cat("\n--- Bootstrap Model (95% Confidence Intervals for Coefficients) ---\n")

print(bootstrap_coef_ci)

cat("\n--- Bootstrap Model (Standard Errors for Coefficients) ---\n")

print(bootstrap_coef_se)



cat("\n--- Bootstrap Model (R-squared) ---\n")

cat("Mean R-squared: ", round(mean_r_squared_boot, 4), "\n")



# Histograms

hist(bootstraperrors)
abline(v = mean(bootstraperrors), col = "red")



hist(olserrors)

abline(v = mean(olserrors), col = "red")



hist(bootstrappredictions[1,])

abline(v = mean(bootstrappredictions[1,]), col = "red")

