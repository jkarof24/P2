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

# Create a data frame
data <- data.frame(y, x1, x2, x3, x4)

# Add an error term to x variables that scales with the corresponding y value
x1new <- x1 + rnorm(n, mean = 0, sd = 0.1 * abs(y))
x2new <- x2 + rnorm(n, mean = 0, sd = 0.1 * abs(y))
x3new <- x3 + rnorm(n, mean = 0, sd = 0.1 * abs(y))
x4new <- x4 + rnorm(n, mean = 0, sd = 0.1 * abs(y))

# Create a new data frame
datanew <- data.frame(y, x1new, x2new, x3new, x4new)

# 5. Split i trænings- og testdata
set.seed(123)
trainindices <- sample(1:nrow(datanew), size = 0.8 * nrow(datanew))
traindata <- datanew[trainindices, ]
testdata <- datanew[-trainindices, ]

# 6. Definér antal bootstrap-simuleringer
n_simulations <- 10000

# 7. OLS-model
olsmodel <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = traindata)

# Get summary for OLS model to extract coefficient SE and CI
ols_summary <- summary(olsmodel)
ols_coefs <- ols_summary$coefficients

# Calculate Confidence Intervals for OLS coefficients
ols_confint <- confint(olsmodel)

# 8. Bootstrap-model for coefficients
# Function to extract coefficients from a model fit
coef_function <- function(data, indices) {
  d <- data[indices,] # allows bootstrap to work on the data
  fit <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5), data = d)
  return(coef(fit))
}

# Perform bootstrap
boot_results <- boot(data = traindata, statistic = coef_function, R = n_simulations)

# Get bootstrap standard errors for coefficients
bootstrap_coef_se <- apply(boot_results$t, 2, sd)

# Get bootstrap confidence intervals for coefficients (using percentile method)
bootstrap_coef_ci <- t(apply(boot_results$t, 2, quantile, probs = c(0.025, 0.975)))

# OLS Prediction Errors
ols_preds_info <- predict(olsmodel, newdata = testdata, se.fit = TRUE, interval = "confidence")
olspreds <- ols_preds_info$fit[, "fit"]
olserrors <- olspreds - testdata$y
MBEOLS <- mean(olserrors)
RMSEOLS <- sqrt(mean(olserrors^2))
SEOLS_pred <- mean(ols_preds_info$se.fit) # Renamed to avoid confusion
CI_OLS_pred <- ols_preds_info$fit[, "upr"] - ols_preds_info$fit[, "lwr"]
AvgCIWidthOLS_pred <- mean(CI_OLS_pred) # Renamed to avoid confusion

# Bootstrap Prediction Errors
# (Prediction calculation remains the same as it uses the mean of bootstrap predictions)
bootstrappredictions <- matrix(NA, nrow = nrow(testdata), ncol = n_simulations)
for (i in 1:n_simulations) {
  indices <- sample(1:nrow(traindata), replace = TRUE)
  bootmodel <- lm(y ~ I(x1new^2) + I(x2new^3) + I(x3new^4) + I(x4new^5),
                  data = traindata[indices, ])
  bootstrappredictions[, i] <- predict(bootmodel, newdata = testdata)
}
bootstrapmeanpreds <- rowMeans(bootstrappredictions)
bootstraperrors <- bootstrapmeanpreds - testdata$y
MBEBootstrap <- mean(bootstraperrors)
RMSEBootstrap <- sqrt(mean(bootstraperrors^2))
SEBootstrap_pred <- mean(apply(bootstrappredictions, 1, sd)) # Renamed to avoid confusion
CI_Bootstrap_pred <- t(apply(bootstrappredictions, 1, quantile, probs = c(0.025, 0.975)))
AvgCIWidthBootstrap_pred <- mean(CI_Bootstrap_pred[, 2] - CI_Bootstrap_pred[, 1]) # Renamed to avoid confusion



# --- Output Results ---




# (Keeping the prediction error metrics for comparison as well)


cat("\n--- OLS Model (Prediction Errors) ---\n")
cat("MBEOLS: ", round(MBEOLS, 4), "\n")
cat("RMSEOLS: ", round(RMSEOLS, 4), "\n")

cat("\n--- OLS Model (Coefficient Estimates) ---\n")
print(ols_coefs)
cat("\n--- OLS Model (95% Confidence Intervals for Coefficients) ---\n")
print(ols_confint)


cat("\n--- Bootstrap Model (Prediction Errors) ---\n")
cat("MBEBootstrap: ", round(MBEBootstrap, 4), "\n")
cat("RMSEBootstrap: ", round(RMSEBootstrap, 4), "\n")

cat("\n--- Bootstrap Model (95% Confidence Intervals for Coefficients) ---\n")
# Print the calculated bootstrap confidence intervals
print(bootstrap_coef_ci)
cat("\n--- Bootstrap Model (Standard Errors for Coefficients) ---\n")
# Print the calculated bootstrap standard errors
print(bootstrap_coef_se)

