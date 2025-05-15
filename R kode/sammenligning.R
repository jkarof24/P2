# Pakker
library(dplyr)
library(car)     # for vif()
library(boot)    # for bootstrapping

# 1. Indlæs data
setwd("C:/Users/jonat/Documents/GitHub/P2/R kode")
data <- read.csv("auto-mpg.csv", na.strings = ".")

# 2. Konverter 'horsepower' til numerisk og fjern NA
data$horsepower <- as.numeric(data$horsepower)
data <- data %>% na.omit()

# 3. Vælg kun numeriske kolonner
numeric_data <- data[sapply(data, is.numeric)]

# 4. Fjern multikollinearitet vha. VIF
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

numeric_data <- remove_multicollinearity(numeric_data)

# 5. Split i trænings- og testdata
set.seed(123)
train_indices <- sample(1:nrow(numeric_data), size = 0.8 * nrow(numeric_data))
train_data <- numeric_data[train_indices, ]
test_data <- numeric_data[-train_indices, ]

# 6. Definér antal bootstrap-simuleringer
n_simulations <- 1000

# 7. OLS-model
ols_model <- lm(mpg ~ poly(acceleration, 2) + poly(cylinders, 2) + 
                  poly(model.year, 2) + poly(origin, 2), data = train_data)

ols_preds <- predict(ols_model, newdata = test_data)
actuals <- test_data$mpg
ols_errors <- ols_preds - actuals
MBE_OLS <- mean(ols_errors)
RMSE_OLS <- sqrt(mean(ols_errors^2))

# 8. Bootstrap-model
bootstrap_predictions <- matrix(NA, nrow = nrow(test_data), ncol = n_simulations)

for (i in 1:n_simulations) {
  indices <- sample(1:nrow(train_data), replace = TRUE)
  boot_model <- lm(mpg ~ poly(acceleration, 2) + poly(cylinders, 2) + 
                     poly(model.year, 2) + poly(origin, 2), 
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
  model <- lm(mpg ~ poly(acceleration, 2) + poly(cylinders, 2) + 
                poly(model.year, 2) + poly(origin, 2), 
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
