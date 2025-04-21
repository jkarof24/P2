# Load necessary libraries
library(MASS)
library(car)
library(caret)
library(glmnet)
library(ggplot2)
library(gridExtra)

# Load the Auto MPG dataset
auto_mpg <- read.csv("auto-mpg.csv", na.strings = ".")

# Clean the data
auto_mpg <- na.omit(auto_mpg)

# Replace missing values in 'horsepower' with the mean
auto_mpg$horsepower <- as.numeric(auto_mpg$horsepower)
mean_horsepower <- mean(auto_mpg$horsepower, na.rm = TRUE)
auto_mpg$horsepower[is.na(auto_mpg$horsepower)] <- mean_horsepower

# Select numeric columns except 'car.name'
numeric_data <- auto_mpg[, sapply(auto_mpg, is.numeric)]

# Print column names to verify
print(names(numeric_data))

# Calculate means for the specified columns
means <- colMeans(numeric_data[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model.year", "origin")])
print(means)

# Calculate standard deviations for the specified columns
sds <- apply(numeric_data[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "model.year", "origin")], 2, sd)
print(sds)

# Set seed for reproducibility
set.seed(123)
n <- nrow(numeric_data)

# Generate normally distributed data
df_normal <- data.frame(
  mpg = rnorm(n, means["mpg"], sds["mpg"]),
  cylinders = rnorm(n, means["cylinders"], sds["cylinders"]),
  displacement = rnorm(n, means["displacement"], sds["displacement"]),
  horsepower = rnorm(n, means["horsepower"], sds["horsepower"]),
  weight = rnorm(n, means["weight"], sds["weight"]),
  acceleration = rnorm(n, means["acceleration"], sds["acceleration"]),
  model.year = rnorm(n, means["model.year"], sds["model.year"]),
  origin = rnorm(n, means["origin"], sds["origin"])
)

df_normal$mpg <- 30 - 0.5 * df_normal$cylinders + 0.3 * df_normal$displacement - 0.02 * df_normal$horsepower + 0.01 * df_normal$weight - 0.1 * df_normal$acceleration + rnorm(n, 0, 2)

# Print the first few rows of the generated data
print(head(df_normal))

# Generate non-homoscedastic data
df_non_homoscedastic <- data.frame(
  mpg = rnorm(n, means["mpg"], sds["mpg"]),
  cylinders = rnorm(n, means["cylinders"], sds["cylinders"]),
  displacement = rnorm(n, means["displacement"], sds["displacement"]),
  horsepower = rnorm(n, means["horsepower"], sds["horsepower"]),
  weight = rnorm(n, means["weight"], sds["weight"]),
  acceleration = rnorm(n, means["acceleration"], sds["acceleration"]),
  model.year = rnorm(n, means["model.year"], sds["model.year"]),
  origin = rnorm(n, means["origin"], sds["origin"])
)

# Introduce non-homoscedasticity using a combination of all predictors
error_term <- rnorm(n, 0, sqrt(df_non_homoscedastic$cylinders^2 + df_non_homoscedastic$displacement^2 + df_non_homoscedastic$horsepower^2 + df_non_homoscedastic$weight^2 + df_non_homoscedastic$acceleration^2 + df_non_homoscedastic$model.year^2 + df_non_homoscedastic$origin^2))
df_non_homoscedastic$mpg <- 30 - 0.5 * df_non_homoscedastic$cylinders + 0.3 * df_non_homoscedastic$displacement - 0.02 * df_non_homoscedastic$horsepower + 0.01 * df_non_homoscedastic$weight - 0.1 * df_non_homoscedastic$acceleration + error_term

# Print the first few rows of the generated non-homoscedastic data
print(head(df_non_homoscedastic))

# Perform polynomial regression on normally distributed data
model_normal <- lm(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_normal)
summary(model_normal)

# Perform polynomial regression on non-homoscedastic data
model_non_homoscedastic <- lm(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_non_homoscedastic)
summary(model_non_homoscedastic)

# Calculate R-squared values
r_squared_normal <- summary(model_normal)$r.squared
r_squared_non_homoscedastic <- summary(model_non_homoscedastic)$r.squared

# Print R-squared values
cat("R-squared for Normal Data: ", r_squared_normal, "\n")
cat("R-squared for Non-Homoscedastic Data: ", r_squared_non_homoscedastic, "\n")

# Analyze residuals and diagnostics
par(mfrow = c(2, 2))
plot(model_normal, main = "Normal Data - Residuals and Diagnostics")
plot(model_non_homoscedastic, main = "Non-Homoscedastic Data - Residuals and Diagnostics")

# Check for multicollinearity
vif(model_normal)
vif(model_non_homoscedastic)

# Perform cross-validation
train_control <- trainControl(method = "cv", number = 10, allowParallel = FALSE)
model_normal_cv <- train(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_normal, method = "lm", trControl = train_control)
model_non_homoscedastic_cv <- train(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_non_homoscedastic, method = "lm", trControl = train_control)

# Print cross-validation results
print(model_normal_cv)
print(model_non_homoscedastic_cv)

# Create scatter plots for each numeric column against 'mpg' for normal data
scatter_plots_normal <- lapply(names(df_normal)[-length(names(df_normal))], function(col) {
  ggplot(df_normal, aes_string(x = col, y = "mpg")) +
    geom_point(color = "blue") +
    ggtitle(paste("Normal Data: Scatter Plot of", col, "vs mpg")) +
    theme_minimal()
})

# Create scatter plots for each numeric column against 'mpg' for non-homoscedastic data
scatter_plots_non_homoscedastic <- lapply(names(df_non_homoscedastic)[-length(names(df_non_homoscedastic))], function(col) {
  ggplot(df_non_homoscedastic, aes_string(x = col, y = "mpg")) +
    geom_point(color = "red") +
    ggtitle(paste("Non-Homoscedastic Data: Scatter Plot of", col, "vs mpg")) +
    theme_minimal()
})

# Arrange scatter plots in grids
grid.arrange(grobs = scatter_plots_normal, ncol = 2, top = "Scatter Plots for Normal Data")
grid.arrange(grobs = scatter_plots_non_homoscedastic, ncol = 2, top = "Scatter Plots for Non-Homoscedastic Data")
