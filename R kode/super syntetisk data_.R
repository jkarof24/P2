# Load necessary libraries
library(MASS)
library(car)
library(caret)
library(glmnet)
library(ggplot2)
library(gridExtra)
library(lmtest)

# Load and clean the Auto MPG dataset
auto_mpg <- read.csv("auto-mpg.csv", na.strings = ".")
auto_mpg <- na.omit(auto_mpg)
auto_mpg$horsepower <- as.numeric(auto_mpg$horsepower)
auto_mpg$horsepower[is.na(auto_mpg$horsepower)] <- mean(auto_mpg$horsepower, na.rm = TRUE)

# Select numeric columns except 'car.name'
numeric_data <- auto_mpg[, sapply(auto_mpg, is.numeric)]

# Calculate means and standard deviations
means <- colMeans(numeric_data)
sds <- apply(numeric_data, 2, sd)

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

# Generate non-homoscedastic data by scaling standard deviations with individual MPG values
df_non_homoscedastic <- data.frame(
  mpg = df_normal$mpg,
  cylinders = rnorm(n, means["cylinders"], sds["cylinders"] * df_normal$mpg),
  displacement = rnorm(n, means["displacement"], sds["displacement"] * df_normal$mpg),
  horsepower = rnorm(n, means["horsepower"], sds["horsepower"] * df_normal$mpg),
  weight = rnorm(n, means["weight"], sds["weight"] * df_normal$mpg),
  acceleration = rnorm(n, means["acceleration"], sds["acceleration"] * df_normal$mpg),
  model.year = rnorm(n, means["model.year"], sds["model.year"] * df_normal$mpg),
  origin = rnorm(n, means["origin"], sds["origin"] * df_normal$mpg)
)

# Perform polynomial regression and calculate R-squared values
model_normal <- lm(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_normal)
model_non_homoscedastic <- lm(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_non_homoscedastic)
cat("R-squared for Normal Data: ", summary(model_normal)$r.squared, "\n")
cat("R-squared for Non-Homoscedastic Data: ", summary(model_non_homoscedastic)$r.squared, "\n")

# Analyze residuals and diagnostics
par(mfrow = c(2, 2))
plot(model_normal, which = 1:2, main = "Normal Data - Residuals and Diagnostics")
plot(model_non_homoscedastic, which = 1:2, main = "Non-Homoscedastic Data - Residuals and Diagnostics")

# Check for multicollinearity
vif(model_normal)
vif(model_non_homoscedastic)

# Perform cross-validation
train_control <- trainControl(method = "cv", number = 10, allowParallel = FALSE)
model_normal_cv <- train(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_normal, method = "lm", trControl = train_control)
model_non_homoscedastic_cv <- train(mpg ~ poly(cylinders, 2) + poly(displacement, 2) + poly(horsepower, 2) + poly(weight, 2) + poly(acceleration, 2) + poly(model.year, 2) + poly(origin, 2), data = df_non_homoscedastic, method = "lm", trControl = train_control)
print(model_normal_cv)
print(model_non_homoscedastic_cv)

# Create scatter plots for each numeric column against 'mpg'
scatter_plots <- function(data, color, title_prefix) {
  lapply(names(data)[-length(names(data))], function(col) {
    ggplot(data, aes_string(x = col, y = "mpg")) +
      geom_point(color = color) +
      ggtitle(paste(title_prefix, col, "vs mpg")) +
      theme_minimal()
  })
}
grid.arrange(grobs = scatter_plots(df_normal, "blue", "Normal Data: Scatter Plot of"), ncol = 2, top = "Scatter Plots for Normal Data")
grid.arrange(grobs = scatter_plots(df_non_homoscedastic, "red", "Non-Homoscedastic Data: Scatter Plot of"), ncol = 2, top = "Scatter Plots for Non-Homoscedastic Data")

# Create histograms for each numeric column
create_histograms <- function(data, color, title_prefix) {
  lapply(names(data), function(col) {
    n_bins <- ceiling(log2(length(data[[col]])) + 1)
    ggplot(data, aes_string(x = col)) +
      geom_histogram(bins = n_bins, fill = color, color = "black") +
      ggtitle(paste(title_prefix, col)) +
      theme_minimal()
  })
}

# Histograms for normal data
histograms_normal <- create_histograms(df_normal, "blue", "Histogram of")
# Histograms for non-homoscedastic data
histograms_non_homoscedastic <- create_histograms(df_non_homoscedastic, "red", "Histogram of")

# Arrange all histograms in grids
do.call(grid.arrange, c(histograms_normal, ncol = 3, top = "Histograms for Normal Data"))
do.call(grid.arrange, c(histograms_non_homoscedastic, ncol = 3, top = "Histograms for Non-Homoscedastic Data"))
