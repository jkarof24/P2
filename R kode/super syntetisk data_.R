library(ggplot2)
library(gridExtra)
library(lmtest)  # Tilføj lmtest for Breusch-Pagan testen

# Set seed for reproducibility
set.seed(223)

# Generate independent variables
n <- 250
x1 <- rnorm(n, mean = 5, sd = 1)
x2 <- rnorm(n, mean = 10, sd = 4)
x3 <- rnorm(n, mean = 15, sd = 3)
x4 <- rnorm(n, mean = 20, sd = 5)

# Generate dependent variable with a polynomial relationship
y <- 3 + 2*x1 + 5*x1^2 + 1.5*x2 + 3*x3^2 + 2*x4 + 0.001*rnorm(n, mean = 0, sd = 1)

# Create a data frame
data <- data.frame(y, x1, x2, x3, x4)

# Fit a polynomial regression model
model <- lm(y ~ poly(x1, 2) + x2 + poly(x3, 2) + x4, data = data)

# Summary of the model
summary(model)

# Add an error term to x1 that scales with the corresponding y value
x3_new <- x3 + rnorm(n, mean = 0, sd = 0.00254 * abs(y))

# Generate new dependent variable with the same polynomial relationship
y_new <- 3 + 2*x1 + 5*x1^2 + 1.5*x2 + 3*x3_new^2 + 2*x4 + 0.001*rnorm(n, mean = 0, sd = 1)

# Create a new data frame
data_new <- data.frame(y_new, x1 = x1, x2, x3_new, x4)

# Fit a new polynomial regression model
model_new <- lm(y_new ~ poly(x1, 2) + x2 + poly(x3, 2) + x4, data = data_new)

# Summary of the new model
summary(model_new)

# Diagnostic plots for the original model
par(mfrow = c(2, 2))
plot(model)

# Diagnostic plots for the new model
par(mfrow = c(2, 2))
plot(model_new)

# Function to create scatter plots for each numeric column against 'y'
scatter_plots <- function(data, color, title_prefix) {
  lapply(names(data)[-1], function(col) {
    ggplot(data, aes_string(x = "y", y = col)) +
      geom_point(color = color) +
      ggtitle(paste(title_prefix, col, "vs y")) +
      theme_minimal()
  })
}

# Scatter plots for normal data
scatter_plots_normal <- scatter_plots(data, "blue", "Normal Data: Scatter Plot of")
# Scatter plots for non-homoscedastic data
scatter_plots_non_homoscedastic <- scatter_plots(data_new, "red", "Non-Homoscedastic Data: Scatter Plot of")

# Arrange scatter plots in grids
grid.arrange(grobs = scatter_plots_normal, ncol = 2, top = "Scatter Plots for Normal Data")
grid.arrange(grobs = scatter_plots_non_homoscedastic, ncol = 2, top = "Scatter Plots for Non-Homoscedastic Data")

# Function to create histograms for each numeric column
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
histograms_normal <- create_histograms(data, "blue", "Histogram of")
# Histograms for non-homoscedastic data
histograms_non_homoscedastic <- create_histograms(data_new, "red", "Histogram of")

# Arrange histograms in grids
do.call(grid.arrange, c(histograms_normal, ncol = 3, top = "Histograms for Normal Data"))
do.call(grid.arrange, c(histograms_non_homoscedastic, ncol = 3, top = "Histograms for Non-Homoscedastic Data"))

# Breusch-Pagan test for the original model
bp_test <- bptest(model)
print(bp_test)
bp_test <- bptest(model_new)
print(bp_test)


