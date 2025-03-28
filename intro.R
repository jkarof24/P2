library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)
library(tidyr)
library(caret)
library(fastDummies)
library(moments)
setwd("C:/Users/jonat/Documents/GitHub/P2")
data = read.csv("auto-mpg.csv", na.strings = ".")
summary(data)

numeric_data <- data[sapply(data, is.numeric)]
cor(numeric_data)


# Function to calculate and print moments for each column
calculate_moments <- function(data) {
  moments <- data %>%
    summarise(across(everything(), list(
      mean = ~ mean(.),
      variance = ~ var(.),
      skewness = ~ skewness(.),
      kurtosis = ~ kurtosis(.)
    )))
  
  print(moments)
}

# Calculate and print moments for all columns
calculate_moments(numeric_data)




dependent_variable <- "mpg"

# Lav scatter plots for hver kolonne i numeric_data mod 'mpg'
scatter_plots <- lapply(names(numeric_data), function(col) {
  ggplot(numeric_data, aes_string(x = col, y = dependent_variable)) +
    geom_point(color = "blue") +
    ggtitle(paste("Scatter Plot of", col, "vs", dependent_variable)) +
    theme_minimal()
})

do.call(grid.arrange, scatter_plots)
  
  
  
  
plots <- lapply(names(numeric_data), function(col) {
  n_bins <- ceiling(log2(length(numeric_data[[col]])) + 1)
  
  ggplot(numeric_data, aes_string(x = col)) +
    geom_histogram(bins = 100, fill = "blue", color = "black") +
    ggtitle(paste("Histogram of", col)) +
    theme_minimal()
})

# Arrange all plots in a grid
do.call(grid.arrange, c(plots, ncol = 3))



plots <- lapply(names(numeric_data), function(col) {
  n_bins <- ceiling(log2(length(numeric_data[[col]])) + 1)
  mean_value <- mean(numeric_data[[col]])
  sd_value <- sd(numeric_data[[col]])
  
  ggplot(numeric_data, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = n_bins, fill = "blue", color = "black") +
    geom_density(color = "red", size = 1) +
    geom_vline(aes(xintercept = mean_value), color = "green", linetype = "dashed", size = 1) +
    ggtitle(paste("Density, Mean and SD of", col)) +
    theme_minimal() +
    annotate("text", x = Inf, y = Inf, label = paste("Mean:", round(mean_value, 2), "\nSD:", round(sd_value, 2)), 
             hjust = 1.1, vjust = 2, color = "black", size = 4)
})

do.call(grid.arrange, c(plots, ncol = 3))

# Create a matrix with the provided correlation data
data_heatmap <- matrix(c(
  1.0000000, -0.7753963, -0.8042028, -0.8317409, 0.4202889, 0.5792671, 0.5634504,
  -0.7753963, 1.0000000, 0.9507214, 0.8960168, -0.5054195, -0.3487458, -0.5625433,
  -0.8042028, 0.9507214, 1.0000000, 0.9328241, -0.5436841, -0.3701642, -0.6094094,
  -0.8317409, 0.8960168, 0.9328241, 1.0000000, -0.4174573, -0.3065643, -0.5810239,
  0.4202889, -0.5054195, -0.5436841, -0.4174573, 1.0000000, 0.2881370, 0.2058730,
  0.5792671, -0.3487458, -0.3701642, -0.3065643, 0.2881370, 1.0000000, 0.1806622,
  0.5634504, -0.5625433, -0.6094094, -0.5810239, 0.2058730, 0.1806622, 1.0000000
), nrow = 7, byrow = TRUE)

# Set row and column names
rownames(data_heatmap) <- colnames(data_heatmap) <- c("mpg", "cylinders", "displacement", "weight", "acceleration", "model.year", "origin")

# Convert the matrix to a data frame for ggplot2
data_melt <- melt(data_heatmap)

# Create the heatmap
ggplot(data_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient2(low = "gray", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "")     



# Function to calculate percentage of NA values for each column
na_percentage <- function(column) {
  na_count <- sum(is.na(column))
  total_count <- length(column)
  (na_count / total_count) * 100
}

# Apply the function to each column in the dataset
na_percentages <- sapply(data, na_percentage)

# Print the percentages
print(na_percentages)


# Convert car.name to factor and create dummy variables
#data <- data %>%
 # dummy_cols(select_columns = "car.name", remove_first_dummy = TRUE)

# Replace spaces in dummy variable names with underscores
#names(data) <- gsub(" ", "_", names(data))

# Create a list of polynomial terms for numerical columns
#poly_terms <- paste("poly(", c("cylinders", "displacement", "horsepower", "weight", "acceleration", "model.year", "origin"), ", 2)", collapse = " + ")

# Create a list of dummy variable terms for car.name
#dummy_terms <- names(data)[grepl("car.name_", names(data))]

# Combine the terms into the formula using reformulate
#formula <- reformulate(c(poly_terms, dummy_terms), response = "mpg")

# Fit the polynomial regression model
#model <- lm(formula, data = data)

# Display the summary of the model
#summary(model)


str(data)

# Convert columns to numeric if necessary
data$cylinders <- as.numeric(data$cylinders)
data$displacement <- as.numeric(data$displacement)
data$horsepower <- as.numeric(data$horsepower)
data$weight <- as.numeric(data$weight)
data$acceleration <- as.numeric(data$acceleration)
data$model.year <- as.numeric(data$model.year)
data$origin <- as.numeric(data$origin)

data$cylinders[is.na(data$cylinders)] <- mean(data$cylinders, na.rm = TRUE)
data$displacement[is.na(data$displacement)] <- mean(data$displacement, na.rm = TRUE)
data$horsepower[is.na(data$horsepower)] <- mean(data$horsepower, na.rm = TRUE)
data$weight[is.na(data$weight)] <- mean(data$weight, na.rm = TRUE)
data$acceleration[is.na(data$acceleration)] <- mean(data$acceleration, na.rm = TRUE)
data$model.year[is.na(data$model.year)] <- mean(data$model.year, na.rm = TRUE)
data$origin[is.na(data$origin)] <- mean(data$origin, na.rm = TRUE)

# Create a list of polynomial terms for numerical columns
poly_terms <- paste("poly(", c("cylinders", "displacement", "horsepower", "weight", "acceleration", "model.year", "origin"), ", 2)", collapse = " + ")

# Use reformulate to create the formula without car.name dummy variables
formula <- reformulate(poly_terms, response = "mpg")

# Fit the polynomial regression model
model <- lm(formula, data = data)

# Display the summary of the model
summary(model)

