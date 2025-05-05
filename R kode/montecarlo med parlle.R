library(parallel)
library(dplyr)
library(moments)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)

# Define or load the rpearson function
rpearson <- function(n, moments) {
  # Custom implementation of rpearson
  # This is a placeholder. Replace with the actual implementation.
  rnorm(n)
}

# Set working directory and read data
setwd("C:/Users/Jonathan/Documents/GitHub/P2/R kode")
data <- read.csv("auto-mpg.csv", na.strings = ".")

# Select numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Calculate moments for all columns
moments <- numeric_data %>%
  summarise(across(everything(), list(
    mean = ~ mean(.),
    variance = ~ var(.),
    skewness = ~ skewness(.),
    kurtosis = ~ kurtosis(.)
  )))

generate_data <- function(size, mean_target, variance_target, skewness_target, kurtosis_target) {
  data <- rnorm(size)
  data <- (data - mean(data)) / sd(data) * sqrt(variance_target) + mean_target
  data <- rpearson(size, moments = c(mean_target, variance_target, skewness_target, kurtosis_target))
  return(data)
}

fit_polynomial_regression <- function(data) {
  formula <- as.formula(paste("mpg ~", paste(sapply(setdiff(names(data), "mpg"), function(var) paste("poly(", var, ", 2)")), collapse = " + ")))
  lm(formula, data = data)
}

run_simulations <- function(n_simulations, numeric_data) {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("numeric_data", "generate_data", "fit_polynomial_regression", "moments", "%>%", "across", "summarise", "rpearson"))
  clusterEvalQ(cl, {
    library(dplyr)
    library(moments)
  })
  
  results <- foreach(i = 1:n_simulations, .combine = rbind, .packages = c("dplyr", "moments")) %dopar% {
    simulated_data <- numeric_data %>%
      mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))
    model <- fit_polynomial_regression(simulated_data)
    c(coef(model), summary(model)$r.squared)
  }
  
  stopCluster(cl)
  
  results_df <- as.data.frame(results)
  colnames(results_df) <- c(names(coef(fit_polynomial_regression(numeric_data))), "r_squared")
  
  return(results_df)
}

set.seed(421)
n_simulations <- 1000
results_df <- run_simulations(n_simulations, numeric_data)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)

create_histograms <- function(df) {
  plots <- list()
  
  for (col in names(df)) {
    bin_width <- (max(df[[col]]) - min(df[[col]])) / 30
    p <- ggplot(df, aes(x = .data[[col]])) +
      geom_histogram(binwidth = bin_width, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Histogram of", col), x = col, y = "Frequency") +
      theme_minimal()
    plots[[col]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 2)
}

# Generate histograms for the simulation results
create_histograms(results_df)