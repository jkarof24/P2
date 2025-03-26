library(parallel)
library(dplyr)
library(moments)
library(ggplot2)
library(gridExtra)
library(foreach)
library(doParallel)

# Set working directory and read data
setwd("C:/Users/Jonathan/Documents/GitHub/P2")
data <- read.csv("auto-mpg.csv", na.strings = ".")

# Select numeric columns
numeric_data <- data[sapply(data, is.numeric)]

# Define the strata variable (e.g., origin)
strata_var <- "mpg"

# Function to perform stratified sampling
stratified_sampling <- function(data, strata_var, sample_size) {
  data %>%
    group_by(.data[[strata_var]]) %>%
    sample_n(sample_size, replace = TRUE) %>%
    ungroup()
}

fit_polynomial_regression <- function(data) {
  formula <- as.formula(paste("mpg ~", paste(sapply(setdiff(names(data), "mpg"), function(var) paste("poly(", var, ", 2)")), collapse = " + ")))
  lm(formula, data = data)
}

run_simulations <- function(n_simulations, numeric_data, strata_var, sample_size_per_stratum) {
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  clusterExport(cl, c("numeric_data", "fit_polynomial_regression", "stratified_sampling", "strata_var", "sample_size_per_stratum", "%>%", "across", "summarise"))
  clusterEvalQ(cl, {
    library(dplyr)
  })
  
  results <- foreach(i = 1:n_simulations, .combine = rbind, .packages = c("dplyr")) %dopar% {
    resampled_data <- stratified_sampling(numeric_data, strata_var, sample_size_per_stratum)
    model <- fit_polynomial_regression(resampled_data)
    c(coef(model), summary(model)$r.squared)
  }
  
  stopCluster(cl)
  
  results_df <- as.data.frame(results)
  colnames(results_df) <- c(names(coef(fit_polynomial_regression(numeric_data))), "r_squared")
  
  return(results_df)
}

set.seed(200)
n_simulations <- 1000
sample_size_per_stratum <- 75  # Adjust sample size as needed
results_df <- run_simulations(n_simulations, numeric_data, strata_var, sample_size_per_stratum)

# Clean column names to remove special characters
clean_colnames <- function(df) {
  colnames(df) <- make.names(colnames(df), unique = TRUE)
  return(df)
}

results_df <- clean_colnames(results_df)

create_histograms <- function(df) {
  plots <- lapply(names(df), function(col) {
    n_bins <- ceiling(log2(length(df[[col]])) + 1000)
    mean_value <- mean(df[[col]])
    sd_value <- sd(df[[col]])
    
    ggplot(df, aes_string(x = col)) +
      geom_histogram(aes(y = ..density..), bins = n_bins, fill = "blue", color = "blue") +
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