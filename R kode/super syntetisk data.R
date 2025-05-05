# Set working directory and read data
setwd("C:/Users/Jonathan/Documents/GitHub/P2")
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
  # Generate normal data
  data <- rnorm(size)
  
  # Adjust mean and variance
  data <- (data - mean(data)) / sd(data) * sqrt(variance_target) + mean_target
  
  # Adjust skewness and kurtosis using the Pearson system
  data <- rpearson(size, moments = c(mean_target, variance_target, skewness_target, kurtosis_target))
  
  return(data)
}

# Generate new data for each column
generated_data <- numeric_data %>%
  mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))

fit_polynomial_regression <- function(data) {
  response_var <- "mpg"
  predictor_vars <- setdiff(names(data), response_var)
  
  # Corrected formula generation
  formula <- as.formula(paste(response_var, "~", paste(sapply(predictor_vars, function(var) paste("poly(", var, ", 2)")), collapse = " + ")))
  
  model <- lm(formula, data = data)
  return(model)
}

set.seed(400)
n_simulations <- 100
results <- replicate(n_simulations, {
  simulated_data <- numeric_data %>%
    mutate(across(everything(), ~ generate_data(n(), mean(.), var(.), skewness(.), kurtosis(.))))
  model <- fit_polynomial_regression(simulated_data)
  coefficients <- coef(model)
  r_squared <- summary(model)$r.squared
  c(coefficients, r_squared)
})

# Convert results to a data frame
results_df <- as.data.frame(t(results))

# Function to create histograms for each column in the results
create_histograms <- function(results_df) {
  for (col in names(results_df)) {
    hist(results_df[[col]], main = paste("Histogram of", col), xlab = col, ylab = "Frequency", col = "lightblue", border = "black")
  }
  return("Histograms created successfully")
}

# Create histograms and get confirmation
i <- create_histograms(results_df)
print(i)
