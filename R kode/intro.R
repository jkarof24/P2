library(ggplot2)
library(dplyr)
library(gridExtra)
library(reshape2)
library(lmtest)
library(gridExtra)
# Indlæs data
data <- read.csv("auto-mpg.csv", na.strings = ".")

# Funktion til at beregne procentdelen af NA-værdier for hver kolonne
na_percentage <- function(column) {
  sum(is.na(column)) / length(column) * 100
}

# Udskriv procentdelene af NA-værdier
print(sapply(data, na_percentage))

# Convert horsepower to numeric, coercing non-numeric values to NA
data$horsepower <- as.numeric(data$horsepower)



# Remove rows with any NA values
data <- data %>% na.omit()

# Udvælg numeriske kolonner
numeric_data <- data %>% select_if(is.numeric)

# Beregn korrelationer og smelt til format for ggplot2
data_korrelationer <- melt(cor(numeric_data, use = "complete.obs"))

# Opret heatmap baseret på korrelationer
ggplot(data_korrelationer, aes(x = Var1, y = Var2, fill = abs(value))) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red", limit = c(0, 1), name="|Correlation|") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix Heatmap", x = "", y = "")

# Lav scatter plots for hver kolonne mod 'mpg'
scatter_plots <- lapply(names(numeric_data), function(col) {
  ggplot(numeric_data, aes_string(x = col, y = "mpg")) +
    geom_point(color = "blue") +
    ggtitle(paste("Scatter Plot of", col, "vs mpg")) +
    theme_minimal()
})

# Beregn Breusch–Pagan test p-værdier
bp_results <- sapply(names(numeric_data), function(col) {
  model <- lm(mpg ~ numeric_data[[col]], data = numeric_data)
  bp_test <- bptest(model)
  bp_test$p.value
})

# Opret data frame til p-værdier
bp_df <- data.frame(Variable = names(bp_results), P_Value = bp_results)

print(bp_df)

# Arranger scatter plots og Breusch–Pagan plot
do.call(grid.arrange, c(scatter_plots, ncol = 3))

# Lav histogrammer og density plots med mean og SD
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