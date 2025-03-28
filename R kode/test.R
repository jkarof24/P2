# Load the necessary libraries
library(tidyverse)

# Assign the diamonds dataset to a variable
diamonds_data <- agg_dataNum

# View the first few rows of the dataset
head(diamonds_data)

# Summarize the dataset
summary(diamonds_data)

# Visualize the distribution of a numeric variable (e.g., price)
ggplot(diamonds_data, aes(x = )) +
  geom_histogram(binwidth = 500, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Diamond Prices", x = "Price", y = "Count")

# Check for missing values
sum(is.na(diamonds_data))