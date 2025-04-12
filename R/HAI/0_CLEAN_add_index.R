# Load required libraries
library(readr)
library(dplyr)

# Read the CSV file
hai_data <- read_csv("data/HAI.csv")

# Add index column
hai_data <- hai_data %>%
  mutate(Index = row_number()) %>%
  select(Index, everything())  # Move Index to first column

# Write back to CSV
write_csv(hai_data, "data/HAI.csv")

print("Added index column to HAI.csv")
