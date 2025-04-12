library(dplyr)
library(stringr)

clean_text <- function(text) {
  # Handle NA values
  if (is.na(text)) {
    return("")
  }
  
  # Convert to string if not already
  text <- as.character(text)
  
  # Remove URLs
  text <- str_replace_all(text, "http\\S+|www\\S+|https\\S+", "")
  
  # Replace tabs, newlines and other whitespace with single space
  text <- str_replace_all(text, "[\t\n\r\f\v]+", " ")
  
  # Remove special characters and numbers but keep periods
  text <- str_replace_all(text, "[^a-zA-Z\\s\\.]", " ")
  
  # Remove extra whitespace
  text <- str_squish(text)
  
  # Remove multiple periods
  text <- str_replace_all(text, "\\.+", ".")
  
  # Remove spaces before periods
  text <- str_replace_all(text, "\\s+\\.", ".")
  
  # Remove single letters
  text <- str_replace_all(text, "\\s+[a-zA-Z]\\s+", " ")
  
  # Remove leading/trailing periods and whitespace
  text <- str_trim(text, "both")
  text <- str_trim(str_replace_all(text, "^\\.|\\.$", ""), "both")
  
  return(text)
}

# Read the CSV file
tryCatch({
  df <- read.csv("data/HAI.csv", stringsAsFactors = FALSE)
}, error = function(e) {
  stop(paste("Error reading CSV file:", e$message))
})

# Clean Context and Response columns
df <- df %>%
  mutate(
    Context_clean = sapply(Context, clean_text),
    Response_clean = sapply(Response, clean_text)
  )

# Save cleaned data
tryCatch({
  write.csv(df, "data/HAI_cleaned.csv", row.names = FALSE)
}, error = function(e) {
  stop(paste("Error saving CSV file:", e$message))
})
