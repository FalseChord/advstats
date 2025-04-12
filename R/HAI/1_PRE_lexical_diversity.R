# 這個腳本會計算以下詞彙多樣性指標：
# Type-Token Ratio (TTR)：unique words / total words
# Yule's K：衡量詞彙重複使用的程度
# Simpson's D：衡量詞彙集中度
# Herdan's C：TTR 的自然對數版本
# Brunet's W：使用 -0.165 作為常數的詞彙豐富度指標
# Honore's R：考慮只出現一次的詞(hapax legomena)的詞彙豐富度指標

# Load required libraries
library(readr)
library(dplyr)
library(stringr)

# Function to get cleaned words
get_words <- function(text) {
  # Convert to lowercase and remove punctuation
  text <- tolower(text)
  text <- str_replace_all(text, "[[:punct:]]", "")
  # Split into words and remove empty strings
  words <- unlist(str_split(text, "\\s+"))
  words <- words[nzchar(words)]
  return(words)
}

# Function to calculate lexical diversity metrics
calculate_diversity <- function(words) {
  N <- length(words)  # Total number of words (tokens)
  V <- length(unique(words))  # Number of unique words (types)
  
  # Calculate frequency of each word
  freq_table <- table(words)
  freq_dist <- as.vector(freq_table)
  
  # Type-Token Ratio (TTR)
  TTR <- V / N
  
  # Yule's K
  Yule_K <- 10000 * (sum((freq_dist^2) * (1:length(freq_dist))) - N) / (N^2)
  
  # Simpson's D
  Simpson_D <- sum(freq_dist * (freq_dist - 1)) / (N * (N - 1))
  
  # Herdan's C (Natural log TTR)
  Herdan_C <- log(V) / log(N)
  
  # Brunet's W
  Brunet_W <- N ^ (V ^ -0.165)
  
  # Honore's R
  V1 <- sum(freq_dist == 1)  # Number of words occurring once
  Honore_R <- 100 * log(N) / (1 - (V1 / V))
  
  return(list(
    TTR = round(TTR, 4),
    Yule_K = round(Yule_K, 4),
    Simpson_D = round(Simpson_D, 4),
    Herdan_C = round(Herdan_C, 4),
    Brunet_W = round(Brunet_W, 4),
    Honore_R = round(Honore_R, 4)
  ))
}

# Read the CSV file
hai_data <- read_csv("data/HAI_filtered_95.csv")

# Create empty dataframe for results
diversity_df <- data.frame(
  Index = integer(),
  TTR = numeric(),
  Yule_K = numeric(),
  Simpson_D = numeric(),
  Herdan_C = numeric(),
  Brunet_W = numeric(),
  Honore_R = numeric(),
  Text_Type = character(),
  stringsAsFactors = FALSE
)

# Process each row
for(i in seq_len(nrow(hai_data))) {
  # Calculate metrics for Context (question)
  question_words <- get_words(hai_data$Context[i])
  question_metrics <- calculate_diversity(question_words)
  
  # Calculate metrics for Response (human response)
  response_words <- get_words(hai_data$Response[i])
  response_metrics <- calculate_diversity(response_words)
  
  # Calculate metrics for LLM (AI response)
  llm_words <- get_words(hai_data$LLM[i])
  llm_metrics <- calculate_diversity(llm_words)
  
  # Add all results to dataframe
  diversity_df <- rbind(
    diversity_df,
    # Question metrics
    data.frame(
      Index = hai_data$Index[i],
      as.data.frame(question_metrics),
      Text_Type = "Question"
    ),
    # Human response metrics
    data.frame(
      Index = hai_data$Index[i],
      as.data.frame(response_metrics),
      Text_Type = "Human_Response"
    ),
    # LLM response metrics
    data.frame(
      Index = hai_data$Index[i],
      as.data.frame(llm_metrics),
      Text_Type = "AI_Response"
    )
  )
}

# Standardize metrics
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Create standardized columns and rename original metrics
diversity_df <- diversity_df %>%
  group_by(Text_Type) %>%
  mutate(
    TTR_raw = TTR,
    Yule_K_raw = Yule_K,
    Simpson_D_raw = Simpson_D,
    Herdan_C_raw = Herdan_C,
    Brunet_W_raw = Brunet_W,
    Honore_R_raw = Honore_R,
    
    TTR = standardize(TTR),
    Yule_K = standardize(Yule_K),
    Simpson_D = standardize(Simpson_D),
    Herdan_C = standardize(Herdan_C),
    Brunet_W = standardize(Brunet_W),
    Honore_R = standardize(Honore_R)
  ) %>%
  ungroup()

# Write results to CSV
write_csv(diversity_df, "data/HAI_metrics_diversity.csv")
print("Lexical diversity metrics (including raw scores) have been saved to HAI_metrics_diversity.csv")
