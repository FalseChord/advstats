# 定義函數:
# get_sentences(): 將文本分割成句子
# get_words(): 將文本分割成單字
# get_metrics(): 計算文本指標

# 計算的指標包括:
# 字數(word count)
# 句子數(sentence count)
# 平均句子長度(words per sentence)
# 平均單字長度(characters per word)


# Load required libraries
library(stringr)
library(dplyr)
library(readr)

# Function to get sentences
get_sentences <- function(text) {
  # Split text into sentences using common sentence endings
  sentences <- unlist(str_split(text, "[.!?]+"))
  # Remove empty sentences and trim whitespace
  sentences <- sentences[nzchar(trimws(sentences))]
  return(trimws(sentences))
}

# Function to get words
get_words <- function(text) {
  # Split text into words and remove empty strings
  words <- unlist(str_split(text, "\\s+"))
  words <- words[nzchar(words)]
  return(words)
}

# Function to calculate metrics
get_metrics <- function(text) {
  sentences <- get_sentences(text)
  words <- get_words(text)
  
  # Calculate metrics
  sentence_count <- length(sentences)
  word_count <- length(words)
  avg_sentence_length <- ifelse(sentence_count > 0, 
                              word_count / sentence_count, 
                              0)
  avg_word_length <- ifelse(length(words) > 0,
                           mean(nchar(words)),
                           0)
  
  return(list(
    word_count = word_count,
    sentence_count = sentence_count,
    avg_sentence_length = round(avg_sentence_length, 2),
    avg_word_length = round(avg_word_length, 2)
  ))
}

# Read the CSV file
hai_data <- read_csv("data/HAI_filtered_95.csv")

# Process questions and write to CSV
metrics_df <- data.frame(
  Index = integer(),
  Word_Count = integer(),
  Sentence_Count = integer(),
  Avg_Sentence_Length = numeric(),
  Avg_Word_Length = numeric(),
  stringsAsFactors = FALSE
)

for(i in seq_len(nrow(hai_data))) {
  # Calculate metrics for each text type
  for(text_type in c("Question", "Human_Response", "AI_Response")) {
    text <- switch(text_type,
                  "Question" = hai_data$Context[i],
                  "Human_Response" = hai_data$Response[i],
                  "AI_Response" = hai_data$LLM[i])

    metrics <- get_metrics(text)
    metrics_df <- rbind(metrics_df, data.frame(
      Index = hai_data$Index[i],
      Text_Type = text_type,
      Word_Count = metrics$word_count,
      Sentence_Count = metrics$sentence_count,
      Avg_Sentence_Length = metrics$avg_sentence_length,
      Avg_Word_Length = metrics$avg_word_length
    ))
  }
}

# Standardize metrics
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Create standardized columns and rename original metrics
metrics_df <- metrics_df %>%
  group_by(Text_Type) %>%
  mutate(
    Word_Count_raw = Word_Count,
    Sentence_Count_raw = Sentence_Count,
    Avg_Sentence_Length_raw = Avg_Sentence_Length,
    Avg_Word_Length_raw = Avg_Word_Length,
    
    Word_Count = standardize(Word_Count),
    Sentence_Count = standardize(Sentence_Count),
    Avg_Sentence_Length = standardize(Avg_Sentence_Length),
    Avg_Word_Length = standardize(Avg_Word_Length)
  ) %>%
  ungroup()

# Write results to CSV
write_csv(metrics_df, "data/HAI_metrics_text.csv")
print("Text metrics (including raw scores) have been saved to HAI_metrics_text.csv")
