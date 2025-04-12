# 這個腳本計算了多種可讀性指標：
# Flesch Reading Ease：分數範圍 0-100，根據單字和句子的長度來衡量文本的易讀性
# Flesch-Kincaid Grade Level：理解給定文本所需的美國學年程度
# Automated Readability Index (ARI)：它根據字元數估計了理解文本所需的年級程度
# Coleman-Liau Index：基於字元而非音節，預測文本理解所需美國年級的指標
# Gunning Fog Index：考慮複雜詞彙(3音節以上)的比例，初次閱讀理解給定文本所需的正規教育年數
# SMOG Index：特別適用於評估健康相關文本，理解文本所需的教育年數，特別關注包含多個音節的單字
# LIX (Läsbarhets Index)：瑞典可讀性指標，考慮單字和句子的長度來評估文本的難度
# RIX (Anderson's Readability Index)：簡化版的 LIX，它根據長字相對於句子數量的比例來評估文本的易讀性

# Load required libraries
library(readr)
library(dplyr)
library(stringr)
library(quanteda)

# Function to get cleaned text statistics
get_text_stats <- function(text) {
  # Clean and prepare text
  text <- str_replace_all(text, "\\s+", " ")  # Normalize whitespace
  
  # Get words
  words <- unlist(str_split(text, "\\s+"))
  words <- words[nzchar(words)]
  
  # Get sentences
  sentences <- unlist(str_split(text, "[.!?]+\\s*"))
  sentences <- sentences[nzchar(trimws(sentences))]
  
  # Count syllables (basic implementation)
  count_syllables <- function(word) {
    # 轉小寫並移除非字母字元
    word <- tolower(word)
    word <- gsub("[^a-z]", "", word)
    
    # 特殊單詞字典
    special_cases <- list(
      "business" = 2,
      "wednesday" = 2,
      "february" = 3,
      "library" = 3,
      "dictionary" = 4,
      "interesting" = 3,
      "different" = 2
      # 可以繼續添加更多特殊情況
    )
    
    # 檢查是否為特殊情況
    if (!is.null(special_cases[[word]])) {
      return(special_cases[[word]])
    }
    
    # 子音+le結尾的規則
    if (str_detect(word, "[^aeiou]le$")) {
      syl_count <- 1
    } else {
      syl_count <- 0
    }
    
    # 處理常見的音節規則
    word <- gsub("(?![aeiou])[aeiouy]+(?![aeiou])", "a", word, perl = TRUE)  # 合併連續母音
    word <- gsub("tion", "shun", word)  # 處理 -tion
    word <- gsub("sion", "zhun", word)  # 處理 -sion
    word <- gsub("(?<=[aeiou])ed$", "", word, perl = TRUE)  # 處理過去式 -ed
    
    # 特殊組合規則
    subtractions <- c(
      "ia", "iu", "io",     # 雙母音
      "ea", "eu", "oa", "ou", "oo", "ue", "ay", "ey", 
      "ie", "ei", "ai", "au", "oi", "oy",
      "cial", "tial",       # 特殊後綴
      "ious", "gious", 
      "([^aeiou])ely$"      # -ely 結尾
    )
    
    # 計算基本音節
    vowel_count <- str_count(word, "[aeiouy]+")
    
    # 應用減法規則
    for (pattern in subtractions) {
      vowel_count <- vowel_count - str_count(word, pattern)
    }
    
    # 處理結尾的 e
    if (str_detect(word, "e$") && !str_detect(word, "[aeiou]e$")) {
      vowel_count <- vowel_count - 1
    }
    
    # 處理結尾的 es 和 ed
    if (str_detect(word, "(es|ed)$") && !str_detect(word, "[aeiouy](es|ed)$")) {
      vowel_count <- vowel_count - 1
    }
    
    # 加上之前計算的 -le 結尾音節
    vowel_count <- vowel_count + syl_count
    
    # 確保至少有一個音節
    return(max(1, vowel_count))
  }
  
  # 只呼叫一次 count_syllables 並儲存結果
  syllables_per_word <- sapply(words, count_syllables)
  
  # 使用儲存的結果計算所需的統計量
  n_words <- length(words)
  n_sentences <- length(sentences)
  n_chars <- nchar(gsub("[^[:alnum:]]", "", text))
  n_syllables <- sum(syllables_per_word)
  avg_word_length <- mean(nchar(words))
  
  # 計算複雜詞（3音節以上）
  complex_words <- sum(syllables_per_word >= 3)
  
  return(list(
    words = n_words,
    sentences = n_sentences,
    characters = n_chars,
    syllables = n_syllables,
    avg_word_length = avg_word_length,
    complex_words = complex_words
  ))
}

# Function to calculate readability scores
calculate_readability <- function(stats) {
  # Flesch Reading Ease
  FRE <- 206.835 - 1.015 * (stats$words / stats$sentences) - 84.6 * (stats$syllables / stats$words)
  
  # Flesch-Kincaid Grade Level
  FKGL <- 0.39 * (stats$words / stats$sentences) + 11.8 * (stats$syllables / stats$words) - 15.59
  
  # Automated Readability Index
  ARI <- 4.71 * (stats$characters / stats$words) + 0.5 * (stats$words / stats$sentences) - 21.43
  
  # Coleman-Liau Index
  CLI <- 0.0588 * (stats$characters / stats$words * 100) - 0.296 * (stats$sentences / stats$words * 100) - 15.8
  
  # Gunning Fog Index
  GFI <- 0.4 * ((stats$words / stats$sentences) + 100 * (stats$complex_words / stats$words))
  
  # SMOG Index
  SMOG <- 1.0430 * sqrt(stats$complex_words * (30 / stats$sentences)) + 3.1291
  
  # LIX (Läsbarhets Index)
  LIX <- (stats$words / stats$sentences) + (stats$complex_words / stats$words * 100)
  
  # RIX (Anderson's Readability Index)
  RIX <- stats$complex_words / stats$sentences
  
  return(list(
    Flesch_Reading_Ease = round(FRE, 2),
    Flesch_Kincaid_Grade = round(FKGL, 2),
    Automated_Readability_Index = round(ARI, 2),
    Coleman_Liau_Index = round(CLI, 2),
    Gunning_Fog_Index = round(GFI, 2),
    SMOG_Index = round(SMOG, 2),
    LIX_Score = round(LIX, 2),
    RIX_Score = round(RIX, 2)
  ))
}

# Read the CSV file
hai_data <- read_csv("data/HAI_filtered_95.csv")

# Create empty dataframe for results
readability_df <- data.frame(
  Index = integer(),
  Text_Type = character(),
  Flesch_Reading_Ease = numeric(),
  Flesch_Kincaid_Grade = numeric(),
  Automated_Readability_Index = numeric(),
  Coleman_Liau_Index = numeric(),
  Gunning_Fog_Index = numeric(),
  SMOG_Index = numeric(),
  LIX_Score = numeric(),
  RIX_Score = numeric(),
  stringsAsFactors = FALSE
)

# Process each row
for(i in seq_len(nrow(hai_data))) {
  # Calculate metrics for each text type
  for(text_type in c("Question", "Human_Response", "AI_Response")) {
    text <- switch(text_type,
                  "Question" = hai_data$Context[i],
                  "Human_Response" = hai_data$Response[i],
                  "AI_Response" = hai_data$LLM[i])
    
    stats <- get_text_stats(text)
    scores <- calculate_readability(stats)
    
    readability_df <- rbind(
      readability_df,
      data.frame(
        Index = hai_data$Index[i],
        Text_Type = text_type,
        as.data.frame(scores)
      )
    )
  }
}

# Standardize metrics
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Create standardized columns and rename original metrics
readability_df <- readability_df %>%
  group_by(Text_Type) %>%
  mutate(
    Flesch_Reading_Ease_raw = Flesch_Reading_Ease,
    Flesch_Kincaid_Grade_raw = Flesch_Kincaid_Grade,
    Automated_Readability_Index_raw = Automated_Readability_Index,
    Coleman_Liau_Index_raw = Coleman_Liau_Index,
    Gunning_Fog_Index_raw = Gunning_Fog_Index,
    SMOG_Index_raw = SMOG_Index,
    LIX_Score_raw = LIX_Score,
    RIX_Score_raw = RIX_Score,
    
    Flesch_Reading_Ease = standardize(Flesch_Reading_Ease),
    Flesch_Kincaid_Grade = standardize(Flesch_Kincaid_Grade),
    Automated_Readability_Index = standardize(Automated_Readability_Index),
    Coleman_Liau_Index = standardize(Coleman_Liau_Index),
    Gunning_Fog_Index = standardize(Gunning_Fog_Index),
    SMOG_Index = standardize(SMOG_Index),
    LIX_Score = standardize(LIX_Score),
    RIX_Score = standardize(RIX_Score)
  ) %>%
  ungroup()

# Write results to CSV
write_csv(readability_df, "data/HAI_metrics_readability.csv")
print("Readability scores (including raw scores) have been saved to HAI_metrics_readability.csv")