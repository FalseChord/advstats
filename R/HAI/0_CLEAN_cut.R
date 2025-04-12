# 原始資料： 3508 筆
# 95% 區間： 3059 筆
# 99% 區間： 3409 筆
# 99.9% 區間： 3501 筆

library(tidyverse)
library(stringr)

# 1. 讀取原始資料
data <- read_csv("data/HAI_cleaned.csv", col_names = FALSE) %>%
  rename(Index = X1, Context = X2, Response = X3, LLM = X4)

# 2. 計算各類型文本的分位數（包含下界）
get_percentiles <- function(text_vector) {
  text_lengths <- str_length(text_vector)
  percentiles <- list(
    p001 = quantile(text_lengths, 0.001),  # 下界 0.1%
    p01 = quantile(text_lengths, 0.01),    # 下界 1%
    p05 = quantile(text_lengths, 0.05),    # 下界 5%
    p95 = quantile(text_lengths, 0.95),    # 上界 95%
    p99 = quantile(text_lengths, 0.99),    # 上界 99%
    p999 = quantile(text_lengths, 0.999)   # 上界 99.9%
  )
  return(percentiles)
}

# 分別計算各文本類型的分位數
q_percentiles <- get_percentiles(data$Context)
h_percentiles <- get_percentiles(data$Response)
a_percentiles <- get_percentiles(data$LLM)

# 3. 過濾和截斷函數
filter_and_truncate <- function(text, min_length, max_length) {
  text_length <- str_length(text)
  if(text_length < min_length) {
    return(NA)  # 移除過短的文本
  } else if(text_length > max_length) {
    return(str_sub(text, 1, max_length))  # 截斷過長的文本
  }
  return(text)
}

# 4. 生成不同版本的資料
# 95% 區間（移除 5% 最短和 5% 最長）
data_95 <- data %>%
  mutate(
    Context = map_chr(Context, ~filter_and_truncate(., q_percentiles$p05, q_percentiles$p95)),
    Response = map_chr(Response, ~filter_and_truncate(., h_percentiles$p05, h_percentiles$p95)),
    LLM = map_chr(LLM, ~filter_and_truncate(., a_percentiles$p05, a_percentiles$p95))
  ) %>%
  filter(!is.na(Context) & !is.na(Response) & !is.na(LLM))

# 99% 區間（移除 1% 最短和 1% 最長）
data_99 <- data %>%
  mutate(
    Context = map_chr(Context, ~filter_and_truncate(., q_percentiles$p01, q_percentiles$p99)),
    Response = map_chr(Response, ~filter_and_truncate(., h_percentiles$p01, h_percentiles$p99)),
    LLM = map_chr(LLM, ~filter_and_truncate(., a_percentiles$p01, a_percentiles$p99))
  ) %>%
  filter(!is.na(Context) & !is.na(Response) & !is.na(LLM))

# 99.9% 區間（移除 0.1% 最短和 0.1% 最長）
data_999 <- data %>%
  mutate(
    Context = map_chr(Context, ~filter_and_truncate(., q_percentiles$p001, q_percentiles$p999)),
    Response = map_chr(Response, ~filter_and_truncate(., h_percentiles$p001, h_percentiles$p999)),
    LLM = map_chr(LLM, ~filter_and_truncate(., a_percentiles$p001, a_percentiles$p999))
  ) %>%
  filter(!is.na(Context) & !is.na(Response) & !is.na(LLM))

# 5. 輸出過濾和截斷點資訊
cat("文本長度範圍：\n")
cat("\n問題文本：\n")
print(q_percentiles)
cat("\n人類回應：\n")
print(h_percentiles)
cat("\nAI回應：\n")
print(a_percentiles)

# 6. 輸出資料集大小資訊
cat("\n資料集大小：\n")
cat("原始資料：", nrow(data), "筆\n")
cat("95% 區間：", nrow(data_95), "筆\n")
cat("99% 區間：", nrow(data_99), "筆\n")
cat("99.9% 區間：", nrow(data_999), "筆\n")

# 7. 儲存結果
write_csv(data_95, "data/HAI_filtered_95.csv")
write_csv(data_99, "data/HAI_filtered_99.csv")
write_csv(data_999, "data/HAI_filtered_999.csv")