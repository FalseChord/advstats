library(tidyverse)
library(psych)
library(corrplot)
library(knitr)
library(broom)

# ===== 1. 數據準備 =====
# 讀取可讀性數據
readability_data <- read_csv("data/HAI_metrics_readability.csv")

# 重組數據框架
prepare_paired_data <- function(data) {
  # 分別獲取問題、AI回答和人類回答的數據
  questions <- data %>% filter(Text_Type == "Question")
  ai_responses <- data %>% filter(Text_Type == "AI_Response")
  human_responses <- data %>% filter(Text_Type == "Human_Response")
  
  # 合併數據
  analysis_df <- data.frame(
    # 問題的指標
    Q_FRE = questions$Flesch_Reading_Ease,
    Q_FK = questions$Flesch_Kincaid_Grade,
    Q_ARI = questions$Automated_Readability_Index,
    Q_CLI = questions$Coleman_Liau_Index,
    Q_GFI = questions$Gunning_Fog_Index,
    Q_SMOG = questions$SMOG_Index,
    Q_LIX = questions$LIX_Score,
    Q_RIX = questions$RIX_Score,
    
    # AI回答的指標
    AI_FRE = ai_responses$Flesch_Reading_Ease,
    AI_FK = ai_responses$Flesch_Kincaid_Grade,
    AI_ARI = ai_responses$Automated_Readability_Index,
    AI_CLI = ai_responses$Coleman_Liau_Index,
    AI_GFI = ai_responses$Gunning_Fog_Index,
    AI_SMOG = ai_responses$SMOG_Index,
    AI_LIX = ai_responses$LIX_Score,
    AI_RIX = ai_responses$RIX_Score,

    # 人類回答的指標
    H_FRE = human_responses$Flesch_Reading_Ease,
    H_FK = human_responses$Flesch_Kincaid_Grade,
    H_ARI = human_responses$Automated_Readability_Index,
    H_CLI = human_responses$Coleman_Liau_Index,
    H_GFI = human_responses$Gunning_Fog_Index,
    H_SMOG = human_responses$SMOG_Index,
    H_LIX = human_responses$LIX_Score,
    H_RIX = human_responses$RIX_Score
  )
  
  return(analysis_df)
}

analysis_df <- prepare_paired_data(readability_data)

# ===== 2. 相關性分析 =====
# 創建相關性分析函數
analyze_correlations <- function(data, metric) {
  q_col <- paste0("Q_", metric)
  ai_col <- paste0("AI_", metric)
  h_col <- paste0("H_", metric)
  
  # 計算相關係數
  cor_q_ai <- cor.test(data[[q_col]], data[[ai_col]])
  cor_q_human <- cor.test(data[[q_col]], data[[h_col]])
  
  # 比較相關係數
  comparison <- r.test(
    n = nrow(data),
    r12 = cor(data[[q_col]], data[[ai_col]], use = "complete.obs"),
    r34 = cor(data[[q_col]], data[[h_col]], use = "complete.obs")
  )
  
  return(list(
    ai = cor_q_ai,
    human = cor_q_human,
    comparison = comparison
  ))
}

# 對每個指標進行相關性分析
metrics <- c("FRE", "FK", "ARI", "CLI", "GFI", "SMOG", "LIX", "RIX")
correlation_results <- lapply(metrics, function(m) analyze_correlations(analysis_df, m))
names(correlation_results) <- metrics

# ===== 3. 回歸分析 =====
# 創建回歸分析函數
analyze_regression <- function(data, metric) {
  q_col <- paste0("Q_", metric)
  ai_col <- paste0("AI_", metric)
  h_col <- paste0("H_", metric)
  
  # AI回歸模型
  model_ai <- lm(as.formula(paste(ai_col, "~", q_col)), data = data)
  
  # 人類回歸模型
  model_human <- lm(as.formula(paste(h_col, "~", q_col)), data = data)
  
  return(list(
    ai = model_ai,
    human = model_human
  ))
}

# 對每個指標進行回歸分析
regression_results <- lapply(metrics, function(m) analyze_regression(analysis_df, m))
names(regression_results) <- metrics

# ===== 4. 結果輸出 =====
# 創建結果輸出目錄
dir.create("output/readability_influence", recursive = TRUE, showWarnings = FALSE)

# 輸出相關性分析結果
sink("output/readability_influence/correlation_analysis.txt")
cat("可讀性相關性分析結果\n")
cat("==========================\n\n")

for(metric in metrics) {
  cat(sprintf("\n%s 指標分析:\n", metric))
  cat("-------------\n")
  
  # 輸出AI相關性
  cat(sprintf("問題與AI回答的相關性:\n"))
  print(correlation_results[[metric]]$ai)
  
  # 輸出人類相關性
  cat(sprintf("\n問題與人類回答的相關性:\n"))
  print(correlation_results[[metric]]$human)
  
  # 輸出比較結果
  cat("\n相關係數比較:\n")
  print(correlation_results[[metric]]$comparison)
  cat("\n")
}
sink()

# 輸出回歸分析結果
sink("output/readability_influence/regression_analysis.txt")
cat("可讀性回歸分析結果\n")
cat("==========================\n\n")

for(metric in metrics) {
  cat(sprintf("\n%s 指標分析:\n", metric))
  cat("-------------\n")
  
  # 輸出AI回歸結果
  cat(sprintf("問題對AI回答的影響:\n"))
  print(summary(regression_results[[metric]]$ai))
  
  # 輸出人類回歸結果
  cat(sprintf("\n問題對人類回答的影響:\n"))
  print(summary(regression_results[[metric]]$human))
  cat("\n")
}
sink()

# ===== 5. 視覺化 =====
# 為每個指標創建散點圖
for(metric in metrics) {
  q_col <- paste0("Q_", metric)
  ai_col <- paste0("AI_", metric)
  h_col <- paste0("H_", metric)
  
  p <- ggplot(analysis_df) +
    # AI回答的散點和回歸線
    geom_point(aes(x = .data[[q_col]], y = .data[[ai_col]], color = "AI回答")) +
    geom_smooth(aes(x = .data[[q_col]], y = .data[[ai_col]], color = "AI回答"), 
                method = "lm", se = TRUE) +
    # 人類回答的散點和回歸線
    geom_point(aes(x = .data[[q_col]], y = .data[[h_col]], color = "人類回答")) +
    geom_smooth(aes(x = .data[[q_col]], y = .data[[h_col]], color = "人類回答"), 
                method = "lm", se = TRUE) +
    labs(
      title = sprintf("%s 指標的問題-回答關係", metric),
      x = "問題文本指標值",
      y = "回答文本指標值",
      color = "回答類型"
    ) +
    theme_minimal()
  
  ggsave(
    sprintf("output/readability_influence/%s_scatter.png", metric),
    p,
    width = 10,
    height = 6
  )
}

print("分析完成！結果已保存到 output/readability_influence/ 目錄") 