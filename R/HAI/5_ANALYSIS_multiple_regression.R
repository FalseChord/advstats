library(tidyverse)
library(car)  # 用於VIF檢測
library(stargazer)  # 用於生成漂亮的回歸表格

# ===== 1. 讀取數據 =====
diversity_data <- read_csv("data/HAI_metrics_diversity.csv")
readability_data <- read_csv("data/HAI_metrics_readability.csv")

# ===== 2. 準備數據 =====
prepare_combined_data <- function(diversity_data, readability_data) {
  # 分別獲取問題、AI回答和人類回答的數據
  # Diversity
  div_questions <- diversity_data %>% filter(Text_Type == "Question")
  div_ai <- diversity_data %>% filter(Text_Type == "AI_Response")
  div_human <- diversity_data %>% filter(Text_Type == "Human_Response")
  
  # Readability
  read_questions <- readability_data %>% filter(Text_Type == "Question")
  read_ai <- readability_data %>% filter(Text_Type == "AI_Response")
  read_human <- readability_data %>% filter(Text_Type == "Human_Response")
  
  # 合併數據
  analysis_df <- data.frame(
    # 問題特徵
    Q_TTR = div_questions$TTR,
    Q_Yule_K = div_questions$Yule_K,
    Q_Simpson_D = div_questions$Simpson_D,
    Q_Herdan_C = div_questions$Herdan_C,
    Q_Brunet_W = div_questions$Brunet_W,
    Q_SMOG = read_questions$SMOG_Index,
    Q_GFI = read_questions$Gunning_Fog_Index,
    Q_LIX = read_questions$LIX_Score,
    # AI回答特徵
    AI_TTR = div_ai$TTR,
    AI_Yule_K = div_ai$Yule_K,
    AI_Simpson_D = div_ai$Simpson_D,
    AI_Herdan_C = div_ai$Herdan_C,
    AI_Brunet_W = div_ai$Brunet_W,
    AI_SMOG = read_ai$SMOG_Index,
    AI_GFI = read_ai$Gunning_Fog_Index,
    AI_LIX = read_ai$LIX_Score,
    # 人類回答特徵
    H_TTR = div_human$TTR,
    H_Yule_K = div_human$Yule_K,
    H_Simpson_D = div_human$Simpson_D,
    H_Herdan_C = div_human$Herdan_C,
    H_Brunet_W = div_human$Brunet_W,
    H_SMOG = read_human$SMOG_Index,
    H_GFI = read_human$Gunning_Fog_Index,
    H_LIX = read_human$LIX_Score
  )
  
  return(analysis_df)
}

analysis_df <- prepare_combined_data(diversity_data, readability_data)

# ===== 3. 多元回歸分析 =====

# 方法一：分類特徵組合分析
run_category_regression <- function(data) {
  # 詞彙多樣性對AI的影響
  ai_diversity_model <- lm(AI_TTR ~ Q_TTR + Q_Yule_K + Q_Simpson_D + Q_Herdan_C + Q_Brunet_W, 
                          data = data)
  
  # 可讀性對AI的影響
  ai_readability_model <- lm(AI_SMOG ~ Q_SMOG + Q_GFI, Q_LIX,
                            data = data)
  
  # 詞彙多樣性對人類的影響
  human_diversity_model <- lm(H_TTR ~ Q_TTR + Q_Yule_K + Q_Simpson_D + Q_Herdan_C + Q_Brunet_W, 
                             data = data)
  
  # 可讀性對人類的影響
  human_readability_model <- lm(H_SMOG ~ Q_SMOG + Q_GFI, Q_LIX,
                               data = data)
  
  return(list(
    ai_diversity = ai_diversity_model,
    ai_readability = ai_readability_model,
    human_diversity = human_diversity_model,
    human_readability = human_readability_model
  ))
}

# 方法二：最佳預測模型分析
run_best_prediction_models <- function(data) {
  # 對AI回答的TTR的預測模型
  ai_ttr_full <- lm(AI_TTR ~ Q_TTR + Q_Yule_K + Q_Simpson_D + Q_Herdan_C + Q_Brunet_W + 
                      Q_SMOG + Q_GFI + Q_LIX, 
                    data = data)
  
  # 對AI回答的SMOG的預測模型
  ai_smog_full <- lm(AI_SMOG ~ Q_TTR + Q_Yule_K + Q_Simpson_D + Q_Herdan_C + Q_Brunet_W + 
                      Q_SMOG + Q_GFI + Q_LIX, 
                    data = data)
  
  # 對人類回答的TTR的預測模型
  human_ttr_full <- lm(H_TTR ~ Q_TTR + Q_Yule_K + Q_Simpson_D + Q_Herdan_C + Q_Brunet_W + 
                         Q_SMOG + Q_GFI + Q_LIX, 
                       data = data)
  
  # 對人類回答的SMOG的預測模型
  human_smog_full <- lm(H_SMOG ~ Q_TTR + Q_Yule_K + Q_Simpson_D + Q_Herdan_C + Q_Brunet_W + 
                         Q_SMOG + Q_GFI + Q_LIX, 
                       data = data)
  
  return(list(
    ai_ttr = ai_ttr_full,
    ai_smog = ai_smog_full,
    human_ttr = human_ttr_full,
    human_smog = human_smog_full
  ))
}

# ===== 4. 執行分析 =====
category_models <- run_category_regression(analysis_df)
prediction_models <- run_best_prediction_models(analysis_df)

# ===== 5. 輸出結果 =====
dir.create("output/multiple_regression", recursive = TRUE, showWarnings = FALSE)

# 輸出分類特徵分析結果
sink("output/multiple_regression/category_analysis.txt")
cat("分類特徵組合分析結果\n")
cat("==========================\n\n")

cat("\nAI回答的詞彙多樣性模型:\n")
print(summary(category_models$ai_diversity))
cat("\nVIF值:\n")
print(vif(category_models$ai_diversity))

cat("\nAI回答的可讀性模型:\n")
print(summary(category_models$ai_readability))
cat("\nVIF值:\n")
print(vif(category_models$ai_readability))

cat("\n人類回答的詞彙多樣性模型:\n")
print(summary(category_models$human_diversity))
cat("\nVIF值:\n")
print(vif(category_models$human_diversity))

cat("\n人類回答的可讀性模型:\n")
print(summary(category_models$human_readability))
cat("\nVIF值:\n")
print(vif(category_models$human_readability))
sink()

# 輸出最佳預測模型分析結果
sink("output/multiple_regression/prediction_analysis.txt")
cat("最佳預測模型分析結果\n")
cat("==========================\n\n")

cat("\nAI回答TTR的完整模型:\n")
print(summary(prediction_models$ai_ttr))
cat("\nVIF值:\n")
print(vif(prediction_models$ai_ttr))

cat("\nAI回答ARI的完整模型:\n")
print(summary(prediction_models$ai_smog))
cat("\nVIF值:\n")
print(vif(prediction_models$ai_smog))

cat("\n人類回答TTR的完整模型:\n")
print(summary(prediction_models$human_ttr))
cat("\nVIF值:\n")
print(vif(prediction_models$human_ttr))

cat("\n人類回答ARI的完整模型:\n")
print(summary(prediction_models$human_smog))
cat("\nVIF值:\n")
print(vif(prediction_models$human_smog))
sink()

print("多元回歸分析完成！結果已保存到 output/multiple_regression/ 目錄")