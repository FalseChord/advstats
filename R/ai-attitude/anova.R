# 載入必要的套件
library(tidyverse)
library(stats)
library(car)  # 用於 Levene's test
library(rstatix)  # 用於事後比較

# 讀取資料
data <- read.csv("data/AI.csv")

# 轉換為長格式數據
data_long <- data %>%
  select(Gender, GPT_Use, pre.MHL_Total, post.MHL_Total) %>%
  pivot_longer(
    cols = c(pre.MHL_Total, post.MHL_Total),
    names_to = "Time",
    values_to = "MHL_Total"
  ) %>%
  mutate(
    Time = factor(Time, 
                 levels = c("pre.MHL_Total", "post.MHL_Total"),
                 labels = c("Pre-test", "Post-test")),
    GPT_Use = factor(GPT_Use, 
                     levels = c("Never", 
                              "Occasionally (once or twice)",
                              "Sometimes (once a week)", 
                              "Always (daily)"))
  )

# 描述性統計
descriptive_stats <- data_long %>%
  group_by(Time, GPT_Use, Gender) %>%
  summarise(
    n = n(),
    mean = mean(MHL_Total, na.rm = TRUE),
    sd = sd(MHL_Total, na.rm = TRUE),
    se = sd/sqrt(n)
  )

# Levene's test
levene_result <- leveneTest(MHL_Total ~ Time * GPT_Use * Gender, data = data_long)

# 進行三因子 ANOVA
anova_result <- aov(MHL_Total ~ GPT_Use * Gender * Time, data = data_long)
anova_summary <- summary(anova_result)

# 輸出結果
cat("\n=== GPT使用程度、性別與前後測對心理健康素養的影響分析 ===\n")

cat("\n描述性統計：\n")
print(descriptive_stats)

cat("\n變異數同質性檢定結果：\n")
print(levene_result)
cat("\n註：若 p > .05，表示符合變異數同質性假設\n")

cat("\n=== 三因子 ANOVA 統計結果 ===\n\n")
print(anova_summary)

# 計算效果量 (Partial Eta Squared)
anova_table <- anova_summary[[1]]
effects_names <- rownames(anova_table)
effects_names <- effects_names[-length(effects_names)]

for(effect_name in effects_names) {
  effect_row <- anova_table[effect_name, ]
  error_row <- anova_table["Residuals", ]
  
  # 計算偏 eta 平方
  ss_effect <- as.numeric(effect_row["Sum Sq"])
  ss_error <- as.numeric(error_row["Sum Sq"])
  pes <- ss_effect / (ss_effect + ss_error)
  
  # 輸出結果，使用 as.numeric() 確保獲取純數值
  cat(sprintf("\n%s：", effect_name))
  cat(sprintf("\nF(%d, %d) = %.2f, p = %.4f, partial η² = %.3f", 
              as.numeric(effect_row["Df"]),
              as.numeric(error_row["Df"]),
              as.numeric(effect_row["F value"]),
              as.numeric(effect_row["Pr(>F)"]),
              pes))
  
  if(as.numeric(effect_row["Pr(>F)"]) < 0.05) {
    cat("\n結果解讀：達到統計顯著水準 (p < .05)")
    if(grepl(":", effect_name)) {
      cat("，表示存在顯著的交互作用。\n")
    } else {
      cat("，表示存在顯著的主要效果。\n")
    }
  } else {
    cat("\n結果解讀：未達統計顯著水準 (p > .05)")
    if(grepl(":", effect_name)) {
      cat("，表示不存在顯著的交互作用。\n")
    } else {
      cat("，表示不存在顯著的主要效果。\n")
    }
  }
}

# 繪製交互作用圖
interaction_plot <- ggplot(data_long, 
                          aes(x = Time, y = MHL_Total, 
                              color = GPT_Use, group = GPT_Use)) +
  facet_wrap(~Gender) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  theme_minimal() +
  labs(title = "GPT使用程度、性別與前後測對心理健康素養的交互作用",
       x = "測驗時間",
       y = "心理健康素養總分") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 儲存交互作用圖
ggsave("GPT_Gender_Time_interaction.png", interaction_plot, width = 12, height = 6)