# 載入必要的套件
library(tidyverse)
library(stats)
library(effectsize)  # 用於計算效果量
library(car)  # 用於 Levene's test

# 讀取資料
data <- read.csv("data/AI.csv")

# 移除遺漏值後再進行分析
data_complete <- na.omit(data[c("pre.MHL_Total", "post.MHL_Total")])

# 準備進行 Levene's test 的資料
data_long <- data.frame(
  Score = c(data_complete$pre.MHL_Total, data_complete$post.MHL_Total),
  Time = factor(rep(c("Pre-test", "Post-test"), each = nrow(data_complete)))
)

# 進行 Levene's test
levene_result <- leveneTest(Score ~ Time, data = data_long)

# 進行配對樣本t檢定
t_result <- t.test(data_complete$post.MHL_Total - data_complete$pre.MHL_Total,
                   mu = 0,
                   alternative = "greater")

# 計算效果量 (Cohen's d)
mean_diff <- mean(data_complete$post.MHL_Total - data_complete$pre.MHL_Total)
sd_diff <- sd(data_complete$post.MHL_Total - data_complete$pre.MHL_Total)
cohens_d <- mean_diff / sd_diff

# 計算描述性統計
descriptive_stats <- data.frame(
  Time = c("Pre-test", "Post-test"),
  Mean = c(mean(data_complete$pre.MHL_Total),
           mean(data_complete$post.MHL_Total)),
  SD = c(sd(data_complete$pre.MHL_Total),
         sd(data_complete$post.MHL_Total))
)

# 輸出結果
cat("\n=== 心理健康素養介入成效分析 ===\n")

cat("\n變異數同質性檢定結果：\n")
print(levene_result)
cat("\n註：若 p > .05，表示符合變異數同質性假設\n")

cat("\n描述性統計：\n")
print(descriptive_stats)

cat("\n配對樣本t檢定結果：\n")
cat("t值 =", round(t_result$statistic, 3), "\n")
cat("自由度 =", t_result$parameter, "\n")
cat("p值 =", round(t_result$p.value, 4), "\n")
cat("效果量 (Cohen's d) =", round(cohens_d, 3), "\n")

# 繪製箱型圖比較前後測差異
ggplot(data_long, aes(x = Time, y = Score, fill = Time)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "心理健康素養前後測分數比較",
       x = "測驗時間",
       y = "心理健康素養總分") +
  theme(legend.position = "none")

# 儲存圖片
ggsave("MHL_comparison.png", width = 8, height = 6)