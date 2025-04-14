# 載入需要的套件
library(tidyverse)
library(readxl)
library(car)  # 用於 Levene's test

# 讀取資料
data <- read_excel("data/MIDTERM.xlsx")

# 將工作年資轉換為因子
data$C3 <- factor(data$C3, 
                  levels = 1:3,
                  labels = c("1-10年", "11-20年", "21年以上"))

# 建立單因子變異數分析函數
analyze_variance <- function(dv_name) {
  # 描述性統計
  desc_stats <- data %>%
    group_by(C3) %>%
    summarise(
      n = n(),
      mean = mean(get(dv_name), na.rm = TRUE),
      sd = sd(get(dv_name), na.rm = TRUE)
    )
  
  # Levene's test
  levene_test <- leveneTest(get(dv_name) ~ C3, data = data)
  
  # ANOVA
  aov_result <- aov(get(dv_name) ~ C3, data = data)
  
  # 輸出結果
  cat("\n", dv_name, "職業倦怠指標：\n")
  cat("=====================================\n")
  
  # 描述性統計
  cat("\n描述性統計：\n")
  print(desc_stats)
  
  # Levene's test
  cat("\nLevene's test：\n")
  cat("F(", levene_test$Df[1], ",", levene_test$Df[2], ") = ", 
      round(levene_test$`F value`[1], 3), 
      ", p = ", round(levene_test$`Pr(>F)`[1], 3), "\n")
  
  # ANOVA結果
  cat("\nANOVA結果：\n")
  print(summary(aov_result))
  
  # 如果ANOVA顯著，進行事後檢定
  if(summary(aov_result)[[1]]$`Pr(>F)`[1] < 0.05) {
    # Tukey HSD
    tukey_result <- TukeyHSD(aov_result)
    cat("\nTukey HSD事後檢定：\n")
    print(tukey_result)
    
    # Scheffe
    scheffe_result <- scheffe.test(aov_result, "C3")
    cat("\nScheffé事後檢定：\n")
    print(scheffe_result$comparison)
  }
  
  return(aov_result)
}

# 執行分析
results_Y1 <- analyze_variance("Y1")
results_Y2 <- analyze_variance("Y2")
results_Y3 <- analyze_variance("Y3")