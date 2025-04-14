# 載入需要的套件
library(tidyverse)
library(readxl)
library(car)  # 用於檢驗變異數同質性

# 讀取資料
data <- read_excel("data/MIDTERM.xlsx")

# 將類別變項轉換為因子
data$C1 <- factor(data$C1, 
                  levels = c(0, 1),
                  labels = c("女", "男"))
data$C4 <- factor(data$C4, 
                  levels = c(0, 1),
                  labels = c("無婚姻", "有婚姻"))

# 建立描述性統計
desc_stats <- data %>%
  group_by(C1, C4) %>%
  summarise(
    n = n(),
    across(c(Z1, Z2, Z3), 
           list(
             mean = ~mean(., na.rm = TRUE),
             sd = ~sd(., na.rm = TRUE)
           ),
           .names = "{.col}_{.fn}")
  )

# 執行雙因子變異數分析
aov_Z1 <- aov(Z1 ~ C1 * C4, data = data)
aov_Z2 <- aov(Z2 ~ C1 * C4, data = data)
aov_Z3 <- aov(Z3 ~ C1 * C4, data = data)

# 輸出結果
cat("\n性別與婚姻狀況對人際支持的影響分析\n")
cat("==========================================\n")

# 描述性統計
cat("\n描述性統計：\n")
print(desc_stats)

# ANOVA 結果
cat("\nZ1 人際支持指標的變異數分析：\n")
print(summary(aov_Z1))

cat("\nZ2 人際支持指標的變異數分析：\n")
print(summary(aov_Z2))

cat("\nZ3 人際支持指標的變異數分析：\n")
print(summary(aov_Z3))

# 檢驗假設
# 變異數同質性檢定
cat("\n變異數同質性檢定（Levene's test）：\n")
cat("Z1:", leveneTest(Z1 ~ C1 * C4, data = data)$`Pr(>F)`[1], "\n")
cat("Z2:", leveneTest(Z2 ~ C1 * C4, data = data)$`Pr(>F)`[1], "\n")
cat("Z3:", leveneTest(Z3 ~ C1 * C4, data = data)$`Pr(>F)`[1], "\n")

# 修改簡單主效果分析函數
simple_effects <- function(dv_name, data) {
  # 男性組的分析
  male_data <- data[data$C1 == "男", ]
  male_test <- t.test(get(dv_name) ~ C4, data = male_data)
  
  # 女性組的分析
  female_data <- data[data$C1 == "女", ]
  female_test <- t.test(get(dv_name) ~ C4, data = female_data)
  
  cat("\n男性組中婚姻狀況的效果：\n")
  print(male_test)
  cat("\n女性組中婚姻狀況的效果：\n")
  print(female_test)
}

# 如果有顯著交互作用，則執行簡單主效果分析
if(summary(aov_Z1)[[1]]$`Pr(>F)`[3] < 0.05) {
  cat("\nZ1的簡單主效果分析：\n")
  simple_effects("Z1", data)
}

if(summary(aov_Z2)[[1]]$`Pr(>F)`[3] < 0.05) {
  cat("\nZ2的簡單主效果分析：\n")
  simple_effects("Z2", data)
}

if(summary(aov_Z3)[[1]]$`Pr(>F)`[3] < 0.05) {
  cat("\nZ3的簡單主效果分析：\n")
  simple_effects("Z3", data)
}

# 繪製交互作用圖
library(ggplot2)

plot_interaction <- function(dv, dv_name) {
  ggplot(data, aes(x = C1, y = .data[[dv]], color = C4, group = C4)) +
    stat_summary(fun = mean, geom = "point") +
    stat_summary(fun = mean, geom = "line") +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
    labs(title = paste(dv_name, "的性別與婚姻狀況交互作用"),
         x = "性別",
         y = dv_name) +
    theme_minimal()
}

# 儲存交互作用圖
plots <- list(
  Z1 = plot_interaction("Z1", "Z1人際支持"),
  Z2 = plot_interaction("Z2", "Z2人際支持"),
  Z3 = plot_interaction("Z3", "Z3人際支持")
) 