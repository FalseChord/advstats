# 載入需要的套件
library(tidyverse)
library(readxl)

# 讀取資料
data <- read_excel("data/MIDTERM.xlsx")

# 將性別轉換為因子
data$C1 <- factor(data$C1, 
                  levels = c(0, 1),
                  labels = c("女", "男"))

# 建立t檢定結果格式化函數
format_t_test <- function(t_test, var_name) {
  # 計算描述性統計
  means <- tapply(data[[var_name]], data$C1, mean, na.rm = TRUE)
  sds <- tapply(data[[var_name]], data$C1, sd, na.rm = TRUE)
  
  # 輸出結果
  cat("\n", var_name, "工作壓力指標：\n")
  cat("女性：M =", round(means["女"], 2), ", SD =", round(sds["女"], 2), "\n")
  cat("男性：M =", round(means["男"], 2), ", SD =", round(sds["男"], 2), "\n")
  cat("t(", round(t_test$parameter, 2), ") = ", round(t_test$statistic, 3),
      ", p = ", round(t_test$p.value, 3), "\n")
}

# 執行獨立樣本t檢定
cat("\n性別在工作壓力指標的差異分析\n")
cat("=====================================\n")

# X1 工作壓力
t_test_X1 <- t.test(X1 ~ C1, data = data)
format_t_test(t_test_X1, "X1")

# X2 工作壓力
t_test_X2 <- t.test(X2 ~ C1, data = data)
format_t_test(t_test_X2, "X2")

# X3 工作壓力
t_test_X3 <- t.test(X3 ~ C1, data = data)
format_t_test(t_test_X3, "X3")

# X4 工作壓力
t_test_X4 <- t.test(X4 ~ C1, data = data)
format_t_test(t_test_X4, "X4")

# 檢查常態性假設
cat("\n常態性檢定（Shapiro-Wilk test）：\n")
cat("=====================================\n")
for(x in c("X1", "X2", "X3", "X4")) {
  cat("\n", x, "指標：\n")
  cat("女性：W =", round(shapiro.test(data[[x]][data$C1 == "女"])$statistic, 3),
      ", p =", round(shapiro.test(data[[x]][data$C1 == "女"])$p.value, 3), "\n")
  cat("男性：W =", round(shapiro.test(data[[x]][data$C1 == "男"])$statistic, 3),
      ", p =", round(shapiro.test(data[[x]][data$C1 == "男"])$p.value, 3), "\n")
}
