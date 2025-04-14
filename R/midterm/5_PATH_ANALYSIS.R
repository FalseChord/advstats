library(tidyverse)
library(lavaan)    # 用於路徑分析
library(semPlot)   # 用於繪製路徑圖
library(readxl)    # 用於讀取 xlsx 檔案
library(psych)     # 用於描述性統計

# ===== 1. 數據準備 =====
# 讀取 xlsx 檔案
data <- read_excel("data/MIDTERM.xlsx")

# 計算總分
data <- data %>%
  mutate(
    X_total = X1 + X2 + X3 + X4,        # 工作壓力總分
    Y_total = Y1 + Y2 + Y3,             # 職業倦怠總分
    Z_total = Z1 + Z2 + Z3              # 人際支持總分
  )

# ===== 2. 路徑分析 =====
# 定義模型
model <- '
  # 路徑關係
  Z_total ~ a*X_total      # X -> Z
  Y_total ~ b*Z_total      # Z -> Y
  Y_total ~ c*X_total      # X -> Y (直接效果)
  
  # 定義間接效果
  indirect := a*b          # X -> Z -> Y (間接效果)
  
  # 定義總效果
  total := c + (a*b)      # 總效果 = 直接效果 + 間接效果
'

# 執行路徑分析
fit <- sem(model, data = data)

# ===== 3. 結果輸出 =====
# 創建輸出目錄
dir.create("output/midterm/path_analysis", recursive = TRUE, showWarnings = FALSE)

# 輸出基本摘要統計
sink("output/midterm/path_analysis/descriptive_stats.txt")
cat("描述性統計\n")
cat("====================\n\n")
print(describe(data[c("X_total", "Y_total", "Z_total")]))
sink()

# 輸出路徑分析結果
sink("output/midterm/path_analysis/path_analysis_results.txt")
cat("路徑分析結果\n")
cat("====================\n\n")

# 模型適配度
cat("模型適配度指標：\n")
print(fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")))
cat("\n")

# 路徑係數
cat("路徑係數：\n")
print(standardizedSolution(fit))
cat("\n")

# 效果分解
cat("效果分解：\n")
print(parameterEstimates(fit))
sink()

# ===== 4. 視覺化 =====
# 繪製路徑圖
pdf("output/midterm/path_analysis/path_diagram.pdf")
semPaths(fit, 
         whatLabels = "std", 
         layout = "tree",
         edge.label.cex = 1.2,
         sizeMan = 10,
         edge.color = "black",
         edge.width = 1.5)
dev.off()

# ===== 5. Bootstrap 分析 =====
# 執行 Bootstrap 以獲得間接效果的信賴區間
boot_fit <- sem(model, data = data, se = "bootstrap", bootstrap = 5000)

sink("output/midterm/path_analysis/bootstrap_results.txt")
cat("Bootstrap 分析結果 (95% 信賴區間)\n")
cat("================================\n\n")
print(parameterEstimates(boot_fit, boot.ci.type = "bca.simple"))
sink()

# ===== 6. 效果量計算 =====
sink("output/midterm/path_analysis/effect_size.txt")
cat("效果量分析\n")
cat("====================\n\n")

# 計算 VAF (Variance Accounted For)
standardized <- standardizedSolution(fit)
indirect_effect <- standardized$est.std[standardized$label == "indirect"]
total_effect <- standardized$est.std[standardized$label == "total"]
VAF <- indirect_effect / total_effect

cat("中介效果比例 (VAF):", round(VAF * 100, 2), "%\n")

# R-square
cat("\nR-square 值：\n")
print(inspect(fit, "r2"))
sink()

print("路徑分析完成！結果已保存到 output/midterm/path_analysis/ 目錄")
