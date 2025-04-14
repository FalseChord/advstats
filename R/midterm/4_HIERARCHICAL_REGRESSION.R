# 載入需要的套件
library(tidyverse)
library(readxl)

# 讀取資料
data <- read_excel("data/MIDTERM.xlsx")

# 建立階層迴歸分析函數
hierarchical_regression <- function(dv_name) {
  # Block 1: 工作壓力指標
  model1 <- lm(paste(dv_name, "~ X1 + X2 + X3 + X4"), data = data)
  
  # Block 2: 加入人際支持指標
  model2 <- lm(paste(dv_name, "~ X1 + X2 + X3 + X4 + Z1 + Z2 + Z3"), data = data)
  
  # 計算 R² 變化量
  r2_change <- summary(model2)$r.squared - summary(model1)$r.squared
  
  # F 檢定比較兩個模型
  f_test <- anova(model1, model2)
  
  # 輸出結果
  cat("\n", dv_name, "的階層迴歸分析結果：\n")
  cat("==========================================\n")
  
  cat("\nBlock 1 (工作壓力指標)：\n")
  cat("R² =", round(summary(model1)$r.squared, 3), "\n")
  cat("Adjusted R² =", round(summary(model1)$adj.r.squared, 3), "\n")
  cat("\n係數：\n")
  print(round(summary(model1)$coefficients, 3))
  
  cat("\nBlock 2 (加入人際支持指標)：\n")
  cat("R² =", round(summary(model2)$r.squared, 3), "\n")
  cat("Adjusted R² =", round(summary(model2)$adj.r.squared, 3), "\n")
  cat("R² 變化量 =", round(r2_change, 3), "\n")
  cat("\n係數：\n")
  print(round(summary(model2)$coefficients, 3))
  
  cat("\n模型比較（ANOVA）：\n")
  print(f_test)
  
  # 回傳模型以供後續使用
  return(list(model1 = model1, model2 = model2))
}

# 執行三個依變項的階層迴歸分析
results_Y1 <- hierarchical_regression("Y1")
results_Y2 <- hierarchical_regression("Y2")
results_Y3 <- hierarchical_regression("Y3")

# 檢查多重共線性
check_vif <- function(model) {
  vif_values <- car::vif(model)
  return(vif_values)
}

# 輸出 VIF 值
cat("\n多重共線性檢查 (VIF)：\n")
cat("\nY1 最終模型的 VIF：\n")
print(round(check_vif(results_Y1$model2), 3))
cat("\nY2 最終模型的 VIF：\n")
print(round(check_vif(results_Y2$model2), 3))
cat("\nY3 最終模型的 VIF：\n")
print(round(check_vif(results_Y3$model2), 3))

# 檢查殘差
check_residuals <- function(model, dv_name) {
  # 建立診斷圖
  par(mfrow = c(2,2))
  plot(model)
  par(mfrow = c(1,1))
}

# 檢查每個模型的殘差
cat("\n殘差診斷圖：\n")
check_residuals(results_Y1$model2, "Y1")
check_residuals(results_Y2$model2, "Y2")
check_residuals(results_Y3$model2, "Y3") 