# ===== 1. 載入必要的套件 =====
library(tidyverse)
library(factoextra)
library(corrplot)
library(gridExtra)
library(ggrepel)

# ===== 2. 讀取資料 =====
readability_data <- read.csv("data/HAI_metrics_readability.csv")

# ===== 3. 建立必要的輸出資料夾 =====
if (!dir.exists("output")) {
  dir.create("output")
}
if (!dir.exists("output/pca")) {
  dir.create("output/pca")
}
if (!dir.exists("output/pca/results")) {
  dir.create("output/pca/results")
}
if (!dir.exists("output/pca/plots")) {
  dir.create("output/pca/plots")
}

# ===== 4. Readability PCA 分析 =====
# 準備 Readability PCA 資料
readability_pca_data <- readability_data %>%
  select(Flesch_Reading_Ease, Flesch_Kincaid_Grade, 
         Automated_Readability_Index, Coleman_Liau_Index, 
         Gunning_Fog_Index, SMOG_Index, LIX_Score, RIX_Score) %>%
  scale()

# 執行 PCA
readability_pca_result <- prcomp(readability_pca_data)

# 顯示摘要
readability_pca_summary <- summary(readability_pca_result)
print("Readability PCA Summary:")
print(readability_pca_summary)

# 將摘要寫入文件
capture.output(readability_pca_summary, 
              file="output/pca/results/readability_pca_summary.txt")

# 分析 PC1 和 PC2 的組成
readability_loadings <- readability_pca_result$rotation
readability_pc_composition <- data.frame(
  Variable = rownames(readability_loadings),
  PC1 = readability_loadings[,1],
  PC2 = readability_loadings[,2]
)

# 排序後的結果（依據絕對值大小）
readability_pc1_sorted <- readability_pc_composition[
  order(abs(readability_pc_composition$PC1), decreasing = TRUE),]
readability_pc2_sorted <- readability_pc_composition[
  order(abs(readability_pc_composition$PC2), decreasing = TRUE),]

# 將結果寫入文件
write.table(readability_pc1_sorted, 
            "output/pca/results/readability_pc1_composition.txt", 
            sep="\t", row.names=FALSE, quote=FALSE)
write.table(readability_pc2_sorted, 
            "output/pca/results/readability_pc2_composition.txt", 
            sep="\t", row.names=FALSE, quote=FALSE)

# 計算特徵值和變異解釋量
readability_eigenvalues <- readability_pca_result$sdev^2
readability_var_explained <- readability_eigenvalues/sum(readability_eigenvalues)
readability_cum_var_explained <- cumsum(readability_var_explained)

# 製作決策輔助表格
readability_decision_table <- data.frame(
  PC = paste0("PC", 1:length(readability_eigenvalues)),
  Eigenvalue = readability_eigenvalues,
  Variance_Explained = readability_var_explained * 100,
  Cumulative_Variance = readability_cum_var_explained * 100
)

# 輸出決策表格
write.table(readability_decision_table, 
            "output/pca/results/readability_pca_decision_metrics.txt", 
            sep="\t", row.names=FALSE, quote=FALSE)

# 繪製碎石圖
png("output/pca/plots/readability_scree_plot.png", width=800, height=600)
plot(readability_eigenvalues, type="b", main="Readability Scree Plot",
     xlab="Principal Component", ylab="Eigenvalue",
     pch=19)
abline(h=1, col="red", lty=2)  # Kaiser 準則參考線
dev.off()

# 打印主要貢獻變數（載荷量絕對值 > 0.3）
cat("\nPC1 主要貢獻變數（|loading| > 0.3）：\n")
print(readability_pc1_sorted[abs(readability_pc1_sorted$PC1) > 0.3, c("Variable", "PC1")])

cat("\nPC2 主要貢獻變數（|loading| > 0.3）：\n")
print(readability_pc2_sorted[abs(readability_pc2_sorted$PC2) > 0.3, c("Variable", "PC2")])
