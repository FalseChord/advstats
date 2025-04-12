# 詞彙多樣性分析腳本
# 分析病人提問與回應的詞彙多樣性特徵

# 載入必要的套件
library(tidyverse)
library(factoextra)
library(corrplot)
library(gridExtra)
library(ggrepel)

# ===== 1. 資料準備 =====
# 讀取資料
data <- read.csv("data/HAI_metrics_diversity.csv")

# 資料預處理
diversity_data <- data %>%
  # 選擇詞彙多樣性指標
  select(Text_Type, TTR, Yule_K, Simpson_D, Herdan_C, Brunet_W) %>%
  # 確保Text_Type為因子型
  mutate(Text_Type = factor(Text_Type))

# ===== 4. PCA分析 =====
# 建立必要的輸出資料夾
if (!dir.exists("output/pca")) {
  dir.create("output/pca")
}
if (!dir.exists("output/pca/results")) {
  dir.create("output/pca/results")
}
if (!dir.exists("output/pca/plots")) {
  dir.create("output/pca/plots")
}

# 準備PCA資料
pca_data <- diversity_data %>%
  select(TTR, Yule_K, Simpson_D, Herdan_C, Brunet_W) %>%
  mutate(across(everything(), function(x) ifelse(is.infinite(x), NA, x)))  # 將無限值轉換為NA

# 在進行PCA之前處理NA值
complete_cases <- complete.cases(pca_data)
pca_data_clean <- scale(pca_data[complete_cases, ])  # 只對完整的案例進行標準化

# 執行PCA
pca_result <- prcomp(pca_data_clean)

# 保存哪些行被使用了
used_rows <- which(complete_cases)

# 儲存PCA結果摘要
sink("output/pca/results/pca_summary.txt")
print(summary(pca_result))
sink()

# 顯示 PCA loadings
loadings <- pca_result$rotation
print("PCA Loadings (Variable Contributions to each PC):")
print(loadings)

# 將 loadings 寫入文件
write.table(loadings, "output/pca/results/pca_loadings.txt", sep="\t", quote=FALSE)

# ===== 5. PCA視覺化 =====
# 碎石圖
p1 <- fviz_eig(pca_result, 
               addlabels = TRUE,
               title = "主成分解釋變異比例") +
  theme_minimal()

# 儲存圖片
ggsave("output/pca/plots/pca_scree.png", p1, width = 8, height = 6)

# ===== 6. 按文本類型分析 =====
# 分組PCA
pca_by_type <- diversity_data %>%
  split(.$Text_Type) %>%
  map(function(df) {
    df %>%
      select(TTR, Yule_K, Simpson_D, Herdan_C, Brunet_W) %>%
      scale() %>%
      prcomp()
  })

# 儲存分組PCA結果
sink("output/pca/results/pca_by_type.txt")
print(map(pca_by_type, summary))
sink()

# ===== 7. 主成分得分分析 =====
# 添加主成分得分到原始資料
pca_scores <- as.data.frame(pca_result$x) %>%
  bind_cols(Text_Type = diversity_data$Text_Type)

# 計算各組的主成分得分摘要
pc_summary <- pca_scores %>%
  group_by(Text_Type) %>%
  summarise(across(starts_with("PC"), 
                   list(mean = mean, sd = sd),
                   .names = "{.col}_{.fn}"))

# 儲存主成分得分摘要
write.csv(pc_summary, "output/pca/results/pc_scores_summary.csv")

# ===== 8. 統計檢驗 =====
# 對主要主成分進行ANOVA
pc1_aov <- aov(PC1 ~ Text_Type, data = pca_scores)
pc2_aov <- aov(PC2 ~ Text_Type, data = pca_scores)

# 儲存ANOVA結果
sink("output/pca/results/anova_results.txt")
print("PC1 ANOVA結果:")
print(summary(pc1_aov))
print("\nPC2 ANOVA結果:")
print(summary(pc2_aov))
sink()

# ===== 9. 生成報告 =====
# 可以使用 R Markdown 生成完整報告
# 包含以上所有分析結果和視覺化

# ===== PCA 結果分析 =====

# 1. 計算每個 PC 的解釋變異比例
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
cumulative_var <- cumsum(var_explained)

# 輸出變異解釋比例
cat("\n各主成分解釋變異比例：\n")
for(i in 1:length(var_explained)) {
    cat(sprintf("PC%d: %.2f%% (累積: %.2f%%)\n", 
                i, var_explained[i], cumulative_var[i]))
}

# 2. 分析變量對主成分的貢獻
loadings <- pca_result$rotation
cat("\n變量對主成分的貢獻：\n")
print(round(loadings, 3))

# 3. 找出每個 PC 最重要的貢獻變量
for(i in 1:ncol(loadings)) {
    cat(sprintf("\nPC%d 主要組成：\n", i))
    # 根據絕對值大小排序
    sorted_loadings <- sort(abs(loadings[,i]), decreasing = TRUE)
    for(var in names(sorted_loadings)) {
        contribution <- loadings[var,i]
        cat(sprintf("%s: %.3f\n", var, contribution))
    }
}

# 4. 儲存分析結果
sink("output/pca/results/pca_detailed_analysis.txt")

cat("=== PCA 詳細分析結果 ===\n\n")

cat("1. 變異解釋比例：\n")
for(i in 1:length(var_explained)) {
    cat(sprintf("PC%d: %.2f%% (累積: %.2f%%)\n", 
                i, var_explained[i], cumulative_var[i]))
}

cat("\n2. 變量載荷矩陣：\n")
print(round(loadings, 3))

cat("\n3. 主成分組成分析：\n")
for(i in 1:ncol(loadings)) {
    cat(sprintf("\nPC%d 主要組成：\n", i))
    sorted_loadings <- sort(abs(loadings[,i]), decreasing = TRUE)
    for(var in names(sorted_loadings)) {
        contribution <- loadings[var,i]
        cat(sprintf("%s: %.3f\n", var, contribution))
    }
}

sink()

# 5. 生成變量貢獻熱圖
library(corrplot)
png("output/pca/plots/pca_loadings_heatmap.png", width = 800, height = 600)
corrplot(loadings, is.corr = FALSE, 
         method = "color",
         addCoef.col = "black",
         tl.col = "black",
         cl.pos = "r",
         title = "PCA Loadings Heatmap")
dev.off()
