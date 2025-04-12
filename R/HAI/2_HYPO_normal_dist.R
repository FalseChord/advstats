# 載入必要套件
library(tidyverse)
library(stats)
library(car) # for Levene's test
library(nortest) # for Anderson-Darling normality test
library(moments) # for skewness and kurtosis

# 讀取資料
basic_data <- read.csv("data/HAI_metrics_text.csv")
readability_data <- read.csv("data/HAI_metrics_readability.csv")
lexical_data <- read.csv("data/HAI_metrics_diversity.csv")

# 定義要分析的指標群組
basic_metrics <- c("Word_Count", "Sentence_Count", "Avg_Sentence_Length",
                  "Avg_Word_Length")

readability_metrics <- c("Flesch_Reading_Ease", "Flesch_Kincaid_Grade", 
                        "Automated_Readability_Index", "Coleman_Liau_Index",
                        "Gunning_Fog_Index", "SMOG_Index", "LIX_Score", "RIX_Score")

diversity_metrics <- c("TTR", "Yule_K", "Simpson_D", "Herdan_C",
                      "Brunet_W", "Honore_R")

# 建立分析函數
analyze_metric <- function(data, metric) {
  # 基本統計量
  basic_stats <- data %>%
    group_by(Text_Type) %>%
    summarise(
      n = n(),
      Mean = mean(!!sym(metric), na.rm = TRUE),
      SD = sd(!!sym(metric), na.rm = TRUE),
      Median = median(!!sym(metric), na.rm = TRUE),
      Q1 = quantile(!!sym(metric), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(metric), 0.75, na.rm = TRUE),
      Skewness = moments::skewness(!!sym(metric), na.rm = TRUE),
      Kurtosis = moments::kurtosis(!!sym(metric), na.rm = TRUE)
    )
  
  # Kolmogorov-Smirnov 檢定
  ks_tests <- data %>%
    group_by(Text_Type) %>%
    summarise(
      KS_p = tryCatch({
        clean_data <- pull(data, !!sym(metric))
        clean_data <- clean_data[!is.na(clean_data) & !is.infinite(clean_data)]
        ks.test(clean_data, "pnorm", 
                mean = mean(clean_data), 
                sd = sd(clean_data))$p.value
      }, error = function(e) NA)
    )
  
  # 繪製 QQ 圖
  qq_plot <- ggplot(data, aes(sample = !!sym(metric))) +
    geom_qq() +
    geom_qq_line() +
    facet_wrap(~Text_Type) +
    labs(title = paste("Normal Q-Q Plot of", metric),
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  # 儲存 QQ 圖
  dir.create("output/plots/qq_plots", showWarnings = FALSE)
  ggsave(
    paste0("output/plots/qq_plots/", metric, "_qq_plot.png"),
    qq_plot,
    width = 10,
    height = 6
  )
  
  # 合併結果
  results <- list(
    basic_stats = basic_stats,
    ks_tests = ks_tests
  )
  
  return(results)
}

# 新增解釋函數
interpret_normality <- function(data, metric_name, text_type) {
    # 取得乾淨的資料
    clean_data <- data[[metric_name]][!is.na(data[[metric_name]]) & !is.infinite(data[[metric_name]])]
    n_samples <- length(clean_data)
    
    # 如果沒有足夠的數據，返回提示訊息
    if(n_samples < 3) {
        cat(sprintf("\n文本類型：%s - 樣本數不足，無法進行常態性分析\n", text_type))
        return()
    }
    
    # 基本統計量
    skew <- tryCatch(
        moments::skewness(clean_data),
        error = function(e) NA
    )
    kurt <- tryCatch(
        moments::kurtosis(clean_data),
        error = function(e) NA
    )
    
    cat(sprintf("\n文本類型：%s （樣本數：%d）\n", text_type, n_samples))
    
    # 常態性評估
    cat("常態性評估：\n")
    
    # 1. 形狀描述
    cat("1. 分配形狀：\n")
    if(is.na(skew)) {
        cat("   - 無法計算偏度\n")
    } else if(abs(skew) < 0.5) {
        cat("   - 對稱性良好（偏度 = ", round(skew, 2), "）\n")
    } else {
        cat("   - 有", ifelse(skew > 0, "右", "左"), "偏趨勢（偏度 = ", round(skew, 2), "）\n")
    }
    
    if(is.na(kurt)) {
        cat("   - 無法計算峰度\n")
    } else if(abs(kurt - 3) < 0.5) {
        cat("   - 峰度接近常態分配（峰度 = ", round(kurt, 2), "）\n")
    } else {
        cat("   - 分配", ifelse(kurt > 3, "較陡峭", "較平坦"), "（峰度 = ", round(kurt, 2), "）\n")
    }
    
    # 2. Kolmogorov-Smirnov 檢定結果
    cat("2. 統計檢定：\n")
    ks_test <- tryCatch(
        ks.test(clean_data, "pnorm", 
                mean = mean(clean_data), 
                sd = sd(clean_data)),
        error = function(e) NULL
    )
    
    if(!is.null(ks_test)) {
        cat(sprintf("   - Kolmogorov-Smirnov 檢定：p = %.4g\n", ks_test$p.value))
    } else {
        cat("   - Kolmogorov-Smirnov 檢定無法執行\n")
    }
    
    # 3. 綜合結論
    cat("3. 綜合結論：\n")
    
    # 計算 QQ 圖斜率
    tryCatch({
        qq_data <- qqnorm(clean_data, plot.it = FALSE)
        qq_fit <- lm(qq_data$y ~ qq_data$x)
        qq_slope <- coef(qq_fit)[2]
        qq_r2 <- summary(qq_fit)$r.squared
        
        cat("   QQ圖評估：\n")
        cat(sprintf("   - 斜率 = %.2f（理想值為1）\n", qq_slope))
        cat(sprintf("   - R² = %.3f（越接近1越好）\n", qq_r2))
        
        # 綜合形狀指標、KS 檢定結果和 QQ 圖斜率
        shape_normal <- !is.na(skew) && !is.na(kurt) && abs(skew) < 0.5 && abs(kurt - 3) < 0.5
        shape_acceptable <- !is.na(skew) && !is.na(kurt) && abs(skew) < 1 && abs(kurt - 3) < 1
        ks_normal <- !is.null(ks_test) && ks_test$p.value > 0.05
        qq_good <- abs(qq_slope - 1) < 0.1 && qq_r2 > 0.95
        qq_acceptable <- abs(qq_slope - 1) < 0.2 && qq_r2 > 0.90
        
        if(shape_normal && ks_normal && qq_good) {
            cat("   實務上可視為常態分配\n",
                "   （形狀指標良好，KS 檢定 p > 0.05，QQ圖表現優異）\n")
        } else if(shape_acceptable && ks_normal && qq_acceptable) {
            cat("   近似常態分配，偏離程度可接受\n",
                "   （形狀指標尚可，KS 檢定 p > 0.05，QQ圖表現尚可）\n")
        } else if((shape_acceptable || qq_acceptable) && !ks_normal) {
            cat("   形狀接近常態但統計檢定不支持\n",
                "   （KS 檢定 p ≤ 0.05，QQ圖表現", 
                ifelse(qq_acceptable, "尚可", "不佳"), "）\n",
                "   建議視情況使用參數或非參數方法\n")
        } else {
            cat("   明顯偏離常態分配\n",
                "   （形狀指標不佳",
                ifelse(!ks_normal, "，KS 檢定 p ≤ 0.05", ""),
                "，QQ圖表現不佳）\n",
                "   建議使用非參數方法\n")
        }
    }, error = function(e) {
        cat("   無法完成QQ圖分析\n")
        cat("   建議使用非參數方法\n")
    })
}

# 分析並輸出結果的函數
analyze_and_output <- function(data, metrics, group_name) {
  # 建立結果資料夾
  dir.create("output", showWarnings = FALSE)
  dir.create("output/plots", showWarnings = FALSE)
  
  # 對每個指標進行分析
  results_list <- lapply(metrics, function(metric) {
    results <- analyze_metric(data, metric)
    return(results)
  })
  
  # 輸出摘要報告
  sink(paste0("output/", group_name, "_normal_dist_analysis_summary.txt"))
  cat(paste(group_name, "分析摘要\n"))
  cat(rep("=", nchar(group_name) + 16), "\n\n")
  
  for(i in seq_along(metrics)) {
    cat("\n", gsub("_", " ", metrics[i]), "\n")
    cat(rep("-", nchar(metrics[i])), "\n")
    
    # 基本統計量
    print(results_list[[i]]$basic_stats)
    
    # 對每個文本類型進行詳細的常態性評估
    for(text_type in unique(data$Text_Type)) {
        subset_data <- data[data$Text_Type == text_type, ]
        interpret_normality(subset_data, metrics[i], text_type)
    }
    cat("\n")
  }
  
  sink()
  
  # 計算並繪製相關性矩陣
  cor_matrix <- cor(data[metrics], use = "complete.obs")
  cor_plot <- ggplot(data = reshape2::melt(cor_matrix), 
                     aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                        midpoint = 0, limit = c(-1,1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste(group_name, "Correlation Matrix"))
  
  ggsave(paste0("output/plots/", group_name, "_correlation_matrix.png"), cor_plot)
  
  return(results_list)
}

# 執行所有分析
basic_results <- analyze_and_output(basic_data, basic_metrics, "basic")
readability_results <- analyze_and_output(readability_data, readability_metrics, "readability")
diversity_results <- analyze_and_output(lexical_data, diversity_metrics, "diversity")

sink()
