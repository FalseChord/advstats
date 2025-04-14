# 載入需要的套件
library(tidyverse)
library(readxl)

# 讀取資料
data <- read_excel("data/MIDTERM.xlsx")

# 將類別變項轉換為因子並加上適當標籤
data$C1 <- factor(data$C1, 
                  levels = c(0, 1),
                  labels = c("女", "男"))

data$C2 <- factor(data$C2, 
                  levels = 1:3,
                  labels = c("20-39歲", "40-49歲", "50-65歲"))

data$C3 <- factor(data$C3, 
                  levels = 1:3,
                  labels = c("1-10年", "11-20年", "21年以上"))

data$C4 <- factor(data$C4, 
                  levels = c(0, 1),
                  labels = c("無婚姻", "有婚姻"))

# 輸出結果
cat("\n樣本基本資料分布\n")
cat("=====================================\n\n")

# 1. 性別分布
cat("1. 性別分布：\n")
cat("   女性：", table(data$C1)["女"], "\n")
cat("   男性：", table(data$C1)["男"], "\n\n")

# 2. 年齡層分布
cat("2. 年齡層分布：\n")
cat("   20-39歲：", table(data$C2)["20-39歲"], "\n")
cat("   40-49歲：", table(data$C2)["40-49歲"], "\n")
cat("   50-65歲：", table(data$C2)["50-65歲"], "\n\n")

# 3. 工作年資分布
cat("3. 工作年資分布：\n")
cat("   1-10年：", table(data$C3)["1-10年"], "\n")
cat("   11-20年：", table(data$C3)["11-20年"], "\n")
cat("   21年以上：", table(data$C3)["21年以上"], "\n\n")

# 4. 婚姻狀況分布
cat("4. 婚姻狀況分布：\n")
cat("   無婚姻：", table(data$C4)["無婚姻"], "\n")
cat("   有婚姻：", table(data$C4)["有婚姻"], "\n\n")

# 5. 性別與婚姻狀況交叉分析
marriage_gender_table <- table(data$C4, data$C1)
cat("5. 性別與婚姻狀況交叉分析：\n\n")
cat("                    女性          男性          總計\n")
cat("   無婚姻：", marriage_gender_table["無婚姻", "女"], 
    "           ", marriage_gender_table["無婚姻", "男"],
    "           ", margin.table(marriage_gender_table, 1)["無婚姻"], "\n")
cat("   有婚姻：", marriage_gender_table["有婚姻", "女"],
    "           ", marriage_gender_table["有婚姻", "男"],
    "           ", margin.table(marriage_gender_table, 1)["有婚姻"], "\n")
cat("   總計：  ", margin.table(marriage_gender_table, 2)["女"],
    "           ", margin.table(marriage_gender_table, 2)["男"],
    "           ", sum(marriage_gender_table), "\n\n")

# 6. 特定問題回答
married_male <- marriage_gender_table["有婚姻", "男"]
cat("6. 特定問題回答：\n")
cat("   在有婚姻狀況的樣本中，共有", married_male, "位男性。\n")