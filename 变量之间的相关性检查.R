required_packages <- c("tidyverse", "broom", "car", "stargazer", 
                       "ggplot2", "corrplot", "lmtest", "sandwich", 
                       "ggpubr", "performance")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 加载所有包
library(tidyverse)      # 数据处理和可视化
library(broom)          # 整理回归结果
library(car)            # 回归诊断
library(stargazer)      # 制作专业回归表格
library(ggplot2)        # 绘图
library(corrplot)       # 相关性可视化
library(lmtest)         # 异方差检验
library(sandwich)       # 稳健标准误
library(ggpubr)         # 组合图形
library(performance)    # 模型性能评估

# -------------------------------
# 2. 读取和检查数据
# -------------------------------
data <- read.csv("./final_analysis_test.csv", stringsAsFactors = FALSE)

# 检查数据结构和变量
cat("数据维度（行数, 列数）:", dim(data), "\n\n")
cat("变量名称:\n")
print(names(data))
cat("\n")



# 查看数据概况
cat("数据时间范围:", min(data$date), "至", max(data$date), "\n")
cat("总交易日数:", nrow(data), "\n\n")

# 查看前几行数据
cat("数据前6行预览:\n")
print(head(data, 6))

# 查看数据结构
cat("\n数据结构:\n")
str(data)

# -------------------------------
# 3. 数据清洗和准备
# -------------------------------

# 3.1 检查事件变量的分布
cat("\n=== 事件变量分布 ===\n")
event_vars <- c("phone_pos", "phone_neg", "car_pos", "car_neg", "iot_pos", "iot_neg")

event_summary <- data_clean %>%
  select(all_of(event_vars)) %>%
  summarise_all(list(
    发生天数 = ~sum(. == 1),
    比例 = ~round(mean(. == 1) * 100, 2)
  ))

print(event_summary)



# -------------------------------
# 4. 描述性统计
# -------------------------------
cat("\n=== 描述性统计 ===\n")

# 数值型变量的描述性统计
numeric_vars <- c("return_xm", "ma_gap", "rsi_14", "vol_ratio", "return_hsi")

desc_stats <- data_clean %>%
  select(all_of(numeric_vars)) %>%
  summarise_all(list(
    均值 = ~mean(.),
    标准差 = ~sd(.),
    最小值 = ~min(.),
    第一分位数 = ~quantile(., 0.25),
    中位数 = ~median(.),
    第三分位数 = ~quantile(., 0.75),
    最大值 = ~max(.),
    偏度 = ~e1071::skewness(.),
    峰度 = ~e1071::kurtosis(.),
    观测数 = ~n()
  ))

# 将描述性统计整理为可读格式
desc_stats_long <- desc_stats %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  separate(stat, into = c("variable", "statistic"), sep = "_") %>%
  pivot_wider(names_from = "statistic", values_from = "value")

print(desc_stats_long, digits = 3)

# -------------------------------
# 5. 相关性分析
# -------------------------------
cat("\n=== 相关性分析 ===\n")

# 计算所有变量的相关系数
all_vars <- c("return_xm", "ma_gap", "rsi_14", "vol_ratio", 
              event_vars, "return_hsi")

cor_matrix <- data_clean %>%
  select(all_of(all_vars)) %>%
  cor(use = "complete.obs")

cat("相关系数矩阵:\n")
print(round(cor_matrix, 3))

# 可视化相关系数矩阵
png("correlation_matrix.png", width = 1000, height = 1000, res = 150)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.6,
         title = "变量相关系数矩阵",
         mar = c(0, 0, 2, 0))
dev.off()
cat("相关系数矩阵图已保存为: correlation_matrix.png\n")

# 重点关注因变量与其他变量的相关性
cat("\n小米收益率(return_xm)与其他变量的相关性（按绝对值排序）:\n")
return_cor <- cor_matrix["return_xm", ]
return_cor_sorted <- return_cor[order(-abs(return_cor))]
print(round(return_cor_sorted, 3))

# 检查多重共线性
cat("\n=== 多重共线性初步检查 ===\n")
cat("注意：如果两个自变量之间的相关系数绝对值大于0.8，可能存在多重共线性问题\n")
high_cor <- which(abs(cor_matrix) > 0.8 & abs(cor_matrix) < 1, arr.ind = TRUE)
if (length(high_cor) > 0) {
  high_cor_pairs <- data.frame(
    变量1 = rownames(cor_matrix)[high_cor[,1]],
    变量2 = colnames(cor_matrix)[high_cor[,2]],
    相关系数 = cor_matrix[high_cor]
  )
  high_cor_pairs <- high_cor_pairs[high_cor_pairs$变量1 != high_cor_pairs$变量2,]
  high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs[,1:2], 1, sort))),]
  print(high_cor_pairs)
}else {
  cat("未发现高度相关的自变量对（相关系数>0.8）\n")
}

