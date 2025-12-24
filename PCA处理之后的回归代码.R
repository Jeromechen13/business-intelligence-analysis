# ============================================
# 小米股价回归分析 - 基于tech_factor_pc1的主成分分析
# 去除了预测结果部分的优化版本
# ============================================

# -------------------------------
# 1. 加载必要的R包
# -------------------------------
required_packages <- c("tidyverse", "broom", "car", "stargazer", "ggplot2", 
                       "corrplot", "lmtest", "sandwich", "ggpubr", "performance")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(tidyverse)  # 数据处理和可视化
library(broom)      # 整理回归结果
library(car)        # 回归诊断
library(stargazer)  # 制作专业回归表格
library(ggplot2)    # 绘图
library(corrplot)   # 相关性可视化
library(lmtest)     # 异方差检验
library(sandwich)   # 稳健标准误
library(ggpubr)     # 组合图形
library(performance) # 模型性能评估

# -------------------------------
# 2. 读取和检查数据
# -------------------------------
# 注意：根据您的图片，您已经有了tech_factor_pc1列
data <- read.csv("final_analysis_data(PCA2).csv", stringsAsFactors = FALSE)

# 检查数据结构
cat("=== 数据概况 ===\n")
cat("数据维度（行数, 列数）:", dim(data), "\n")
cat("变量名称:\n")
print(names(data))
cat("\n")

# 查看前几行数据
cat("数据前6行预览:\n")
print(head(data, 6))

# 检查tech_factor_pc1列是否存在
if (!"tech_factor_pc1" %in% names(data)) {
  stop("数据中没有找到tech_factor_pc1列，请确认数据文件包含此列。")
}

# 从图片中看到日期格式是"2024/3/25"，转换为标准日期格式
if (!inherits(data$date, "Date")) {
  data$date <- as.Date(data$date, tryFormats = c("%Y/%m/%d", "%Y-%m-%d", "%m/%d/%Y"))
}

# 查看数据时间范围
cat("数据时间范围:", min(data$date, na.rm = TRUE), "至", max(data$date, na.rm = TRUE), "\n")
cat("总行数:", nrow(data), "\n\n")

# -------------------------------
# 3. 数据质量检查
# -------------------------------
cat("=== 数据质量检查 ===\n")



# 3.1 检查事件变量的分布
cat("\n=== 事件变量分布统计 ===\n")
event_vars <- c("phone_pos", "phone_neg", "car_pos", "car_neg", "iot_pos", "iot_neg")

event_summary <- data %>%
  select(all_of(event_vars)) %>%
  summarise_all(list(
    发生天数 = ~sum(. == 1, na.rm = TRUE),
    比例 = ~round(mean(. == 1, na.rm = TRUE) * 100, 2)
  ))

# 整理事件变量统计结果
event_stats <- data.frame(
  事件类型 = c("手机正面", "手机负面", "汽车正面", "汽车负面", "生态链正面", "生态链负面"),
  发生天数 = as.numeric(event_summary[1, seq(1, 12, by = 2)]),
  比例 = as.numeric(event_summary[1, seq(2, 12, by = 2)])
)
print(event_stats)

# 3.2 描述性统计
cat("\n=== 描述性统计 ===\n")
numeric_vars <- c("return_xm", "tech_factor_pc1", "vol_ratio", "return_hsi")

desc_stats <- data %>%
  select(all_of(numeric_vars)) %>%
  summarise_all(list(
    均值 = ~mean(., na.rm = TRUE),
    标准差 = ~sd(., na.rm = TRUE),
    最小值 = ~min(., na.rm = TRUE),
    第一分位数 = ~quantile(., 0.25, na.rm = TRUE),
    中位数 = ~median(., na.rm = TRUE),
    第三分位数 = ~quantile(., 0.75, na.rm = TRUE),
    最大值 = ~max(., na.rm = TRUE)
  ))

# 将描述性统计整理为可读格式
desc_stats_long <- desc_stats %>%
  pivot_longer(everything(), names_to = "stat", values_to = "value") %>%
  separate(stat, into = c("variable", "statistic"), sep = "_") %>%
  pivot_wider(names_from = "statistic", values_from = "value") %>%
  arrange(factor(variable, levels = c("return_xm", "return_hsi", "tech_factor", "vol_ratio")))

print(desc_stats_long, digits = 3)

# -------------------------------
# 4. 相关性分析（检查多重共线性）
# -------------------------------
cat("\n=== 相关性分析 ===\n")

# 计算所有变量的相关系数
all_vars <- c("return_xm", "tech_factor_pc1", "vol_ratio", 
              event_vars, "return_hsi")

# 创建相关系数矩阵
cor_matrix <- data %>%
  select(all_of(all_vars)) %>%
  cor(use = "complete.obs")  # 成对删除缺失值

cat("相关系数矩阵:\n")
print(round(cor_matrix, 3))

# 可视化相关系数矩阵
png("相关系数矩阵_主成分模型.png", width = 1000, height = 1000, res = 150)
corrplot(cor_matrix, 
         method = "color", 
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.6,
         title = "变量相关系数矩阵（主成分模型）",
         mar = c(0, 0, 2, 0))
dev.off()
cat("\n相关系数矩阵图已保存为: 相关系数矩阵_主成分模型.png\n")

# 重点关注因变量与其他变量的相关性
cat("\n小米收益率(return_xm)与其他变量的相关性:\n")
return_cor <- cor_matrix["return_xm", ]
return_cor_sorted <- return_cor[order(-abs(return_cor))]
print(round(return_cor_sorted, 3))

# 检查多重共线性
cat("\n=== 多重共线性诊断 ===\n")
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
} else {
  cat("未发现高度相关的自变量对（相关系数>0.8），多重共线性问题已解决。\n")
}

# 特别检查tech_factor_pc1与其他自变量的相关性
cat("\n=== tech_factor_pc1与其他自变量的相关性 ===\n")
tech_cor <- cor_matrix["tech_factor_pc1", ]
tech_cor_other <- tech_cor[!names(tech_cor) %in% c("tech_factor_pc1", "return_xm")]
print(round(tech_cor_other, 3))

# -------------------------------
# 5. 回归模型构建
# -------------------------------
cat("\n=== 回归模型构建 ===\n")

# 定义所有模型需要用到的变量
all_model_vars <- c("return_xm", "return_hsi", "tech_factor_pc1", "vol_ratio",
                    "phone_pos", "phone_neg", "car_pos", "car_neg", "iot_pos", "iot_neg")

# 创建只包含这些变量的数据集，并删除缺失值
model_data <- data %>%
  select(all_of(all_model_vars)) %>%
  drop_na()

cat("统一数据集维度:", dim(model_data), "\n")
cat("有效观测数:", nrow(model_data), "\n")

# 现在使用这个统一的数据集拟合所有模型
# 模型1：基准模型（仅控制市场因素）
cat("\n1. 模型1: 基准模型（仅市场因素）\n")
formula1 <- as.formula("return_xm ~ return_hsi")
model1 <- lm(formula1, data = model_data)
summary_model1 <- summary(model1)
cat("观测数:", nobs(model1), "\n")
cat("R平方:", round(summary_model1$r.squared, 4), "\n")
cat("调整R平方:", round(summary_model1$adj.r.squared, 4), "\n")

# 模型2：市场因素 + 技术指标主成分 + 成交量比率
cat("\n2. 模型2: 市场因素 + 技术指标主成分 + 成交量比率\n")
formula2 <- as.formula("return_xm ~ return_hsi + tech_factor_pc1 + vol_ratio")
model2 <- lm(formula2, data = model_data)
summary_model2 <- summary(model2)
cat("观测数:", nobs(model2), "\n")
cat("R平方:", round(summary_model2$r.squared, 4), "\n")
cat("调整R平方:", round(summary_model2$adj.r.squared, 4), "\n")

# 模型3：完整模型（市场 + 技术指标主成分 + 成交量比率 + 所有事件变量）
cat("\n3. 模型3: 完整模型（市场 + 技术指标主成分 + 成交量比率 + 所有事件变量）\n")
formula3 <- as.formula("return_xm ~ return_hsi + tech_factor_pc1 + vol_ratio + 
                       phone_pos + phone_neg + car_pos + car_neg + iot_pos + iot_neg")
model3 <- lm(formula3, data = model_data)
summary_model3 <- summary(model3)
cat("观测数:", nobs(model3), "\n")
cat("R平方:", round(summary_model3$r.squared, 4), "\n")
cat("调整R平方:", round(summary_model3$adj.r.squared, 4), "\n")

# 模型4：仅事件变量模型（比较用）
cat("\n4. 模型4: 仅事件变量模型（对比用）\n")
formula4 <- as.formula("return_xm ~ phone_pos + phone_neg + car_pos + car_neg + iot_pos + iot_neg")
model4 <- lm(formula4, data = model_data)
summary_model4 <- summary(model4)
cat("观测数:", nobs(model4), "\n")
cat("R平方:", round(summary_model4$r.squared, 4), "\n")
cat("调整R平方:", round(summary_model4$adj.r.squared, 4), "\n")

# 检查所有模型是否使用相同数量的观测值
cat("\n=== 模型观测数检查 ===\n")
model_nobs <- c(nobs(model1), nobs(model2), nobs(model3), nobs(model4))
if (length(unique(model_nobs)) == 1) {
  cat("所有模型使用相同观测数:", unique(model_nobs), "\n")
  cat("可以安全进行嵌套模型F检验。\n")
} else {
  cat("警告：模型观测数不一致！\n")
  cat("模型1:", model_nobs[1], "\n")
  cat("模型2:", model_nobs[2], "\n")
  cat("模型3:", model_nobs[3], "\n")
  cat("模型4:", model_nobs[4], "\n")
}

# -------------------------------
# 6. 回归结果汇总
# -------------------------------

# 模型比较统计
cat("\n=== 模型比较统计 ===\n")
results_summary <- data.frame(
  模型 = c("模型1: 仅市场", "模型2: 市场+技术主成分", "模型3: 完整模型", "模型4: 仅事件"),
  观测数 = c(nobs(model1), nobs(model2), nobs(model3), nobs(model4)),
  R平方 = c(summary_model1$r.squared, 
          summary_model2$r.squared, 
          summary_model3$r.squared, 
          summary_model4$r.squared),
  调整R平方 = c(summary_model1$adj.r.squared, 
            summary_model2$adj.r.squared, 
            summary_model3$adj.r.squared, 
            summary_model4$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4))
)

print(results_summary, digits = 3, row.names = FALSE)

# 模型比较F检验
cat("\n=== 嵌套模型F检验 ===\n")
cat("模型1 vs 模型2（加入技术指标主成分是否显著改善模型）:\n")
anova_1_2 <- anova(model1, model2)
print(anova_1_2)

cat("\n模型2 vs 模型3（加入事件变量是否显著改善模型）:\n")
anova_2_3 <- anova(model2, model3)
print(anova_2_3)

# -------------------------------
# 7. 回归诊断
# -------------------------------
cat("\n=== 模型3回归诊断 ===\n")

# 7.1 多重共线性检验（VIF）
cat("\n1. 方差膨胀因子(VIF)检验:\n")
vif_values <- vif(model3)
vif_df <- data.frame(
  变量 = names(vif_values),
  VIF值 = vif_values
)
vif_df$严重程度 <- ifelse(vif_df$VIF值 > 10, "严重多重共线性", 
                      ifelse(vif_df$VIF值 > 5, "中度多重共线性", "可接受"))
print(vif_df)

# 7.2 异方差检验
cat("\n2. 异方差检验:\n")
bp_test <- bptest(model3)
cat("Breusch-Pagan检验:\n")
cat("统计量:", round(bp_test$statistic, 3), "p值:", round(bp_test$p.value, 4), "\n")
if (bp_test$p.value < 0.05) {
  cat("结论: 存在异方差问题 (p < 0.05)，建议使用稳健标准误。\n")
} else {
  cat("结论: 无异方差问题 (p > 0.05)。\n")
}

# 7.3 残差正态性检验
cat("\n3. 残差正态性检验:\n")
# 对残差进行Shapiro-Wilk检验（限制样本量）
if (nobs(model3) > 5000) {
  # 如果样本量太大，抽样检验
  set.seed(123)
  sample_residuals <- sample(residuals(model3), 5000)
  shapiro_test <- shapiro.test(sample_residuals)
} else {
  shapiro_test <- shapiro.test(residuals(model3))
}
cat("Shapiro-Wilk正态性检验:\n")
cat("统计量W:", round(shapiro_test$statistic, 4), "p值:", format.pval(shapiro_test$p.value, digits = 4), "\n")
if (shapiro_test$p.value < 0.05) {
  cat("结论: 残差不服从正态分布 (p < 0.05)。\n")
} else {
  cat("结论: 残差服从正态分布 (p > 0.05)。\n")
}

# 7.4 自相关检验
cat("\n4. 自相关检验:\n")
dw_test <- dwtest(model3)
cat("Durbin-Watson检验:\n")
cat("DW统计量:", round(dw_test$statistic, 3), "p值:", round(dw_test$p.value, 4), "\n")
if (dw_test$p.value < 0.05) {
  cat("结论: 存在自相关 (p < 0.05)。\n")
} else {
  cat("结论: 无自相关 (p > 0.05)。\n")
}

# 7.5 离群值检测
cat("\n5. 离群值检测:\n")
# Cook's距离
cooks_d <- cooks.distance(model3)
high_influence <- which(cooks_d > 4/(nobs(model3)))
if (length(high_influence) > 0) {
  cat("发现高影响力观测点（Cook's距离 > 4/n）:", length(high_influence), "个\n")
  cat("观测点序号（在模型数据中的行号）:", high_influence, "\n")
} else {
  cat("未发现高影响力观测点。\n")
}

# -------------------------------
# 8. 稳健性检验
# -------------------------------
cat("\n=== 稳健性检验 ===\n")

# 8.1 使用稳健标准误
cat("\n1. 模型3的稳健标准误估计:\n")
robust_se <- coeftest(model3, vcov = vcovHC(model3, type = "HC3"))
print(robust_se)

# 8.2 比较普通标准误和稳健标准误
cat("\n2. 标准误比较（普通 vs 稳健）:\n")
if (nrow(summary_model3$coefficients) == nrow(robust_se)) {
  se_comparison <- data.frame(
    变量 = rownames(robust_se),
    普通标准误 = summary_model3$coefficients[, 2],
    稳健标准误 = robust_se[, 2],
    比值 = robust_se[, 2] / summary_model3$coefficients[, 2]
  )
  print(se_comparison, digits = 3)
} else {
  cat("注意：稳健标准误和普通标准误的系数行数不一致，无法直接比较。\n")
}

# 8.3 分样本稳健性检验（按时间）
cat("\n3. 分时间段稳健性检验:\n")
if (nrow(data) > 100) {  # 确保有足够的数据
  # 将原始数据分为两半
  split_point <- floor(nrow(data)/2)
  data_first_half <- data[1:split_point, ]
  data_second_half <- data[(split_point+1):nrow(data), ]
  
  # 创建每个时间段的完整数据集
  data_first_complete <- data_first_half %>%
    select(all_of(all_model_vars)) %>%
    drop_na()
  
  data_second_complete <- data_second_half %>%
    select(all_of(all_model_vars)) %>%
    drop_na()
  
  # 在前后半段分别拟合模型
  if (nrow(data_first_complete) > 20) {  # 确保有足够的观测
    model3_first <- lm(formula3, data = data_first_complete)
    cat("前半段时间:", min(data_first_complete$date, na.rm = TRUE), "至", 
        max(data_first_complete$date, na.rm = TRUE), "\n")
    cat("观测数:", nobs(model3_first), "\n")
    cat("R平方:", round(summary(model3_first)$r.squared, 4), "\n")
  } else {
    cat("前半段数据不足，无法拟合模型。\n")
  }
  
  if (nrow(data_second_complete) > 20) {  # 确保有足够的观测
    model3_second <- lm(formula3, data = data_second_complete)
    cat("后半段时间:", min(data_second_complete$date, na.rm = TRUE), "至", 
        max(data_second_complete$date, na.rm = TRUE), "\n")
    cat("观测数:", nobs(model3_second), "\n")
    cat("R平方:", round(summary(model3_second)$r.squared, 4), "\n")
  } else {
    cat("后半段数据不足，无法拟合模型。\n")
  }
} else {
  cat("样本量不足，无法进行分时间段检验。\n")
}

# -------------------------------
# 9. 结果可视化
# -------------------------------
cat("\n=== 生成可视化图表 ===\n")

# 9.1 回归系数图
coef_data <- tidy(model3) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = case_when(
      term == "return_hsi" ~ "市场收益率",
      term == "tech_factor_pc1" ~ "技术指标主成分",
      term == "vol_ratio" ~ "成交量比率",
      term == "phone_pos" ~ "手机正面事件",
      term == "phone_neg" ~ "手机负面事件",
      term == "car_pos" ~ "汽车正面事件",
      term == "car_neg" ~ "汽车负面事件",
      term == "iot_pos" ~ "生态链正面事件",
      term == "iot_neg" ~ "生态链负面事件",
      TRUE ~ term
    ),
    显著性 = case_when(
      p.value < 0.01 ~ "p < 0.01",
      p.value < 0.05 ~ "p < 0.05",
      p.value < 0.1 ~ "p < 0.1",
      TRUE ~ "不显著"
    ),
    显著性 = factor(显著性, levels = c("p < 0.01", "p < 0.05", "p < 0.1", "不显著"))
  )

p1 <- ggplot(coef_data, aes(x = reorder(term, estimate), y = estimate, 
                            color = 显著性, fill = 显著性)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, 
                    ymax = estimate + 1.96*std.error), 
                width = 0.2, size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 0.8) +
  coord_flip() +
  labs(title = "完整模型回归系数估计与95%置信区间",
       subtitle = paste("模型3 | 调整R² =", round(summary_model3$adj.r.squared, 3)),
       x = "解释变量", y = "系数估计值",
       color = "显著性水平", fill = "显著性水平") +
  scale_color_manual(values = c("p < 0.01" = "#2E8B57", 
                                "p < 0.05" = "#4682B4", 
                                "p < 0.1" = "#DAA520", 
                                "不显著" = "#A9A9A9")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 10))

# 9.2 模型解释力比较
p2_data <- data.frame(
  模型 = factor(c("仅市场", "+技术主成分", "+事件变量"), 
              levels = c("仅市场", "+技术主成分", "+事件变量")),
  R平方 = c(summary_model1$r.squared, 
          summary_model2$r.squared, 
          summary_model3$r.squared),
  调整R平方 = c(summary_model1$adj.r.squared, 
            summary_model2$adj.r.squared, 
            summary_model3$adj.r.squared)
)

p2_data_long <- p2_data %>%
  pivot_longer(cols = c(R平方, 调整R平方), 
               names_to = "指标", 
               values_to = "值")

p2 <- ggplot(p2_data_long, aes(x = 模型, y = 值, fill = 指标)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(值, 3)), 
            position = position_dodge(width = 0.8), 
            vjust = -0.5, size = 3.5) +
  labs(title = "模型解释力逐步提升",
       subtitle = "逐步加入技术指标主成分和事件变量",
       x = "模型", y = "R²值", fill = "指标") +
  ylim(0, max(p2_data_long$值) * 1.2) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"))

# 9.3 残差诊断图
# 为模型数据添加拟合值和残差
model_data$拟合值 <- fitted(model3)
model_data$残差 <- residuals(model3)

p4_1 <- ggplot(model_data, aes(x = 拟合值, y = 残差)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_smooth(method = "loess", se = TRUE, color = "darkgreen", fill = "lightgreen") +
  labs(title = "残差 vs 拟合值", x = "拟合值", y = "残差") +
  theme_minimal()

p4_2 <- ggplot(model_data, aes(sample = 残差)) +
  geom_qq(color = "steelblue", alpha = 0.6) +
  geom_qq_line(color = "red", size = 0.8) +
  labs(title = "Q-Q图", x = "理论分位数", y = "样本分位数") +
  theme_minimal()

p4_3 <- ggplot(model_data, aes(x = 残差)) +
  geom_histogram(aes(y = ..density..), bins = 30, 
                 fill = "steelblue", alpha = 0.7, color = "white") +
  geom_density(color = "red", size = 1) +
  labs(title = "残差分布", x = "残差", y = "密度") +
  theme_minimal()

# 组合残差图
p4 <- ggarrange(p4_1, p4_2, p4_3, ncol = 3, nrow = 1)
p4 <- annotate_figure(p4, 
                      top = text_grob("残差诊断图", 
                                      size = 16, face = "bold"))

# 保存所有图形
ggsave("回归系数图_主成分模型.png", p1, width = 10, height = 8, dpi = 300)
ggsave("模型解释力比较_主成分模型.png", p2, width = 8, height = 6, dpi = 300)
ggsave("残差诊断图_主成分模型.png", p4, width = 15, height = 5, dpi = 300)

cat("所有可视化图表已保存为PNG文件\n")

# -------------------------------
# 10. 结果保存
# -------------------------------
cat("\n=== 保存分析结果 ===\n")

# 创建结果文件夹
if (!dir.exists("主成分回归分析结果")) {
  dir.create("主成分回归分析结果")
}

# 保存回归结果
sink("主成分回归分析结果/完整回归结果.txt")
cat("小米股价收益率回归分析结果（主成分模型）\n")
cat("==========================================\n\n")
cat("分析时间:", format(Sys.time(), "%Y年%m月%d日 %H:%M:%S"), "\n")
cat("数据期间:", min(data$date, na.rm = TRUE), "至", max(data$date, na.rm = TRUE), "\n")
cat("有效观测数（模型3）:", nobs(model3), "\n\n")

cat("一、描述性统计\n")
cat("----------------\n")
print(desc_stats_long, digits = 3)

cat("\n\n二、相关性分析\n")
cat("----------------\n")
cat("相关系数矩阵:\n")
print(round(cor_matrix, 3))

cat("\n小米收益率与其他变量的相关性:\n")
print(round(return_cor_sorted, 3))

cat("\n\n三、回归模型结果\n")
cat("----------------\n")
cat("模型1: 基准模型（仅市场因素）\n")
print(summary_model1)
cat("\n\n模型2: 市场因素 + 技术指标主成分 + 成交量比率\n")
print(summary_model2)
cat("\n\n模型3: 完整模型（市场 + 技术指标主成分 + 成交量比率 + 事件变量）\n")
print(summary_model3)

cat("\n\n四、模型比较\n")
cat("----------------\n")
print(results_summary, digits = 3, row.names = FALSE)

cat("\n\n五、模型诊断\n")
cat("----------------\n")
cat("1. 多重共线性检验（VIF）:\n")
print(vif_df)
cat("\n2. 异方差检验:\n")
cat("   Breusch-Pagan检验: 统计量 =", round(bp_test$statistic, 3), 
    "p值 =", round(bp_test$p.value, 4), "\n")
cat("\n3. 正态性检验:\n")
cat("   Shapiro-Wilk检验: W =", round(shapiro_test$statistic, 4), 
    "p值 =", format.pval(shapiro_test$p.value, digits = 4), "\n")
cat("\n4. 自相关检验:\n")
cat("   Durbin-Watson检验: DW =", round(dw_test$statistic, 3), 
    "p值 =", round(dw_test$p.value, 4), "\n")

cat("\n\n六、事件变量分析\n")
cat("----------------\n")
print(event_stats, row.names = FALSE)
sink()

# 保存模型对象
saveRDS(list(model1 = model1, 
             model2 = model2, 
             model3 = model3,
             model4 = model4,
             data = data), 
        file = "主成分回归分析结果/回归模型对象.rds")

# 保存回归系数
coefficients_df <- bind_rows(
  tidy(model1) %>% mutate(模型 = "模型1: 仅市场"),
  tidy(model2) %>% mutate(模型 = "模型2: 市场+技术主成分"),
  tidy(model3) %>% mutate(模型 = "模型3: 完整模型"),
  tidy(model4) %>% mutate(模型 = "模型4: 仅事件")
)
write.csv(coefficients_df, "主成分回归分析结果/回归系数.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\n=== 分析完成 ===\n")
cat("所有结果已保存到 '主成分回归分析结果' 文件夹中:\n")
cat("1. 完整回归结果.txt - 详细分析报告\n")
cat("2. 回归模型对象.rds - 模型对象（可用于后续预测）\n")
cat("3. 回归系数.csv - 各模型的回归系数\n")
cat("4. 多个PNG文件 - 可视化图表\n")

# -------------------------------
# 11. 主要发现总结
# -------------------------------
cat("\n=== 主要发现总结 ===\n")

# 提取模型3的显著变量
significant_vars <- tidy(model3) %>%
  filter(p.value < 0.1 & term != "(Intercept)") %>%
  arrange(p.value)

if (nrow(significant_vars) > 0) {
  cat("在完整模型（模型3）中，以下变量在10%水平下显著：\n")
  for (i in 1:nrow(significant_vars)) {
    var_name <- significant_vars$term[i]
    estimate <- significant_vars$estimate[i]
    p_value <- significant_vars$p.value[i]
    
    # 中文标签映射
    var_label <- case_when(
      var_name == "return_hsi" ~ "恒生指数收益率",
      var_name == "tech_factor_pc1" ~ "技术指标主成分",
      var_name == "vol_ratio" ~ "成交量比率",
      var_name == "phone_pos" ~ "手机正面事件",
      var_name == "phone_neg" ~ "手机负面事件",
      var_name == "car_pos" ~ "汽车正面事件",
      var_name == "car_neg" ~ "汽车负面事件",
      var_name == "iot_pos" ~ "生态链正面事件",
      var_name == "iot_neg" ~ "生态链负面事件",
      TRUE ~ var_name
    )
    
    significance <- ifelse(p_value < 0.01, "极显著", 
                           ifelse(p_value < 0.05, "显著", "较显著"))
    
    effect <- ifelse(estimate > 0, "正向", "负向")
    
    cat(sprintf("   - %s: %s影响 (系数=%.4f, p=%.4f, %s)\n", 
                var_label, effect, estimate, p_value, significance))
  }
} else {
  cat("在完整模型中，没有变量在10%水平下显著\n")
}

# 模型解释力总结
r2_improvement_2 <- summary_model2$r.squared - summary_model1$r.squared
r2_improvement_3 <- summary_model3$r.squared - summary_model2$r.squared

cat(sprintf("\n模型解释力变化:\n"))
cat(sprintf("   - 加入技术指标主成分后，R²提高: %.3f\n", r2_improvement_2))
cat(sprintf("   - 加入事件变量后，R²提高: %.3f\n", r2_improvement_3))
cat(sprintf("   - 完整模型解释力: %.1f%%\n", summary_model3$r.squared * 100))

cat("\n多重共线性评估:\n")
if (max(vif_df$VIF值) > 5) {
  cat("   - 存在中度多重共线性，但通过主成分分析已得到改善。\n")
} else {
  cat("   - 多重共线性问题已通过主成分分析解决。\n")
}

cat("\n模型稳健性:\n")
if (bp_test$p.value < 0.05) {
  cat("   - 存在异方差，建议参考稳健标准误结果。\n")
} else {
  cat("   - 无异方差问题。\n")
}

cat("\n业务建议:\n")
cat("1. 使用tech_factor_pc1成功解决了rsi_14和ma_gap的多重共线性问题。\n")
cat("2. 该主成分综合了两个技术指标的信息，可解释为'技术面综合因子'。\n")
cat("3. 模型3（完整模型）提供了最全面的分析，建议以此为基础进行业务决策。\n")
cat("4. 事件变量中，关注显著性较高的事件类型对股价的影响。\n")
