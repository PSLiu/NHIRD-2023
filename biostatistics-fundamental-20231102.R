# ~ 專案設定 ----

# 資料路徑
path_temp <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/biostatistics-fundamental"

# 套件安裝
install.packages(c("data.table", "ggplot2", "pROC", "tableone", "MatchIt", "survminer"), dependencies = T)

# 套件載入
library(data.table)
library(ggplot2)
library(pROC)
library(tableone)
library(MatchIt)
library(survival)
library(survminer)



# y = 連續變數 ----

# 資料讀取
setwd(path_temp)
bptrial <- fread("bptrial.csv")
head(bptrial)

table(bptrial$txgp)

# 描述性統計分析
# 年齡、收縮壓前測、舒張壓前測的數值分布
summary(bptrial$age)
quantile(bptrial$sbp_pre)
quantile(bptrial$dbp_pre, probs = c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1))
hist(bptrial$sbp_pre, breaks = seq(120, 140, 2), probability = T)

# 皮爾森相關係數 Person’s r correlation coefficient
# 年齡、收縮壓前測、舒張壓前測、收縮壓後測、舒張壓後測的相關係數
cor(bptrial[, .(age, sbp_pre, dbp_pre, sbp_post, dbp_post)])

# 獨立樣本 t 檢定 Independent t-test 
# 檢定兩個介入組別在收縮壓前測、舒張壓前測是否有差異
t.test(sbp_pre ~ txgp, data = bptrial)

var.test(sbp_pre ~ txgp, data = bptrial)

# 成對樣本 t 檢定 Paired t-test 
# 檢定介入組的收縮壓前測與收縮壓後測是否有差異
t.test(bptrial[txgp == 1]$sbp_pre, bptrial[txgp == 1]$sbp_post, paired = T)

# 檢定控制組的收縮壓前測與收縮壓後測是否有差異
t.test(bptrial[txgp == 0]$sbp_pre, bptrial[txgp == 0]$sbp_post, paired = T)

# 變異數分析 ANOVA (Analysis of variance) 事後檢定 post-hoc test
# 檢定三個年齡層之間在收縮壓後測是否有差異
anova_model <- aov(sbp_post ~ factor(agegp), data = bptrial)
summary(anova_model)
TukeyHSD(anova_model, "factor(agegp)")

# 線性迴歸模型 - univariable analysis
# y = 收縮壓後測 x = 介入組別
reg_univariable <- glm(sbp_post ~ txgp, data = bptrial)
summary(reg_univariable)

# 線性迴歸模型 - multivariable analysis
# y = 收縮壓後測 x = 介入組別 + 性別 + 年齡 + 生活品質量表分數前測
reg_multivariable <- glm(sbp_post ~ txgp + male + age + qol_scale_pre, data = bptrial)
summary(reg_multivariable)



# y = 類別變數 ----

# 描述性統計分析
# 生活品質量表分數前測的次數分析
table(bptrial$qol_scale_pre)

# 長條圖 bar chart
barplot(table(bptrial$qol_scale_pre))

# 交叉分析表 cross table 卡方檢定 χ2 test
# 組別與性別的交叉列聯表
table(bptrial$male, bptrial$txgp)
with(bptrial, table(male, txgp))
chisq.test(bptrial$male, bptrial$txgp)

# 費氏精確檢定 Fisher exact test
fisher.test(bptrial$male, bptrial$txgp)

# Logistic模型 - Logistic model - univariable analysis
# y = 生活品質不佳(分數 >= 5分)後測 x = 介入組別
logistic_univariable <- glm(qol_bad_post ~ txgp, data = bptrial, family = binomial(link = "logit"))
summary(logistic_univariable)

# Logistic模型 - Logistic model - multivariable analysis
# y = 生活品質不佳(分數 >= 5分)後測 x = 介入組別 + 性別 + 年齡
logistic_multivariable <- glm(qol_bad_post ~ txgp + male + age, data = bptrial, family = binomial(link = "logit"))
summary(logistic_multivariable)

# Logistic模型 - Logistic model - multivariable analysis and ROC plot
bptrial$ps <- predict(logistic_multivariable, data = bptrial, type = "response")
summary(bptrial$ps)
logistic_multivariable_roc <- roc(bptrial$qol_bad_post, bptrial$ps)
auc(logistic_multivariable_roc)
plot(logistic_multivariable_roc, col = "red", lwd = 3)
text(x = 0.2, y = 0.2, paste0("AUROC = ", round(auc(logistic_multivariable_roc), 4)))



# 傾向分數配對 ----

# 資料讀取
setwd(path_temp)
oacs_dm_complications <- fread("oacs_dm_complications.csv")
head(oacs_dm_complications)

# 背景特質比較
oacs_dm_complications_tab1 <- CreateTableOne(
  vars =       c("age", "male", "c2vs", "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  factorVars = c(       "male",         "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  strata = "oacs",
  data = oacs_dm_complications
)
print(oacs_dm_complications_tab1)

# Logistic回歸模型
psmodel <- glm(noac ~ age + male + c2vs + hyperlipidemia + ckd + cancer + year_2014_2015 + year_2016_2017, data = oacs_dm_complications, family = binomial(link = "logit"))
summary(psmodel)

# 傾向分數計算
oacs_dm_complications$ps <- predict(psmodel, data = oacs_dm_complications, type = "response")
summary(oacs_dm_complications$ps)

# 傾向分數分布
ggplot(oacs_dm_complications, aes(x = ps, fill = factor(noac))) +
  geom_histogram(
    position = "identity", alpha = 0.5, 
    breaks = seq(from = 0, to = 1, by = 0.05), binwidth = 0.05, color = "black")

# 傾向分數配對
oacs_dm_complications_matched <- matchit(noac ~ age + male + c2vs + hyperlipidemia + ckd + cancer + year_2014_2015 + year_2016_2017, data = oacs_dm_complications, caliper = 0.2, ratio = 1, discard = "treated")
oacs_dm_complications_matched <- match.data(oacs_dm_complications_matched)

oacs_dm_complications_matched <- oacs_dm_complications_matched[order(subclass, noac)]
head(oacs_dm_complications_matched[, .(subclass, id, ps, noac)], 10)

# 背景特質比較
oacs_dm_complications_tab1_matched <- CreateTableOne(
  vars = c("age", "male", "c2vs", "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  factorVars = c("male", "hyperlipidemia", "ckd", "cancer", "index_year_gp"),
  strata = "oacs",
  data = oacs_dm_complications_matched, test = F
)
print(oacs_dm_complications_tab1_matched, smd = T)



# 存活分析 ----

# 發生事件
with(oacs_dm_complications_matched, table(event_occur, noac))

# 觀察時間
summary(oacs_dm_complications_matched[noac == 0]$event_ft)
summary(oacs_dm_complications_matched[noac == 1]$event_ft)

# 計算分組觀察期間內結果事件發生率(每千人年)
print(oacs_dm_complications_matched[, .(totN = .N, totEVENT = sum(event_occur), totFT = sum(event_ft), totIR = (sum(event_occur) / sum(event_ft)) * 1000), by = .(noac)][order(-noac)])

# 存活曲線 KM Curves 風險集人數 Number at risk
oacs_dm_complications_matched_km_model <- survfit(Surv(event_ft, event_occur) ~ noac, data = oacs_dm_complications_matched)

oacs_dm_complications_matched_km <- ggsurvplot(
  fit = oacs_dm_complications_matched_km_model,             # 模型參數
  data = oacs_dm_complications_matched,                # 代入資料
  ylim = c(0, 1), break.y.by = 0.2,    # y軸的範圍與間距
  xlim = c(0, 7), break.x.by = 2,      # x軸的範圍與間距
  pval = TRUE, pval.coord = c(5, 0.8), # 呈現log-rank test及座標
  palette = c("blue", "red"),          # 線條的顏色
  censor = F,                          # 不要呈現censor的符號
  legend.title = "Treatment",          # 標籤標題
  legend.labs = c("Warfarin", "NOAC"), # 標籤文字
  risk.table = T,                      # 風險集人數
  risk.table.height = 0.3)             # 風險集佔據畫面的高度比例

print(oacs_dm_complications_matched_km)

# Cox迴歸模型 Cox proportional hazard regression
oacs_dm_complications_matched_cox <- coxph(Surv(event_ft, event_occur) ~ noac + strata(subclass), data = oacs_dm_complications_matched)  # 加上strata
summary(oacs_dm_complications_matched_cox)

# 等比風險假設檢定 Test for proportional hazard assumption
cox.zph(oacs_dm_complications_matched_cox)





### END ###