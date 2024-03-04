# ~ 專案設定 ----

# 資料路徑
pathraw <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/study-practice-cohort/raw"
pathuse <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/study-practice-cohort/use"

# 下載套件
install.packages(c("data.table", "lubridate", "fastDummies", "tableone", "survminer"), dependencies = T)

# 使用套件
library(data.table)
library(lubridate)
library(fastDummies)
library(tableone)
library(survival)
library(survminer)





### 研究樣本 ----



# ~ 研究族群 ----

# 讀取住院費用檔 h_nhi_ipdte2014.csv
setwd(pathraw)
target_1 <- fread("h_nhi_ipdte2014.csv", select = c("id", "in_date", "hosp_id", "icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"), encoding = "UTF-8", colClasses = "character")
head(target_1)

# 找出有目標診斷碼的(肺炎 pneumonia 480-486)
target_2 <- target_1[grepl(pattern = "^48[0-6]", icd9cm_1) | grepl(pattern = "^48[0-6]", icd9cm_2) | grepl(pattern = "^48[0-6]", icd9cm_3) | grepl(pattern = "^48[0-6]", icd9cm_4) | grepl(pattern = "^48[0-6]", icd9cm_5)]
head(target_2)

# 將in_date欄位由文字轉為日期
target_2$in_date <- ymd(target_2$in_date)
head(target_2)
summary(target_2$in_date)

# 找出每人的首次住院日期
target_3 <- target_2[order(id, in_date)]
target_3 <- target_3[, .SD[1], by = .(id)]
head(target_3)
summary(target_3$in_date)

# 分析住院期的年份與月份交叉表
table(month(target_3$in_date), year(target_3$in_date))

# 取出首次在4-9月的病患
target_4 <- target_3[month(in_date) %in% c(4:9)]
table(month(target_4$in_date))
head(target_4)
nrow(target_4)
summary(target_4)

# 檔案整理
target <- target_4

target_id <- target_4[, .(id, in_date)]

rm(target_1, target_2, target_3, target_4)
gc()



# ~ 死亡登記 ----

# 讀取死亡登記檔 h_ost_death2014.csv
setwd(pathraw)
death_1 <- fread("h_ost_death2014.csv", select = c("id", "d_date"), encoding = "UTF-8", colClasses = "character")
head(death_1)

# 取出目標樣本的死亡資料
# 概念為將此資料的ID與目標樣本的ID合併取交集
death_2 <- merge.data.table(death_1, target_id, by = c("id"))
head(death_2)

# 將d_date欄位由文字轉為日期
death_3 <- death_2[, .(id, death = 1, death_date = ymd(d_date))]
head(death_3)

# 檔案整理
death <- death_3
head(death)

rm(death_1, death_2, death_3)
gc()



# ~ 資料合併 ----

# target與death合併
pop_1 <- merge.data.table(target, death, by = c("id"), all.x = T)
head(pop_1)

# 沒有死亡登記的補0和2014-12-31
pop_2 <- pop_1[is.na(death), `:=`(death = 0, death_date = ymd("20141231"))]
head(pop_2)
summary(pop_2)

# 檔案整理
pop <- pop_2
head(pop)

rm(pop_1, pop_2)
rm(death)
gc()





### 基本資料 ----



# ~ 年齡性別 ----

# 讀取健保承保檔 h_nhi_enrol2014.csv
setwd(pathraw)
idfile_1 <- fread("h_nhi_enrol2014.csv", select = c("id", "id_s", "id_birth_y", "prem_ym"), encoding = "UTF-8", colClasses = "character")
head(idfile_1)

# 取出目標樣本的承保資料
# 概念為將此資料的ID與目標樣本的ID合併取交集
idfile_2 <- merge.data.table(idfile_1, target_id, by = c("id"))
head(idfile_2)

# 擷取需要使用欄位
idfile_3 <- idfile_2[, .(id, id_s, id_birth_y = as.numeric(id_birth_y), prem_ym)]
head(idfile_3)

# 檢查欄位內容
table(idfile_3$id_s)
summary(idfile_3$id_birth_y)
summary(2014 - idfile_3$id_birth_y)

# 保留性別編碼為1或2者
idfile_4 <- idfile_3[id_s %in% c("1", "2")]
table(idfile_4$id_s)

# 每人保留一筆
idfile_5 <- idfile_4[order(id, prem_ym)][, .SD[1], by = .(id)]
head(idfile_5)

idfile_5$prem_ym <- NULL

# 虛擬變數
idfile_6 <- idfile_5
idfile_6 <- idfile_6[, `:=`(male = 0)][id_s == "1", `:=`(male = 1)]
head(idfile_6)

# 檔案整理
idfile <- idfile_6
head(idfile)

rm(idfile_1, idfile_2, idfile_3, idfile_4, idfile_5, idfile_6)
gc()





# ~ 承保資料 ----

# 讀取健保承保檔 h_nhi_enrol2014.csv
setwd(pathraw)
nhi_1 <- fread("h_nhi_enrol2014.csv", select = c("id", "prem_ym", "id1_amt"), encoding = "UTF-8", colClasses = "character")
head(nhi_1)

# 擷取需要使用欄位
nhi_2 <- nhi_1[, .(id, prem_ymd = ymd(paste0(prem_ym, "01")), nhi_fee = as.numeric(id1_amt))]
head(nhi_2)

# 檢查欄位內容
summary(nhi_2$prem_ymd)
summary(nhi_2$nhi_fee)

# 取出目標樣本的投保資料
# 概念為將此資料的ID與目標樣本的ID合併取交集
nhi_3 <- merge.data.table(nhi_2, target_id, by = c("id"))
head(nhi_3)

# 保留住院日(不含)以前的最近一筆投保紀錄
nhi_4 <- nhi_3[prem_ymd < in_date]
nhi_4 <- nhi_4[order(id, -prem_ymd)][, .SD[1], by = .(id)]
head(nhi_4)

# 將投保金額以級距分類
nhi_5 <- nhi_4[, .(id, nhi_fee)]
nhi_5 <- nhi_5[, `:=`(nhi_fee_range = cut(x = nhi_fee, breaks = c(0, 15840, 30000, 200000), labels = c(1:3), include.lowest = T))]
head(nhi_5)

# 創造投保金額級距的虛擬變數
nhi_6 <- dummy_cols(.data = nhi_5, select_columns = c("nhi_fee_range"))
head(nhi_6)

# 檔案整理
nhi <- nhi_6
head(nhi)

rm(nhi_1, nhi_2, nhi_3, nhi_4, nhi_5, nhi_6)
gc()





### 糖尿病史 ----



# ~ 門診處方紀錄 ----

# 讀取藥物清單檔 code_dmdrug.csv
setwd(pathuse)
dm_1_dmdrug <- fread("code_dmdrug.csv", encoding = "UTF-8", colClasses = "character")
head(dm_1_dmdrug)

# 讀取門診醫令檔 h_nhi_opdto2014.csv
setwd(pathraw)
dm_1_opdto <- fread("h_nhi_opdto2014.csv", select = c("drug_no", "hosp_id", "fee_ym", "appl_type", "appl_date", "case_type", "seq_no"), encoding = "UTF-8", colClasses = "character")
head(dm_1_opdto)

# 找出糖尿病藥物處方(申報資料 & 藥物代碼的交集)
nrow(dm_1_opdto)
dm_1_opdto <- merge.data.table(dm_1_opdto, dm_1_dmdrug, by = c("drug_no"))
head(dm_1_opdto)
nrow(dm_1_opdto)



# ~ 門診申報紀錄 ----

# 讀取門診費用檔 h_nhi_opdte2014.csv
setwd(pathraw)
dm_1_opdte <- fread("h_nhi_opdte2014.csv", select = c("id", "func_date", "hosp_id", "fee_ym", "appl_type", "appl_date", "case_type", "seq_no"), encoding = "UTF-8", colClasses = "character")
head(dm_1_opdte)

# 擷取需要使用欄位
dm_1_opdte <- dm_1_opdte[, .(id, func_date = ymd(func_date), hosp_id, fee_ym, appl_type, appl_date, case_type, seq_no)]
head(dm_1_opdte)

# 取出目標樣本的就醫資料
nrow(dm_1_opdte)
dm_1_opdte <- merge.data.table(dm_1_opdte, target_id, by = c("id"))
head(dm_1_opdte)
nrow(dm_1_opdte)

# 保留住院日(不含)以前的就醫紀錄
nrow(dm_1_opdte)
dm_1_opdte <- dm_1_opdte[func_date < in_date]
head(dm_1_opdte)
nrow(dm_1_opdte)



# ~ 藥物紀錄整合 ----

# 合併申報及醫令
nrow(dm_1_opdte)
nrow(dm_1_opdto)

dm_1 <- merge.data.table(dm_1_opdte, dm_1_opdto, by = c("hosp_id", "fee_ym", "appl_type", "appl_date", "case_type", "seq_no"))
head(dm_1)
nrow(dm_1)

dm_2 <- unique(dm_1[, .(id, dm = 1)])



# 檔案整理
dm <- dm_2

head(dm)
summary(dm)

rm(dm_1, dm_1_opdte, dm_1_opdto, dm_1_dmdrug, dm_2)
gc()





### 資訊統整 ----



# ~ 變數合併 ----

# 住院資訊合併死亡資訊
dtmerge <- pop
head(dtmerge)

# 合併身分資料
dtmerge <- merge.data.table(dtmerge, idfile, by = c("id"), all.x = T)
head(dtmerge)

# 計算年齡
dtmerge <- dtmerge[, `:=`(age = year(in_date) - id_birth_y)]
head(dtmerge)
summary(dtmerge$age)

# 合併投保資料
dtmerge <- merge.data.table(dtmerge, nhi, by = c("id"), all.x = T)
head(dtmerge)

# 合併DM資料
dtmerge <- merge.data.table(dtmerge, dm, by = c("id"), all.x = T)
dtmerge <- dtmerge[is.na(dm), `:=`(dm = 0)]
head(dtmerge)

# 存活分析變數
dtmerge <- dtmerge[, `:=`(event_occur = 0)]
dtmerge <- dtmerge[death == 1, `:=`(event_occur = 1)]

dtmerge <- dtmerge[, `:=`(event_ft = as.numeric(death_date - in_date))]

summary(dtmerge)



# ~ 排除條件 ----

dtselect <- dtmerge

# 排除基本資料不齊全者
summary(dtselect$age)

dtselect <- dtselect[!(is.na(age))]

# 檢查是否有死亡日期早於住院日期
nrow(dtselect[death == 1 & death_date <= in_date])

# 可以用來分析的資料集
head(dtselect)

# 將整理好的資料檔儲存下來
setwd(pathuse)
fwrite(dtselect, "dtfinal.csv")





### 統計分析 ----



# ~ 樣本特性比較 ----

# table 1

table_vars <- c("age", "male", "nhi_fee_range")
table_fact <- c(       "male", "nhi_fee_range")

t1 <- CreateTableOne(
  vars = table_vars,
  factorVars = table_fact,
  strata = "dm",
  data = dtselect,
  test = F
)

print(t1, smd = T)



# ~ Incidence and KM ----

# Incidence rate
inc_tab <- dtselect[, .(totN = .N, totEVENT = sum(event_occur), totFT = sum(event_ft), totIR = (sum(event_occur) / sum(event_ft)) * 1000), by = .(dm)][order(-dm)]
print(inc_tab)

# KM curves
km_model <- survfit(Surv(event_ft, event_occur) ~ dm, data = dtselect)

km_plot <- ggsurvplot(
  fit = km_model,                        # 模型參數
  data = dtselect,                       # 代入資料
  ylim = c(0, 1), break.y.by = 0.2,      # y軸的範圍與間距
  xlim = c(0, 270), break.x.by = 30,     # x軸的範圍與間距
  pval = TRUE, pval.coord = c(200, 0.2), # 呈現log-rank test及座標
  palette = c("blue", "red"),            # 線條的顏色
  censor = F,                            # 不要呈現censor的符號
  legend.title = "DM history",           # 標籤標題
  legend.labs = c("No", "Yes"),          # 標籤文字
  risk.table = T,                        # 風險集人數
  risk.table.height = 0.3)               # 風險集佔據畫面的高度比例

print(km_plot)



# ~ Cox regression ----

# univariable model
cox_mod_1 <- coxph(Surv(event_ft, event_occur) ~ dm, data = dtselect)
summary(cox_mod_1)

# multivariable model
cox_mod_2 <- coxph(Surv(event_ft, event_occur) ~ dm + male + age + nhi_fee_range_2 + nhi_fee_range_3, data = dtselect)
summary(cox_mod_2)





### END ###
### Peter Pin-Sung Liu ###
### 劉品崧 ###
### psliu520@gmail.com ###