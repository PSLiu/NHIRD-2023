# ~ 專案設定 ----

# 資料路徑
pathraw <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/study-practice-case-control/raw"
pathuse <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/study-practice-case-control/use"

# 使用套件
library(data.table)
library(lubridate)
library(fastDummies)
library(tableone)





### 研究樣本 ----



# ~ 研究族群 ----

# 讀取住院費用檔 h_nhi_ipdte2014.csv
setwd(pathraw)
target_1 <- fread("h_nhi_ipdte2014.csv", encoding = "UTF-8", colClasses = "character")
head(target_1)

# 擷取需要使用欄位
target_2 <- target_1[, .(id, in_date, hosp_id, icd9cm_1, icd9cm_2, icd9cm_3, icd9cm_4, icd9cm_5)]
head(target_2)

# 找出有目標診斷碼的(肺炎 pneumonia 480-486)
target_3 <- target_2[grepl(pattern = "^48[0-6]", icd9cm_1) | grepl(pattern = "^48[0-6]", icd9cm_2) | grepl(pattern = "^48[0-6]", icd9cm_3) | grepl(pattern = "^48[0-6]", icd9cm_4) | grepl(pattern = "^48[0-6]", icd9cm_5)]
head(target_3)

# 將in_date欄位由文字轉為日期
target_3$in_date <- ymd(target_3$in_date)
head(target_3)
summary(target_3$in_date)

# 找出每人的首次住院日期
target_4 <- target_3[order(id, in_date)]
target_4 <- target_4[, .SD[1], by = .(id)]
head(target_4)
summary(target_4$in_date)

# 分析住院期的年份與月份交叉表
table(month(target_4$in_date), year(target_4$in_date))

# 取出首次在4-9月的病患
target_5 <- target_4[month(in_date) %in% c(4:9)]
table(month(target_5$in_date))
head(target_5)
nrow(target_5)
summary(target_5)

# 檔案整理
target <- target_5

target_id <- target_5[, .(id, in_date)]

rm(target_1, target_2, target_3, target_4, target_5)
gc()



# ~ 死亡登記 ----

# 讀取死亡登記檔 h_ost_death2014.csv
setwd(pathraw)
death_1 <- fread("h_ost_death2014.csv", encoding = "UTF-8", colClasses = "character")
head(death_1)

# 取出目標樣本的死亡資料
# 概念為將此資料的ID與目標樣本的ID合併取交集
death_2 <- merge.data.table(death_1, target_id, by = c("id"))
head(death_2)

# 擷取需要使用欄位
death_3 <- death_2[, .(id, d_date)]
head(death_3)

# 將d_date欄位由文字轉為日期
death_4 <- death_3[, .(id, death = 1, death_date = ymd(d_date))]
head(death_4)

# 檔案整理
death <- death_4
head(death)

rm(death_1, death_2, death_3, death_4)
gc()



# ~ 資料合併 ----

# target與death合併
pop_1 <- merge.data.table(target, death, by = c("id"), all.x = T)
head(pop_1)

# 沒有死亡登記的補0和2014-12-31
pop_2 <- pop_1[is.na(death), `:=`(death = 0, death_date = ymd("20141231"))]
head(pop_2)
summary(pop_2)

# 建立變項標註住院後90天內死亡
pop_3 <- pop_2[, `:=`(death_90day = 0)]
pop_3 <- pop_3[death == 1 & as.numeric(death_date - in_date) <= 90, `:=`(death_90day = 1)]
table(pop_3$death_90day)
head(pop_3)

# 檔案整理
pop <- pop_3
head(pop)

rm(pop_1, pop_2, pop_3)
rm(death)
gc()





### 基本資料 ----



# ~ 年齡性別 ----

# 讀取健保承保檔 h_nhi_enrol2014.csv
setwd(pathraw)
idfile_1 <- fread("h_nhi_enrol2014.csv", encoding = "UTF-8", colClasses = "character")
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
nhi_1 <- fread("h_nhi_enrol2014.csv", encoding = "UTF-8", colClasses = "character")
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
nhi_5 <- nhi_5[, `:=`(nhi_fee_range = cut(x = nhi_fee, breaks = c(0, 15840, 30000, 45000, 200000), labels = c(1:4), include.lowest = T))]
head(nhi_5)

# 創造投保金額級距的虛擬變數
nhi_6 <- dummy_cols(.data = nhi_5, select_columns = c("nhi_fee_range"))
head(nhi_6)

# 檔案整理
nhi <- nhi_6
head(nhi)

rm(nhi_1, nhi_2, nhi_3, nhi_4, nhi_5, nhi_6)
gc()





### 計算CCI ----



# ~ 門診診斷紀錄 ----

# 讀取門診費用檔 h_nhi_opdte2014.csv
setwd(pathraw)
cci_1_op <- fread("h_nhi_opdte2014.csv", encoding = "UTF-8", colClasses = "character")
head(cci_1_op)

# 擷取需要使用欄位
cci_1_op <- cci_1_op[, .(id, func_date = ymd(func_date), icd9cm_1, icd9cm_2, icd9cm_3)]
head(cci_1_op)

# 取出目標樣本的就醫資料
cci_1_op <- merge.data.table(cci_1_op, target_id, by = c("id"))
head(cci_1_op)

# 保留住院日(不含)以前的就醫紀錄
cci_1_op <- cci_1_op[func_date < in_date]
head(cci_1_op)

# 將診斷碼轉置成同一欄位
cci_1_op <- melt.data.table(
  data = cci_1_op, 
  id.vars = c("id", "func_date"), 
  measure.vars = c("icd9cm_1", "icd9cm_2", "icd9cm_3"), 
  variable.name = "icd", value.name = "codes")
head(cci_1_op)

nrow(cci_1_op)
nrow(cci_1_op[is.na(codes) | codes == ""])

cci_1_op <- cci_1_op[!(is.na(codes) | codes == "")]
nrow(cci_1_op)
table(cci_1_op$icd)



# ~ 住院診斷紀錄 ----

# 讀取住院費用檔 h_nhi_ipdte2014.csv
setwd(pathraw)
cci_1_ip <- fread("h_nhi_ipdte2014.csv", encoding = "UTF-8", colClasses = "character")
head(cci_1_ip)

# 擷取需要使用欄位
cci_1_ip <- cci_1_ip[, .(id, func_date = ymd(in_date), icd9cm_1, icd9cm_2, icd9cm_3, icd9cm_4, icd9cm_5)]
head(cci_1_ip)

# 取出目標樣本的就醫資料
cci_1_ip <- merge.data.table(cci_1_ip, target_id, by = c("id"))
head(cci_1_ip)

# 保留住院日(不含)以前的就醫紀錄
cci_1_ip <- cci_1_ip[func_date < in_date]
head(cci_1_ip)

# 將診斷碼轉置成同一欄位
cci_1_ip <- melt.data.table(
  data = cci_1_ip, 
  id.vars = c("id", "func_date"), 
  measure.vars = c("icd9cm_1", "icd9cm_2", "icd9cm_3", "icd9cm_4", "icd9cm_5"), 
  variable.name = "icd", value.name = "codes")
head(cci_1_ip)

nrow(cci_1_ip)
nrow(cci_1_ip[is.na(codes) | codes == ""])

cci_1_ip <- cci_1_ip[!(is.na(codes) | codes == "")]
nrow(cci_1_ip)
table(cci_1_ip$icd)



# ~ 資料整理 ----

# 給予權重
cci_1_op$dxw <- 1 
cci_1_ip$dxw <- 2

head(cci_1_op)
head(cci_1_ip)

# 疊加門診住院資訊
cci_1 <- rbind(cci_1_op, cci_1_ip)
table(cci_1$dxw)
rm(cci_1_op, cci_1_ip)

# 標註CCI各別項目
cci_2 <- cci_1[, .(id, func_date, codes, dxw)]
cci_2 <- unique(cci_2)
head(cci_2)

cci_2 <- cci_2[, `:=`(disease_cate = "none")]
head(cci_2)

cci_2 <- cci_2[grepl("^410|^412", codes), `:=`(disease_cate = "mi")]
cci_2 <- cci_2[grepl("^39891|^40201|^40211|^40291|^4040[13]|^4041[13]|^40491|^40493|^425[4-9]|^428", codes), `:=`(disease_cate = "chd")]
cci_2 <- cci_2[grepl("^0930|^4373|^440|^441|^443[1-9]|^4471|^557[19]|^V434", codes), `:=`(disease_cate = "pvd")]
cci_2 <- cci_2[grepl("^36234|^43[0-8]", codes), `:=`(disease_cate = "cvd")]
cci_2 <- cci_2[grepl("^290|^2941|^3312", codes), `:=`(disease_cate = "dementia")]
cci_2 <- cci_2[grepl("^416[89]|^49[0-6]|^50[0-5]|^5064|^508[18]", codes), `:=`(disease_cate = "cpd")]
cci_2 <- cci_2[grepl("^4465|^710[0-4]|^714[0-2]|^7148|^725", codes), `:=`(disease_cate = "rd")]
cci_2 <- cci_2[grepl("^53[1-4]", codes), `:=`(disease_cate = "pud")]
cci_2 <- cci_2[grepl("^0702[23]|^0703[23]|^07044|^07054|^0706|^0709|^570|^571|^573[3489]|^V427", codes), `:=`(disease_cate = "liver_mild")]
cci_2 <- cci_2[grepl("^250[0-389]", codes), `:=`(disease_cate = "dmnc")]
cci_2 <- cci_2[grepl("^250[4-7]", codes), `:=`(disease_cate = "dmwc")]
cci_2 <- cci_2[grepl("^3341|^342|^343|^344[0-69]", codes), `:=`(disease_cate = "plegia")]
cci_2 <- cci_2[grepl("^40301|^40311|^40391|^4040[23]|^4041[23]|^4049[23]|^582|^583[0-7]|^585|^586|^5880|^V420|^V451|^V56", codes), `:=`(disease_cate = "renal")]
cci_2 <- cci_2[grepl("^14[0-9]|^15[0-9]|^16[0-9]|^17[0-24-6]|^18[0-9]|^19[0-5]|^20[0-8]|^2386", codes), `:=`(disease_cate = "cancer")]
cci_2 <- cci_2[grepl("^456[0-2]|^572[2-8]", codes), `:=`(disease_cate = "liver_severe")]
cci_2 <- cci_2[grepl("^19[6-9]", codes), `:=`(disease_cate = "meta")]
cci_2 <- cci_2[grepl("^04[2-4]", codes), `:=`(disease_cate = "aids")]

table(cci_2$disease_cate)

cci_2 <- cci_2[disease_cate != "none"]
table(cci_2$disease_cate)

# 每一診斷種類在每一天只計算一次，如果門診和住院都有的話以住院為主
cci_3 <- cci_2[, .(id, func_date, disease_cate, codes, dxw)]
head(cci_3)

cci_3 <- cci_3[, .(dxw = max(dxw)), by = .(id, func_date, disease_cate)]
head(cci_3)

# 計算各別過去病史的權重加總
cci_4 <- cci_3[, .(dxw = sum(dxw)), by = .(id, disease_cate)]
head(cci_4)
summary(cci_4$dxw)

# 標註符合條件的過去病史
cci_4 <- cci_4[, `:=`(hx = 0)][2 <= dxw, `:=`(hx = 1)]
head(cci_4)
table(cci_4$hx)

# 留下符合條件的過去病史
cci_5 <- cci_4[hx == 1]
cci_5 <- dcast(cci_5, id ~ disease_cate, value.var = "hx")
head(cci_5)

# 補抓不到的內容
cci_list <- c("mi", "chd", "pvd", "cvd", "dementia", "cpd", "rd", "pud", "liver_mild", "dmnc", "dmwc", "plegia", "renal", "cancer", "liver_severe", "meta", "aids")

print(cci_list[!(cci_list %in% colnames(cci_5))])

cci_5 <- cci_5[, `:=`(dmwc = 0, aids = 0)]
head(cci_5)

# 補齊樣本
cci_6 <- merge.data.table(target_id[, .(id)], cci_5, by = c("id"), all.x = T)
head(cci_6)

# 補0
cci_6 <- setnafill(cci_6, fill = 0, cols = cci_list)
head(cci_6)

summary(cci_6)

# 計算分數
cci_6 <- cci_6[, `:=`(ccix = mi * 1 + chd * 1 + pvd * 1 + cvd * 1 + dementia * 1 + cpd * 1 + rd * 1 + pud * 1 + liver_mild * 1 + dmnc * 1 + dmwc * 2 + plegia * 2 + renal * 2 + cancer * 2 + liver_severe * 3 + meta * 6 + aids * 6)]
cci_6 <- cci_6[, `:=`(ccix_gp = 1)]
cci_6 <- cci_6[ccix == 1 | ccix == 2, `:=`(ccix_gp = 2)]
cci_6 <- cci_6[ccix >= 3, `:=`(ccix_gp = 3)]

table(cci_6$ccix)
table(cci_6$ccix_gp)

# 虛擬變項
cci_7 <- dummy_cols(cci_6, c("ccix_gp"))
head(cci_7)

# 檔案整理
cci <- cci_7
head(cci)
summary(cci)

rm(cci_1, cci_2, cci_3, cci_4, cci_5, cci_6, cci_7)
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

# 合併CCI資料
dtmerge <- merge.data.table(dtmerge, cci, by = c("id"), all.x = T)
head(dtmerge)



# ~ 排除條件 ----

dtselect <- dtmerge

# 排除基本資料不齊全者
summary(dtselect$age)

dtselect <- dtselect[!(is.na(age))]

# 檢查是否有死亡日期早於住院日期
nrow(dtselect[death == 1 & death_date < in_date])

# 可以用來分析的資料集
head(dtselect)

# 將整理好的資料檔儲存下來
setwd(pathuse)
fwrite(dtselect, "dtfinal.csv")





### 統計分析 ----



# ~ 樣本特性比較 ----

# table 1

table_vars <- c("male", "age", "nhi_fee_range", "ccix", "ccix_gp", "mi", "chd", "pvd", "cvd", "dementia", "cpd", "rd", "pud", "liver_mild", "dmnc", "dmwc", "plegia", "renal", "cancer", "liver_severe", "meta", "aids")

t1 <- CreateTableOne(
  vars = table_vars,
  factorVars = table_vars[!(table_vars %in% c("age", "ccix"))],
  strata = "death_90day",
  data = dtselect
)

print(t1)



# ~ Logistic regression ----

# model - only CCI index
m0 <- glm(death_90day ~ ccix, data = dtselect, family = binomial("logit"))
summary(m0)

cbind("OR" = exp(coef(m0)), exp(confint(m0)), "P value" = summary(m0)$coefficients[, 4])



# model - overall CCI index
m1 <- glm(death_90day ~ male + age + nhi_fee_range_2 + nhi_fee_range_3 + nhi_fee_range_4 + ccix, data = dtselect, family = binomial("logit"))
summary(m1)

cbind("OR" = exp(coef(m1)), exp(confint(m1)), "P value" = summary(m1)$coefficients[, 4])



# model - every CCI factors
m2 <- glm(death_90day ~ male + age + nhi_fee_range_2 + nhi_fee_range_3 + nhi_fee_range_4 + mi + chd + pvd + cvd + dementia + cpd + rd + pud + liver_mild + dmnc + dmwc + plegia + renal + cancer + liver_severe + meta + aids, data = dtselect, family = binomial("logit"))
summary(m2)

cbind("OR" = exp(coef(m2)), exp(confint(m2)), "P value" = summary(m2)$coefficients[, 4])



# model - every CCI factors omit some with low prevalence(<1%)
m3_pre_check <- dtselect[, .(mi, chd, pvd, cvd, dementia, cpd, rd, pud, liver_mild, dmnc, dmwc, plegia, renal, cancer, liver_severe, meta, aids)]

lapply(m3_pre_check, mean)

m3 <- glm(death_90day ~ male + age + nhi_fee_range_2 + nhi_fee_range_3 + nhi_fee_range_4 + mi + chd +     + cvd + dementia + cpd + rd + pud + liver_mild + dmnc        + plegia + renal + cancer                + meta       , data = dtselect, family = binomial("logit"))

summary(m3)

cbind("OR" = exp(coef(m3)), exp(confint(m3)), "P value" = summary(m3)$coefficients[, 4])





### END ###
### Peter Pin-Sung Liu ###
### 劉品崧 ###
### psliu520@gmail.com ###