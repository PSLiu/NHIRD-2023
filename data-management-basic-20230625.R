# ~ 專案設定 ----

# 下載data.table套件
install.packages("")

# 載入data.table套件
library(data.table)

# 讀取資料路徑
path_temp <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/data-management-basic/temp"



### 資料管理 ----



# ~ 資料讀取 ----

setwd(path_temp)
dt_1 <- fread("dt_1.csv")
dt_2 <- fread("dt_2.csv")
dt_3 <- fread("dt_3.csv")



# ~ 資料檢視 ----

# 就診資料
head(dt_1)
summary(dt_1)

# BMI測量
head(dt_2)
summary(dt_2)

# 就診資料 Dr Chang
head(dt_3)
summary(dt_3)



# ~ 資料篩選：取得想要的觀察值《指定條件》 ----

# 對單一欄位(向量)進行次數分析
table(dt_1$physician)

# physician變項的內容為 Lee
ot_select_physician_1 <- dt_1[physician == "Lee"]
head(ot_select_physician_1)
table(ot_select_physician_1$physician)

# physician變項的內容"不為" Lee
ot_select_physician_2 <- dt_1[!(physician == "Lee")]
ot_select_physician_3 <- dt_1[physician != "Lee"]

table(ot_select_physician_2$physician)
table(ot_select_physician_3$physician)



# ~ 資料篩選：取得想要的觀察值《比大小》 ----

# 對單一欄位(向量)進行描述性統計分析
summary(dt_1$sbp)

# sbp變項的內容為大於等於120
ot_select_sbp120 <- dt_1[120 <= sbp]
head(ot_select_sbp120)
summary(ot_select_sbp120$sbp)



# ~ 資料篩選：取得想要的觀察值《數值模式》 ----

summary(dt_1$dbp)

# dbp介於80到90之間的觀察值
ot_select_dbprange_1 <- dt_1[inrange(x = dbp, lower = 80, upper = 90)]
summary(ot_select_dbprange_1$dbp)

# dbp介於80到90之間的觀察值, "&" = AND 代表條件交集, "|" = OR 代表條件聯集
ot_select_dbprange_2 <- dt_1[(80 <= dbp) & (dbp <= 90)]
summary(ot_select_dbprange_2$dbp)

# dbp介於80到90之間的觀察值（不包含邊界）
ot_select_dbprange_3 <- dt_1[(80 < dbp) & (dbp < 90)] # 注意邊界
summary(ot_select_dbprange_3$dbp)



# ~ 資料篩選：取得想要的觀察值《字串模式》 ----

table(dt_1$physician)

# grepl
# 找出physician字串裡面含有H的觀察值
ot_select_string_2 <- dt_1[grepl(pattern = "H", x = physician)]
head(ot_select_string_2)
table(ot_select_string_2$physician)

# grepl + regexp 基本型態 ^ 代表開頭
ot_select_string_3 <- dt_1[grepl("^H", physician)]
head(ot_select_string_3)
table(ot_select_string_3$physician)

# grepl + regexp 基本型態 $ 代表開頭
ot_select_string_4 <- dt_1[grepl("g$", physician)]
head(ot_select_string_4)
table(ot_select_string_4$physician)

# grepl + regexp 變化型態
# 找出comorbidity字串裡面為433開頭或是434開頭的觀察值
ot_select_string_5 <- dt_1[grepl("^433|^434", comorbidity)]
head(ot_select_string_5)
table(ot_select_string_5$comorbidity)

# 找出comorbidity字串裡面為43開頭，後面接著3或4的觀察值
ot_select_string_6 <- dt_1[grepl("^43[34]", comorbidity)]
head(ot_select_string_6)
table(ot_select_string_6$comorbidity)



# ~ 資料篩選：取得想要的變項 ----

# 在j索引處寫一個句點，一對小括號，裡面寫入想要留下的變項名稱
ot_variable_select <- dt_1
ot_variable_select <- ot_variable_select[, .(patient_id, visit_date, comorbidity)]
head(ot_variable_select)



# ~ 資料排序：讓資料成為想要的順序 ----

ot_sort <- dt_1
head(ot_sort)

# 依照就醫日期排序，預設為遞增（ascending）排序
ot_sort <- ot_sort[order(visit_date)]
head(ot_sort)

# 依照多個排序欄位之間以逗點隔開
ot_sort <- ot_sort[order(physician, patient_id)]
head(ot_sort)

# 若要改為遞減（descending）排序，在變項前面加上負號（-）
ot_sort <- ot_sort[order(id, -sbp)]
head(ot_sort)



# ~ 資料排序：依據排序將觀察值選出 ----

# 第1筆
ot_obs_order_1 <- dt_1
ot_obs_order_1 <- ot_obs_order_1[order(patient_id, visit_date)]
ot_obs_order_1 <- ot_obs_order_1[, .SD[1], by = .(patient_id)]
head(ot_obs_order_1)

# 第k筆，例如說K = 3
ot_obs_order_3 <- dt_1
ot_obs_order_3 <- ot_obs_order_3[order(patient_id, visit_date)]
ot_obs_order_3 <- ot_obs_order_3[, .SD[3], by = .(patient_id)]
head(ot_obs_order_3)

# 第N筆（最後一筆）
ot_obs_order_n <- dt_1
ot_obs_order_n <- ot_obs_order_n[order(patient_id, visit_date)]
ot_obs_order_n <- ot_obs_order_n[, .SD[.N], by = .(patient_id)]
head(ot_obs_order_n)



# ~ 資料修改：產生變數 ----

# 新增一個變項名為vip，內容為數值9
ot_variable_generate <- dt_1
ot_variable_generate <- ot_variable_generate[, `:=`(vip = 9)]
head(ot_variable_generate)
table(ot_variable_generate$vip)



# ~ 資料修改：更新變數 ----

# 把既有的變項內容修改（更新）為0
ot_variable_generate <- ot_variable_generate[, `:=`(vip = 0)]
head(ot_variable_generate)
table(ot_variable_generate$vip)



# ~ 資料修改：有條件更新變數 ----

# 如果是黃醫師看診的病人，都叫做VIP
ot_variable_generate <- ot_variable_generate[physician == "Huang", `:=`(vip = 1)]
head(ot_variable_generate)
table(ot_variable_generate$vip)



# ~ 資料修改：將長表轉置（transpose）成為寬表 ----

ot_transpose_long_wide <- dt_1[, .(patient_id, visit_date, physician, dose_modify)]
ot_transpose_long_wide <- ot_transpose_long_wide[order(patient_id, physician , visit_date)]
ot_transpose_long_wide <- ot_transpose_long_wide[, .SD[1], by = .(patient_id, physician)][patient_id == "A1"]
head(ot_transpose_long_wide)

# 依照patient_id相同的觀察值
# 把physician的內容轉換為新的欄位名稱
# 新的欄位內容由原有的dose_modify填入
ot_transpose_long_wide <- dcast.data.table(
  data = ot_transpose_long_wide, 
  formula = patient_id ~ physician, 
  value.var = "dose_modify")
head(ot_transpose_long_wide)



# ~ 資料修改：將寬表轉置（transpose）成為長表 ----

ot_transpose_wide_long <- ot_transpose_long_wide

# 依照patient_id相同的觀察值
# 把Hsu, Huang, Lee三個欄位轉置成為變項標籤，名為physician
# 原始變項內容則成為新的變數dose_modify
ot_transpose_wide_long <- melt.data.table(
  data = ot_transpose_wide_long, 
  id.vars = "patient_id", 
  measure.vars = c("Hsu", "Huang", "Lee"),
  variable.name = "physician", 
  value.name = "dose_modify",
  na.rm = T)
head(ot_transpose_wide_long)



# ~ 資料修改：去除重複 ----

# 依照data.table內所有的變數進行比對，刪除重複的觀察值
ot_unique_1 <- unique(dt_1[, .(patient_id)])
ot_unique_2 <- unique(dt_1[, .(patient_id, physician)])

print(ot_unique_1)
print(ot_unique_2)



# ~ 資料歸戶：依據變項內容相同者歸納資訊 ----

# 將分群統計資料存為新的變項，觀察值總數變少(與by的內容相同)
ot_aggregate_1 <- dt_1[, .(sbp_m = mean(sbp)), by = .(patient_id)]
head(ot_aggregate_1)



# ~ 資料歸戶：依據變項內容相同者統計資訊 ----

# 將分群統計資料存為新的變項，觀察值總數不變(remerge)
ot_aggregate_2 <- dt_1[, `:=`(sbp_m = mean(sbp)), by = .(patient_id)]
head(ot_aggregate_2, 10)



# ~ 資料合併：依據變項內容相同者串聯不同資料集 ----

head(dt_1[, .(patient_id, visit_date, sbp, dbp)])
head(dt_2[, .(patient_id, visit_date, bmi)])

ot_merge <- merge.data.table(dt_1, dt_2, by = c("patient_id", "visit_date"))
head(ot_merge[, .(patient_id, visit_date, sbp, dbp, bmi)])



# ~ 資料合併：依據變項名稱相同者堆疊不同資料集 ----

ot_bind_1 <- dt_1[, .(patient_id, visit_date, physician, sbp, dbp)]
ot_bind_2 <- dt_3[, .(patient_id, visit_date, physician, sbp, dbp)]

head(ot_bind_1)
head(ot_bind_2)

ot_bind <- rbind(ot_bind_1, ot_bind_2)
head(ot_bind)
nrow(ot_bind) == nrow(ot_bind_1) + nrow(ot_bind_2)



# ~ 工作環境整理 ----

# 刪除指定名稱的物件
rm(ot_merge)

# 用ls()函數找出環境中名稱符合指定模式的物件，刪除
rm(list = ls(pattern = "^ot_bind"))

# 用ls()函數找出環境中全部的物件名稱，刪除
rm(list = ls())

# 記憶體空間釋放
gc()





### END ###