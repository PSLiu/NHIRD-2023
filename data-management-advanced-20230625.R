# ~ 專案設定 ----

# 讀取原始資料路徑資料路徑
path_base <- "C:/Users/liu/Documents/laboratory/database/mohw/data/csv_bind"

# 寫出整理完成之後資料路徑
path_temp <- "C:/Users/liu/Documents/laboratory/project/HDMRP-2023/data-management-advanced/temp"

# 載入套件 data.table
library(data.table)



### 資料管理 ----



# ~ 高齡者 ----

# 讀取健保承保
setwd(path_base)
dt_pop_1 <- fread("h_nhi_enrol2014.csv")

# 使用head函數確認資料讀取樣貌
head(dt_pop_1)

# 保留身分證號(id), 性別(id_s), 出生年(id_birth_y)三個欄位
dt_pop_2 <- dt_pop_1
dt_pop_2 <- dt_pop_2[, .(id, id_s, id_birth_y)]
head(dt_pop_2)
nrow(dt_pop_2)

# 將上面的檔案進行去重複
dt_pop_2 <- unique(dt_pop_2)
head(dt_pop_2)
nrow(dt_pop_2)

# 確認性別欄位編碼一定要是1或2
dt_pop_3 <- dt_pop_2
table(dt_pop_3$id_s)
dt_pop_3 <- dt_pop_3[id_s %in% c(1, 2)]
table(dt_pop_3$id_s)
nrow(dt_pop_3)

# 確認年齡(以2014年計算)是否有異常值
dt_pop_3 <- dt_pop_3[, `:=`(age = 2014 - id_birth_y)]
summary(dt_pop_3$age)

# 只保留65歲(含)以上的高齡族群
dt_pop_3 <- dt_pop_3[65 <= age]
summary(dt_pop_3$age)
nrow(dt_pop_3)

# 刪除不需要的物件並使用gc()釋放記憶體空間
rm(dt_pop_1)
rm(dt_pop_2)
gc()



# ~ 糖尿病診斷 ----

# 讀取門診費用檔所有變項以文字型態進行讀取(使用colClasses參數)
setwd(path_base)
dt_dm_1 <- fread("h_nhi_opdte2014.csv", colClasses = "character")

# 使用head函數確認資料讀取樣貌
head(dt_dm_1)

# 保留身分證號(id), 就醫日期(func_date), 三個診斷代碼(icd9cm_1~3)欄位
dt_dm_2 <- dt_dm_1
dt_dm_2 <- dt_dm_2[, .(id, func_date, icd9cm_1, icd9cm_2, icd9cm_3)]
head(dt_dm_2)

# 篩選具有糖尿病診斷的就醫紀錄
dt_dm_2 <- dt_dm_2[grepl("^250", icd9cm_1) | grepl("^250", icd9cm_2) | grepl("^250", icd9cm_3)]
nrow(dt_dm_2)
head(dt_dm_2)

# 保留身分證號(id), 就醫日期(func_date)欄位並進行去重複
dt_dm_2 <- dt_dm_2[, .(id, func_date)]
head(dt_dm_2)
nrow(dt_dm_2)
dt_dm_2 <- unique(dt_dm_2)
nrow(dt_dm_2)

# 計算每人總共有幾次具有糖尿病診斷的就診並命名為dm_freq
dt_dm_3 <- dt_dm_2[, .(dm_freq = .N), by = .(id)]
head(dt_dm_3)
nrow(dt_dm_3)

# 使用summary函數描述診斷次數的分布
summary(dt_dm_3$dm_freq)

# 只保留至少2次門診就醫紀錄的樣本
dt_dm_3 <- dt_dm_3[2 <= dm_freq]
nrow(dt_dm_3)
summary(dt_dm_3$dm_freq)

# 僅保留DM病人的身分證號(id)
dt_dm_4 <- dt_dm_3[, .(id)]
head(dt_dm_4)

# 加上一個欄位dm_diagnosis = 1
dt_dm_4 <- dt_dm_4[, `:=`(dm_diagnosis = 1)]
head(dt_dm_4)

# 刪除不需要的物件並使用gc()釋放記憶體空間
rm(dt_dm_1)
rm(dt_dm_2)
rm(dt_dm_3)
gc()



# ~ 合併族群總檔 ----

# 以65歲以上的高齡族群為主(all.x = T)合併糖尿病診斷檔
nrow(dt_pop_3)
dt_pop_4 <- merge.data.table(dt_pop_3, dt_dm_4, by = c("id"), all.x = T)
nrow(dt_pop_4)

# 使用head和summary確認現在資料的型態
head(dt_pop_4)
summary(dt_pop_4)

# 若DM就醫紀錄是NA的則把內容修改為0(補0 = 沒有糖尿就醫紀錄)
dt_pop_4 <- dt_pop_4[is.na(dm_diagnosis), `:=`(dm_diagnosis  = 0)]

# 使用head和summary確認修改後資料的型態
head(dt_pop_4)
summary(dt_pop_4)



### 統計分析 ----

# 高齡者總人數
nrow(dt_pop_4)

# DM人數次數分析
table(dt_pop_4$dm_diagnosis)

# DM盛行率
mean(dt_pop_4$dm_diagnosis)

# 將整理好的資料寫到path_temp裡面儲存
setwd(path_temp)
fwrite(dt_pop_4, "elder_dm.csv")





### END ###