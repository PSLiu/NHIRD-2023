/* 專案設定 */

/* 資料路徑 */
libname pathraw "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\study-practice-case-control\raw";
libname pathuse "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\study-practice-case-control\use";



/* 研究樣本 */

/* 讀取住院費用檔 h_nhi_ipdte2014 */
data target_1;
  set pathraw.h_nhi_ipdte2014;
run;
proc print data=target_1(obs = 5);
run;

/* 擷取需要使用欄位 */
data target_2;
   set target_1 (keep=id in_date hosp_id icd9cm_1 icd9cm_2 icd9cm_3 icd9cm_4 icd9cm_5);
run;
proc print data=target_2(obs = 5);
run;

/* 找出有目標診斷碼的(肺炎 pneumonia 480-486) */
data target_3;
   set target_2;
   if prxmatch('/^48[0-6]/', icd9cm_1) or
      prxmatch('/^48[0-6]/', icd9cm_2) or
      prxmatch('/^48[0-6]/', icd9cm_3) or
      prxmatch('/^48[0-6]/', icd9cm_4) or
      prxmatch('/^48[0-6]/', icd9cm_5) then output;
run;
proc print data=target_3(obs = 5);
run;

/* 將in_date欄位由文字轉為日期 */
data target_3;
   set target_3(rename = (in_date = in_date_chr));
   in_date = input(in_date_chr, yymmdd8.);
   format in_date yymmdd10.;
run;
proc print data=target_3(obs = 5);
run;

/* 找出每人的首次住院日期 */
proc sort data=target_3;
   by id in_date;
run;
data target_4;
   set target_3;
   by id;
   if first.id;
run;
proc print data=target_4(obs = 5);
run;
proc sql;
  select min(in_date) as date_range_min format yymmdd10.,  max(in_date) as date_range_max format yymmdd10.
  from target_4;
quit;

/* 分析住院期的年份與月份交叉表 */
data target_4;
  set target_4;

  in_year = year(in_date);
  in_month = month(in_date);
run;

proc freq data=target_4;
   tables in_month * in_year / norow nocol nopercent;
run;

/* 取出首次在4-9月的病患 */
data target_5;
   set target_4;
   where 4 <= in_month & in_month <= 9;
run;
proc freq data=target_5;
   tables in_month;
run;
proc print data=target_5(obs = 5);
run;
proc sql;
  select min(in_date) as date_range_min format yymmdd10.,  max(in_date) as date_range_max format yymmdd10.
  from target_5;
quit;

/* 檔案整理 */
proc sql;
  create table target as
  select id, in_date, hosp_id, icd9cm_1, icd9cm_2, icd9cm_3, icd9cm_4, icd9cm_5 
  from target_5;
quit;
proc print data=target(obs = 5);
run;
data target_id;
   set target_5;
   keep id in_date;
run;
proc print data=target_id(obs = 5);
run;

proc datasets library=work nodetails nolist;
   delete target_1 target_2 target_3 target_4 target_5;
quit;



/* 死亡登記 */

/* 讀取死亡登記檔 h_ost_death2014 */
data death_1;
  set pathraw.h_ost_death2014;
run;
proc print data=death_1(obs = 5);
run;

/* 取出目標樣本的死亡資料 */
/* 概念為將此資料的ID與目標樣本的ID合併取交集 */
proc sort data = death_1;
  by id;
proc sort data = target_id;
  by id;
data death_2;
   merge death_1(in=a) target_id(in=b);
   by id;
   if a = 1 & b = 1;
run;
proc print data=death_2(obs = 5);
run;

/* 擷取需要使用欄位 */
data death_3;
   set death_2;
   keep id d_date;
   rename d_date = d_date_chr;
run;
proc print data=death_3(obs = 5);
run;

/* 將d_date欄位由文字轉為日期 */
data death_4;
   set death_3;
   death_date = input(d_date_chr, yymmdd8.);
   death = 1;
   format death_date yymmdd10.;
run;
proc print data=death_4(obs = 5);
run;

proc sql;
  create table death as
  select id, death, death_date
  from death_4;
quit;
proc print data=death(obs = 5);
run;

proc datasets library=work nodetails nolist;
   delete death_:;
quit;




/* 資料合併 */
proc sort data = target;
  by id;
proc sort data = death;
  by id;
data pop_1;
  merge target(in=a) death(in=b);
  by id;
  if a = 1;
run;

/* 沒有死亡登記的補0和2014-12-31 */
data pop_2;
   set pop_1;
   if death = . then do;
      death = 0;
      death_date = '31DEC2014'd;
   end;
run;

/* 建立變項標註住院後90天內死亡 */
data pop_3;
   set pop_2;
   death_90day = 0;
   if death = 1 & (death_date - in_date) <= 90 then death_90day = 1;
run;

/* 檔案整理 */
data pop;
   set pop_3;
run;
proc freq data = pop;
  table death_90day;
run;

proc datasets library=work nodetails nolist;
   delete pop_:;
quit;



/* 讀取健保承保檔 h_nhi_enrol2014 */
data idfile_1;
  set pathraw.h_nhi_enrol2014;
run;
proc print data=idfile_1(obs = 5);
run;

/* 取出目標樣本的承保資料 */
/* 概念為將此資料的ID與目標樣本的ID合併取交集 */
proc sort data=idfile_1;
  by id;
proc sort data=target_id;
  by id;
data idfile_2;
  merge idfile_1(in = a) target_id(in = b);
  by id;
  if a = 1 & b = 1;
run;

/* 擷取需要使用欄位 */
data idfile_3;
  set idfile_2(rename = (id_birth_y = id_birth_y_chr));
  id_birth_y = input(id_birth_y_chr, 4.);
  keep id id_s id_birth_y prem_ym;
run;
proc print data=idfile_3(obs = 5);
run;

/* 檢查欄位內容 */
proc freq data=idfile_3;
  table id_s;
run;
proc means data=idfile_3;
  var id_birth_y;
run;

/* 保留性別編碼為1或2者 */
data idfile_4;
  set idfile_3;
  if id_s in ("1", "2");
run;
proc freq data=idfile_4;
  table id_s;
run;

/* 每人保留一筆 */
proc sort data=idfile_4;
  by id prem_ym;
run;
data idfile_5;
  set idfile_4;
  by id;
  if first.id then output;
  keep id id_s id_birth_y;
run;
proc print data=idfile_5(obs = 5);
run;

/* 虛擬變數 */
data idfile_6;
  set idfile_5;
  male = 0;
  if id_s = "1" then male = 1;
run;
proc print data=idfile_6(obs = 5);
run;

data idfile;
  set idfile_6;
run;

proc datasets library=work nodetails nolist;
   delete idfile_:;
quit;



/* 讀取健保承保檔 h_nhi_enrol2014 */
data nhi_1;
  set pathraw.h_nhi_enrol2014;
run;
proc print data=nhi_1(obs = 5);
run;

/* 擷取需要使用的欄位 */
data nhi_2;
   set nhi_1(rename = (prem_ym = prem_ym_chr id1_amt = nhi_fee));

   prem_ymd = input(cats(prem_ym_chr, "01"), yymmdd8.);
   format prem_ymd yymmdd10.;
run;
proc print data=nhi_2(obs = 5);
run;

/* 檢查欄位內容 */
proc means data=nhi_2;
   var nhi_fee;
run;

/* 取出目標樣本的投保資料 */
proc sort data=nhi_2;
  by id;
proc sort data=target_id;
  by id;
run;
data nhi_3;
   merge nhi_2(in = a) target_id(in = b);
   by id;
   if a = 1 & b = 1;
run;

/* 保留住院日(不含)以前的最近一筆投保紀錄 */
data nhi_4;
   set nhi_3;
   if prem_ymd < in_date;
   keep id in_date prem_ymd nhi_fee;
run;
proc sort data=nhi_4;
   by id prem_ymd;
run;
data nhi_4;
   set nhi_4;
   by id;
   if last.id;
run;
proc print data=nhi_4(obs = 5);
run;

/* 將投保金額以級距分類 */
/* 創造投保金額級距的虛擬變數 */
data nhi_5;
   set nhi_4;
   nhi_fee_range = 0;
        if nhi_fee <= 15840  then do; nhi_fee_range = 1; nhi_fee_range_1 = 1; nhi_fee_range_2 = 0; nhi_fee_range_3 = 0; nhi_fee_range_4 = 0; end;
   else if nhi_fee <= 30000  then do; nhi_fee_range = 2; nhi_fee_range_1 = 0; nhi_fee_range_2 = 1; nhi_fee_range_3 = 0; nhi_fee_range_4 = 0; end;
   else if nhi_fee <= 45000  then do; nhi_fee_range = 3; nhi_fee_range_1 = 0; nhi_fee_range_2 = 0; nhi_fee_range_3 = 1; nhi_fee_range_4 = 0; end;
   else if nhi_fee <= 200000 then do; nhi_fee_range = 4; nhi_fee_range_1 = 0; nhi_fee_range_2 = 0; nhi_fee_range_3 = 0; nhi_fee_range_4 = 1; end;
run;

data nhi;
  set nhi_5;
  drop prem_ymd in_date;
run;

proc datasets library=work nodetails nolist;
   delete nhi_:;
quit;





/* CCI素材 - 門診 */
data cci_1_op;
  set pathraw.h_nhi_opdte2014;
run;
proc print data=cci_1_op(obs=5);
run;

/* 擷取需要使用的欄位 */
data cci_1_op;
   set cci_1_op(rename = (func_date = func_date_chr));
   func_date = input(func_date_chr, yymmdd10.);
   format func_date yymmdd10.;
   keep id func_date icd9cm_1 icd9cm_2 icd9cm_3;
run;
proc print data=cci_1_op(obs=5);
run;

/* 取出目標樣本的就醫資料 */
proc sort data = cci_1_op;
  by id;
proc sort data = target_id;
  by id;
data cci_1_op;
   merge cci_1_op(in = a) target_id(in = b);
   by id;
   if a = 1 & b = 1;
run;
proc print data=cci_1_op(obs=5);
run;

/* 保留住院日(不含)以前的就醫紀錄 */
data cci_1_op;
   set cci_1_op;
   where func_date < in_date;
run;
proc print data=cci_1_op(obs=5);
run;

/* 將診斷碼轉置成同一欄位 */
proc sql;
  create table cci_1_op as
            select id, func_date, icd9cm_1 as codes from cci_1_op where icd9cm_1 ^= ""
  union all select id, func_date, icd9cm_2 as codes from cci_1_op where icd9cm_2 ^= ""
  union all select id, func_date, icd9cm_3 as codes from cci_1_op where icd9cm_3 ^= "";
quit;
proc print data=cci_1_op(obs=5);
run;



/* CCI素材 - 住院 */
data cci_1_ip;
  set pathraw.h_nhi_ipdte2014;
run;
proc print data=cci_1_ip(obs=5);
run;

/* 擷取需要使用的欄位 */
data cci_1_ip;
   set cci_1_ip(rename = (in_date = func_date_chr));
   func_date = input(func_date_chr, yymmdd10.);
   format func_date yymmdd10.;
   keep id func_date icd9cm_1 icd9cm_2 icd9cm_3 icd9cm_4 icd9cm_5;
run;
proc print data=cci_1_ip(obs=5);
run;

/* 取出目標樣本的就醫資料 */
proc sort data = cci_1_ip;
  by id;
proc sort data = target_id;
  by id;
data cci_1_ip;
   merge cci_1_ip(in = a) target_id(in = b);
   by id;
   if a = 1 & b = 1;
run;
proc print data=cci_1_ip(obs=5);
run;

/* 保留住院日(不含)以前的就醫紀錄 */
data cci_1_ip;
   set cci_1_ip;
   where func_date < in_date;
run;
proc print data=cci_1_ip(obs=5);
run;

/* 將診斷碼轉置成同一欄位 */
proc sql;
  create table cci_1_ip as
            select id, func_date, icd9cm_1 as codes from cci_1_ip where icd9cm_1 ^= ""
  union all select id, func_date, icd9cm_2 as codes from cci_1_ip where icd9cm_2 ^= ""
  union all select id, func_date, icd9cm_3 as codes from cci_1_ip where icd9cm_3 ^= ""
  union all select id, func_date, icd9cm_4 as codes from cci_1_ip where icd9cm_4 ^= ""
  union all select id, func_date, icd9cm_5 as codes from cci_1_ip where icd9cm_5 ^= "";
quit;
proc print data=cci_1_ip(obs=5);
run;



/* CCI資料整理 */

/* 給予權重 */
data cci_1_op; 
  set cci_1_op; 
  dxw = 1; 
run;
data cci_1_ip; 
  set cci_1_ip; 
  dxw = 2; 
run;

/* 疊加門診住院資訊 */
data cci_1; 
  set cci_1_op cci_1_ip;
run;
proc freq data=cci_1; 
  table dxw; 
run;

/* 標註CCI各別項目 */
data cci_2; 
  set cci_1; 
run;
proc sort data=cci_2 nodupkey;
  by id func_date codes dxw;
run;
data cci_2; 
  set cci_2; 
  disease_cate = "nonecategory"; 
run;
proc print data=cci_2(obs=5);
run;

data cci_2; 
  set cci_2;
  if prxmatch('/^410|^412/', codes) then disease_cate = "mi";
  if prxmatch('/^39891|^40201|^40211|^40291|^4040[13]|^4041[13]|^40491|^40493|^425[4-9]|^428/', codes) then disease_cate = "chd";
  if prxmatch('/^0930|^4373|^440|^441|^443[1-9]|^4471|^557[19]|^V434/', codes) then disease_cate = "pvd";
  if prxmatch('/^36234|^43[0-8]/', codes) then disease_cate = "cvd";
  if prxmatch('/^290|^2941|^3312/', codes) then disease_cate = "dementia";
  if prxmatch('/^416[89]|^49[0-6]|^50[0-5]|^5064|^508[18]/', codes) then disease_cate = "cpd";
  if prxmatch('/^4465|^710[0-4]|^714[0-2]|^7148|^725/', codes) then disease_cate = "rd";
  if prxmatch('/^53[1-4]/', codes) then disease_cate = "pud";
  if prxmatch('/^0702[23]|^0703[23]|^07044|^07054|^0706|^0709|^570|^571|^573[3489]|^V427/', codes) then disease_cate = "liver_mild";
  if prxmatch('/^250[0-389]/', codes) then disease_cate = "dmnc";
  if prxmatch('/^250[4-7]/', codes) then disease_cate = "dmwc";
  if prxmatch('/^3341|^342|^343|^344[0-69]/', codes) then disease_cate = "plegia";
  if prxmatch('/^40301|^40311|^40391|^4040[23]|^4041[23]|^4049[23]|^582|^583[0-7]|^585|^586|^5880|^V420|^V451|^V56/', codes) then disease_cate = "renal";
  if prxmatch('/^14[0-9]|^15[0-9]|^16[0-9]|^17[0-24-6]|^18[0-9]|^19[0-5]|^20[0-8]|^2386/', codes) then disease_cate = "cancer";
  if prxmatch('/^456[0-2]|^572[2-8]/', codes) then disease_cate = "liver_severe";
  if prxmatch('/^19[6-9]/', codes) then disease_cate = "meta";
  if prxmatch('/^04[2-4]/', codes) then disease_cate = "aids";
run;

proc freq data=cci_2; 
  table disease_cate; 
run;
data cci_2; 
  set cci_2;
  where disease_cate ^= "nonecategory"; 
run;
proc print data=cci_2(obs=5);
run;
proc freq data=cci_2; 
  table disease_cate; 
run;

/* 每一診斷種類在每一天只計算一次，如果門診和住院都有的話以住院為主 */
data cci_3; 
  set cci_2;
run;
proc sort data=cci_3;
  by id func_date disease_cate dxw;
run;
data cci_3; 
  set cci_3; 
  by id func_date disease_cate;
  if last.disease_cate then output;
run;

/* 計算各別過去病史的權重加總 */
proc sql;
  create table cci_4 as
  select id, disease_cate, sum(dxw) as dxw
  from cci_3
  group id, disease_cate;
quit;

/* 標註符合條件的過去病史 */
data cci_4; 
  set cci_4; 
  hx = 0; if 2 <= dxw then hx = 1;
run;
proc print data=cci_4(obs=5);
run;
proc freq data=cci_4; 
  table hx; 
run;

/* 留下符合條件的過去病史 */
data cci_5; 
  set cci_4; 
  where hx = 1; 
run;
proc sort data = cci_5;
  by id;
run;
proc transpose data=cci_5 out=cci_5(drop=_name_);
  by id;
  var hx;
  id disease_cate;
run;
proc print data=cci_5(obs=5);
run;

/* 沒抓到的給0 */
data cci_5; 
  set cci_5;
  dmwc = 0;
  aids = 0;
run;
proc print data=cci_5(obs=5);
run;

/* 補齊樣本 */
proc sort data = cci_5;
  by id;
proc sort data = target_id;
  by id;
data cci_6;
   merge cci_5(in = a) target_id(in = b keep = id);
   by id;
   if b = 1;
run;
proc print data=cci_6(obs=5);
run;

/* 補0 */
data cci_6;
  set cci_6;

  array cx[*] mi chd pvd cvd dementia cpd rd pud liver_mild dmnc dmwc plegia renal cancer liver_severe meta aids;
  do i = 1 to 17;
    if cx[i] = . then cx[i] = 0;
  end;

  drop i;
run;

proc means data=cci_6;
  var mi chd pvd cvd dementia cpd rd pud liver_mild dmnc dmwc plegia renal cancer liver_severe meta aids;
run;

/* 計算分數 */
data cci_7;
  set cci_6;

  ccix = mi * 1 + chd * 1 + pvd * 1 + cvd * 1 + dementia * 1 + cpd * 1 + rd * 1 + pud * 1 + liver_mild * 1 + dmnc * 1 + dmwc * 2 + plegia * 2 + renal * 2 + cancer * 2 + liver_severe * 3 + meta * 6 + aids * 6;
run;
data cci_7;
  set cci_7;

       if ccix = 0       then do; ccix_gp = 1; ccix_gp_1 = 1; ccix_gp_2 = 0; ccix_gp_3 = 0; end;
  else if ccix in (1, 2) then do; ccix_gp = 2; ccix_gp_1 = 0; ccix_gp_2 = 1; ccix_gp_3 = 0; end;
  else if ccix >= 3      then do; ccix_gp = 3; ccix_gp_1 = 0; ccix_gp_2 = 0; ccix_gp_3 = 1; end;
run;
proc print data=cci_7(obs=5);
run;
proc freq data = cci_7;
  table ccix;
run;

data cci;
  set cci_7;
  keep id ccix ccix_gp ccix_gp_1 ccix_gp_2 ccix_gp_3 mi chd pvd cvd dementia cpd rd pud liver_mild dmnc dmwc plegia renal cancer liver_severe meta aids ;
run;
proc print data=cci(obs=5);
run;

proc datasets library=work nodetails nolist;
   delete cci_:;
quit;





/* 變數整合 */
data dtmerge;
  set pop;
run;
proc print data=dtmerge(obs=5);
run;

/* 合併基本資料 */
proc sort data=dtmerge;
  by id;
proc sort data=idfile;
  by id;
data dtmerge;
  merge dtmerge(in = a) idfile(in = b);
  by id;
  if a = 1;
  age = year(in_date) - id_birth_y;
run;
proc print data=dtmerge(obs=5);
run;

/* 合併保險資料 */
proc sort data=dtmerge;
  by id;
proc sort data=nhi;
  by id;
data dtmerge;
  merge dtmerge(in = a) nhi(in = b);
  by id;
  if a = 1;
run;
proc print data=dtmerge(obs=5);
run;

/* 合併CCI資料 */
proc sort data=dtmerge;
  by id;
proc sort data=cci;
  by id;
data dtmerge;
  merge dtmerge(in = a) cci(in = b);
  by id;
  if a = 1;
run;
proc print data=dtmerge(obs=5);
run;



/* 排除條件 */
data dtselect;
  set dtmerge;
run;

/* 排除基本資料不齊全者 */
proc freq data=dtselect;
  table id_s;
run;

data dtselect;
  set dtselect;
  if id_s = "" then delete;
run;

/* 檢查是否有死亡日期早於住院日期 */
data dtselect;
  set dtselect;
  if death = 1 & death_date < in_date then delete;
run;

/* 將整理好的資料檔儲存下來 */
data pathuse.dtfinal;
  set dtselect;
run;





/* 樣本描述性統計 */
proc tabulate data = dtselect;

  class death_90day;
  class male mi chd pvd cvd dementia cpd rd pud liver_mild dmnc dmwc plegia renal cancer liver_severe meta aids / descending;
  class nhi_fee_range ccix_gp;

  var age ccix;

  table age, death_90day * (mean std) / nocellmerge;
  table (male nhi_fee_range), death_90day * (n colpctn) / nocellmerge;
  table ccix, death_90day * (mean std) / nocellmerge;
  table ccix_gp, death_90day * (n colpctn) / nocellmerge;
  table (mi chd pvd cvd dementia cpd rd pud liver_mild dmnc dmwc plegia renal cancer liver_severe meta aids), death_90day * (n colpctn) / nocellmerge;
run;





/* Logistic regression */

/* model - only CCI index */
proc logistic data=dtselect;
  model death_90day(event = '1') = ccix;
run;

/* model - overall CCI index */
proc logistic data=dtselect;
  model death_90day(event = '1') = age male nhi_fee_range_2 nhi_fee_range_3 nhi_fee_range_4 ccix;
run;

/* model - every CCI factors */
proc logistic data=dtselect;
  model death_90day(event = '1') = age male nhi_fee_range_2 nhi_fee_range_3 nhi_fee_range_4 mi chd pvd cvd dementia cpd rd pud liver_mild dmnc dmwc plegia renal cancer liver_severe meta aids;
run;

/* model - every CCI factors omit some with low prevalence(<1%) */
proc logistic data=dtselect;
  model death_90day(event = '1') = age male nhi_fee_range_2 nhi_fee_range_3 nhi_fee_range_4 mi chd     cvd dementia cpd rd pud liver_mild dmnc      plegia renal cancer              meta     ;
run;




/* END */
/* Peter Pin-Sung Liu */
/*劉品崧 */
