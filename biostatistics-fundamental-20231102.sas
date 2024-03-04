libname temp "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\biostatistics-fundamental";





/****************/
/* y = 連續變數 */
/****************/

proc print data = temp.bptrial(obs = 10);
run;

proc contents data = temp.bptrial;
run;

/* 描述性統計分析 */
/* 年齡、收縮壓前測、舒張壓前測的數值分布 */
proc means data = temp.bptrial n mean median qrange min p5 p25 p50 p75 p95 max;
  var age sbp_pre dbp_pre;
run;

proc univariate data = temp.bptrial;
  var sbp_pre;
  histogram sbp_pre / endpoints = 120 to 140 by 2;
run;

/* 皮爾森相關係數 Person’s r correlation coefficient */
/* 年齡、收縮壓前測、舒張壓前測、收縮壓後測、舒張壓後測的相關係數 */
proc corr data = temp.bptrial;
  var age sbp_pre dbp_pre sbp_post dbp_post;
run;

/* 獨立樣本 t 檢定 Independent t-test  */
/* 檢定兩個介入組別在收縮壓前測、舒張壓前測是否有差異 */
proc ttest data = temp.bptrial plots = none;
  class txgp; 
  var sbp_pre; 
run;

/* 成對樣本 t 檢定 Paired t-test  */
/* 檢定介入組的收縮壓前測與收縮壓後測是否有差異 */
proc ttest data = temp.bptrial plots = none;
  paired sbp_post * sbp_pre; 
  where txgp = 1;
run;

/* 檢定控制組的收縮壓前測與收縮壓後測是否有差異 */
proc ttest data = temp.bptrial plots = none;
  paired sbp_post * sbp_pre; 
  where txgp = 0;
run;

/* 變異數分析 ANOVA (Analysis of variance) 事後檢定 post-hoc test */
/* 檢定三個年齡層之間在收縮壓後測是否有差異 */
proc anova data = temp.bptrial;
  class agegp; 
  model sbp_post = agegp;
  means agegp / tukey;
quit;

/* 線性迴歸模型 - univariable analysis */
/* y = 收縮壓後測 x = 介入組別 */
proc reg data= temp.bptrial plots = none;
  model sbp_post = txgp;
quit;

/* 線性迴歸模型 - multivariable analysis */
/* y = 收縮壓後測 x = 介入組別 + 性別 + 年齡 + 生活品質量表分數前測 */
proc reg data= temp.bptrial plots = none;
  model sbp_post = txgp male age qol_scale_pre;
quit;




/****************/
/* y = 類別變數 */
/****************/

/* 描述性統計分析 */
/* 生活品質量表分數前測的次數分析 */
proc freq data = temp.bptrial;
  table qol_scale_pre;
run;

/* 長條圖 bar chart */
proc freq data = temp.bptrial;
  table qol_scale_pre / plots = freqplot;
run;

/* 交叉分析表 cross table 隱藏不必要的百分比 卡方檢定 χ2 test */
/* 組別與性別的交叉列聯表 */
proc freq data = temp.bptrial;
  table male * txgp / norow nocol nopercent chisq;
run;

/* 費氏精確檢定 Fisher exact test */
proc freq data = temp.bptrial;
  table male * txgp / fisher norow nocol nopercent expected;
run;

/* Logistic模型 - Logistic model - univariable analysis */
/* y = 生活品質不佳(分數 >= 5分)後測 x = 介入組別 */
proc logistic data = temp.bptrial;
  model qol_bad_post(event = "1") = txgp;
run;

/* Logistic模型 - Logistic model - multivariable analysis */
/* y = 生活品質不佳(分數 >= 5分)後測 x = 介入組別 + 性別 + 年齡 */
proc logistic data = temp.bptrial;
  model qol_bad_post(event = "1") = txgp male age;
run;

/* Logistic模型 - Logistic model - multivariable analysis and ROC plot */
proc logistic data = temp.bptrial plot = roc;
  model qol_bad_post(event = "1") = txgp male age;
run;




/****************/
/* 傾向分數配對 */
/****************/

/* 背景特質比較 */
proc tabulate data = temp.oacs_dm_complications;
  class oacs / descending;
  class male hyperlipidemia ckd cancer / descending;
  class index_year_gp;
  var age c2vs;

  table age, oacs * (mean std) / nocellmerge;
  table male, oacs * (n colpctn) / nocellmerge;
  table (c2vs hyperlipidemia ckd cancer), oacs * (n colpctn) / nocellmerge;
  table (index_year_gp), oacs * (n colpctn) / nocellmerge;
run;

/* 傾向分數計算 */
proc logistic data = temp.oacs_dm_complications;
  class noac(ref = '0');
  model noac = age male c2vs hyperlipidemia ckd cancer year_2014_2015 year_2016_2017;
  output out = temp.oacs_dm_complications_calculate predicted = ps;
run;

/* 傾向分數分布 */
proc means data = temp.oacs_dm_complications_calculate;
  class noac;
  var ps;
run;

proc sgplot data = temp.oacs_dm_complications_calculate;
  histogram ps / group = noac transparency = 0.5;
run;

/* 傾向分數配對 */
proc psmatch data = temp.oacs_dm_complications_calculate;
  class noac;
  psmodel noac(treated = "1") = age male c2vs hyperlipidemia ckd cancer year_2014_2015 year_2016_2017;
  match 
    method = greedy(k = 1 order = descending) 
    distance = lps 
    caliper = 0.2;
  output out(obs = match) = temp.oacs_dm_complications_matched matchid = _MatchID;
run;

proc sort data = temp.oacs_dm_complications_matched;
  by PS _MatchID noac;
run;

proc print data = temp.oacs_dm_complications_matched(obs = 10);
  var _MatchID id PS noac;
run;

/* 背景特質比較 */
proc tabulate data = temp.oacs_dm_complications_matched;
  class oacs / descending;
  class male hyperlipidemia ckd cancer / descending;
  class index_year_gp;
  var age c2vs;

  table age, oacs * (mean std) / nocellmerge;
  table male, oacs * (n colpctn) / nocellmerge;
  table (c2vs hyperlipidemia ckd cancer), oacs * (n colpctn) / nocellmerge;
  table (index_year_gp), oacs * (n colpctn) / nocellmerge;
run;




/************/
/* 存活分析 */
/************/ 

/* 發生事件 */
proc freq data = temp.oacs_dm_complications_matched;
  table event_occur * noac / norow nopercent;
run;

/* 觀察時間 */
proc means data = temp.oacs_dm_complications_matched;
  class noac;
  var event_ft;
run;

/* 計算分組觀察期間內結果事件發生率(每千人年) */
proc sql;
  select 
    noac, 
    count(*) as totN, 
    sum(event_occur) as totEVENT, 
    sum(event_ft) as totFU,
    (sum(event_occur) / sum(event_ft)) * 1000 as totIR
  from temp.oacs_dm_complications_matched
  group by noac
  order by noac desc;
quit;

/* 存活曲線 KM Curves 風險集人數 Number at risk */
proc lifetest data = temp.oacs_dm_complications_matched notable plots = survival(nocensor atrisk = 0 to 6 by 1 outside);
  time event_ft * event_occur(0);
  strata noac / test = logrank;
run;

/* Cox迴歸模型 Cox proportional hazard regression */
proc phreg data = temp.oacs_dm_complications_matched nosummary;
  model event_ft * event_occur(0) = noac / rl;
  strata _MatchID;
run;

/* 等比風險假設檢定 Test for proportional hazard assumption */
proc phreg data = temp.oacs_dm_complications_matched nosummary;
  model event_ft * event_occur(0) = noac noac_t / rl;
  strata _MatchID;

  noac_t = noac * log(event_ft);

  proportionality_test: test noac_t;
run;





/* END */
