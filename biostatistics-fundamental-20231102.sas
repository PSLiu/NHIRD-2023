libname temp "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\biostatistics-fundamental";





/****************/
/* y = �s���ܼ� */
/****************/

proc print data = temp.bptrial(obs = 10);
run;

proc contents data = temp.bptrial;
run;

/* �y�z�ʲέp���R */
/* �~�֡B���Y���e���B�αi���e�����ƭȤ��� */
proc means data = temp.bptrial n mean median qrange min p5 p25 p50 p75 p95 max;
  var age sbp_pre dbp_pre;
run;

proc univariate data = temp.bptrial;
  var sbp_pre;
  histogram sbp_pre / endpoints = 120 to 140 by 2;
run;

/* �ֺ��ˬ����Y�� Person��s r correlation coefficient */
/* �~�֡B���Y���e���B�αi���e���B���Y������B�αi������������Y�� */
proc corr data = temp.bptrial;
  var age sbp_pre dbp_pre sbp_post dbp_post;
run;

/* �W�߼˥� t �˩w Independent t-test  */
/* �˩w��Ӥ��J�էO�b���Y���e���B�αi���e���O�_���t�� */
proc ttest data = temp.bptrial plots = none;
  class txgp; 
  var sbp_pre; 
run;

/* ����˥� t �˩w Paired t-test  */
/* �˩w���J�ժ����Y���e���P���Y������O�_���t�� */
proc ttest data = temp.bptrial plots = none;
  paired sbp_post * sbp_pre; 
  where txgp = 1;
run;

/* �˩w����ժ����Y���e���P���Y������O�_���t�� */
proc ttest data = temp.bptrial plots = none;
  paired sbp_post * sbp_pre; 
  where txgp = 0;
run;

/* �ܲ��Ƥ��R ANOVA (Analysis of variance) �ƫ��˩w post-hoc test */
/* �˩w�T�Ӧ~�ּh�����b���Y������O�_���t�� */
proc anova data = temp.bptrial;
  class agegp; 
  model sbp_post = agegp;
  means agegp / tukey;
quit;

/* �u�ʰj�k�ҫ� - univariable analysis */
/* y = ���Y����� x = ���J�էO */
proc reg data= temp.bptrial plots = none;
  model sbp_post = txgp;
quit;

/* �u�ʰj�k�ҫ� - multivariable analysis */
/* y = ���Y����� x = ���J�էO + �ʧO + �~�� + �ͬ��~��q����ƫe�� */
proc reg data= temp.bptrial plots = none;
  model sbp_post = txgp male age qol_scale_pre;
quit;




/****************/
/* y = ���O�ܼ� */
/****************/

/* �y�z�ʲέp���R */
/* �ͬ��~��q����ƫe�������Ƥ��R */
proc freq data = temp.bptrial;
  table qol_scale_pre;
run;

/* ������ bar chart */
proc freq data = temp.bptrial;
  table qol_scale_pre / plots = freqplot;
run;

/* ��e���R�� cross table ���ä����n���ʤ��� �d���˩w �q2 test */
/* �էO�P�ʧO����e�C�p�� */
proc freq data = temp.bptrial;
  table male * txgp / norow nocol nopercent chisq;
run;

/* �O���T�˩w Fisher exact test */
proc freq data = temp.bptrial;
  table male * txgp / fisher norow nocol nopercent expected;
run;

/* Logistic�ҫ� - Logistic model - univariable analysis */
/* y = �ͬ��~�褣��(���� >= 5��)��� x = ���J�էO */
proc logistic data = temp.bptrial;
  model qol_bad_post(event = "1") = txgp;
run;

/* Logistic�ҫ� - Logistic model - multivariable analysis */
/* y = �ͬ��~�褣��(���� >= 5��)��� x = ���J�էO + �ʧO + �~�� */
proc logistic data = temp.bptrial;
  model qol_bad_post(event = "1") = txgp male age;
run;

/* Logistic�ҫ� - Logistic model - multivariable analysis and ROC plot */
proc logistic data = temp.bptrial plot = roc;
  model qol_bad_post(event = "1") = txgp male age;
run;




/****************/
/* �ɦV���ưt�� */
/****************/

/* �I���S���� */
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

/* �ɦV���ƭp�� */
proc logistic data = temp.oacs_dm_complications;
  class noac(ref = '0');
  model noac = age male c2vs hyperlipidemia ckd cancer year_2014_2015 year_2016_2017;
  output out = temp.oacs_dm_complications_calculate predicted = ps;
run;

/* �ɦV���Ƥ��� */
proc means data = temp.oacs_dm_complications_calculate;
  class noac;
  var ps;
run;

proc sgplot data = temp.oacs_dm_complications_calculate;
  histogram ps / group = noac transparency = 0.5;
run;

/* �ɦV���ưt�� */
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

/* �I���S���� */
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
/* �s�����R */
/************/ 

/* �o�ͨƥ� */
proc freq data = temp.oacs_dm_complications_matched;
  table event_occur * noac / norow nopercent;
run;

/* �[��ɶ� */
proc means data = temp.oacs_dm_complications_matched;
  class noac;
  var event_ft;
run;

/* �p������[����������G�ƥ�o�Ͳv(�C�d�H�~) */
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

/* �s�����u KM Curves ���I���H�� Number at risk */
proc lifetest data = temp.oacs_dm_complications_matched notable plots = survival(nocensor atrisk = 0 to 6 by 1 outside);
  time event_ft * event_occur(0);
  strata noac / test = logrank;
run;

/* Cox�j�k�ҫ� Cox proportional hazard regression */
proc phreg data = temp.oacs_dm_complications_matched nosummary;
  model event_ft * event_occur(0) = noac / rl;
  strata _MatchID;
run;

/* �����I���]�˩w Test for proportional hazard assumption */
proc phreg data = temp.oacs_dm_complications_matched nosummary;
  model event_ft * event_occur(0) = noac noac_t / rl;
  strata _MatchID;

  noac_t = noac * log(event_ft);

  proportionality_test: test noac_t;
run;





/* END */
