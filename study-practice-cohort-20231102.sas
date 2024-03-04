/* �M�׳]�w */

/* ��Ƹ��| */
libname pathraw "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\study-practice-cohort\raw";
libname pathuse "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\study-practice-cohort\use";



/* ��s�˥� */

/* Ū����|�O���� h_nhi_ipdte2014 */
data target_1;
  set pathraw.h_nhi_ipdte2014;
  keep id in_date hosp_id icd9cm_1 icd9cm_2 icd9cm_3 icd9cm_4 icd9cm_5;
  rename in_date = in_date_chr;
run;
proc print data=target_1(obs = 5);
run;

/* ��X���ؼжE�_�X��(�ͪ� pneumonia 480-486) */
data target_2;
   set target_1;
   if prxmatch('/^48[0-6]/', icd9cm_1) or
      prxmatch('/^48[0-6]/', icd9cm_2) or
      prxmatch('/^48[0-6]/', icd9cm_3) or
      prxmatch('/^48[0-6]/', icd9cm_4) or
      prxmatch('/^48[0-6]/', icd9cm_5) then output;
run;
proc print data=target_2(obs = 5);
run;

/* �Nin_date���Ѥ�r�ର��� */
data target_3;
   set target_2;
   in_date = input(in_date_chr, yymmdd8.);
   format in_date yymmdd10.;
   drop in_date_chr;
run;
proc print data=target_3(obs = 5);
run;

/* ��X�C�H��������|��� */
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

/* ���R��|�����~���P�����e�� */
data target_4;
  set target_4;

  in_year = year(in_date);
  in_month = month(in_date);
run;

proc freq data=target_4;
   tables in_month * in_year / norow nocol nopercent;
run;

/* ���X�����b4-9�몺�f�w */
data target_5;
   set target_4;
   if 4 <= in_month <= 9;
run;
proc freq data=target_5;
   tables in_month;
run;
proc print data=target_5(obs = 5);
run;

/* �ɮ׾�z */
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



/* ���`�n�O */

/* Ū�����`�n�O�� h_ost_death2014 */
data death_1;
  set pathraw.h_ost_death2014;
   keep id d_date;
   rename d_date = d_date_chr;
run;
proc print data=death_1(obs = 5);
run;

/* ���X�ؼм˥������`��� */
/* �������N����ƪ�ID�P�ؼм˥���ID�X�֨��涰 */
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

/* �Nd_date���Ѥ�r�ର��� */
data death_3;
   set death_2;
   death_date = input(d_date_chr, yymmdd8.);
   death = 1;
   format death_date yymmdd10.;
   drop d_date_chr;
run;
proc print data=death_3(obs = 5);
run;

proc sql;
  create table death as
  select id, death, death_date
  from death_3;
quit;
proc print data=death(obs = 5);
run;

proc datasets library=work nodetails nolist;
   delete death_:;
quit;




/* ��ƦX�� */
proc sort data = target;
  by id;
proc sort data = death;
  by id;
data pop_1;
  merge target(in=a) death(in=b);
  by id;
  if a = 1;
run;

/* �S�����`�n�O����0�M2014-12-31 */
data pop_2;
   set pop_1;
   if death = . then do;
      death = 0;
      death_date = '31DEC2014'd;
   end;
run;

/* �ɮ׾�z */
data pop;
   set pop_2;
run;

proc datasets library=work nodetails nolist;
   delete pop_1 pop_2;
quit;



/* Ū�����O�ӫO�� h_nhi_enrol2014 */
data idfile_1;
  set pathraw.h_nhi_enrol2014;
  keep id id_s id_birth_y prem_ym;
  rename id_birth_y = id_birth_y_chr;
run;
proc print data=idfile_1(obs = 5);
run;

/* ���X�ؼм˥����ӫO��� */
/* �������N����ƪ�ID�P�ؼм˥���ID�X�֨��涰 */
proc sort data=idfile_1;
  by id;
proc sort data=target_id;
  by id;
data idfile_2;
  merge idfile_1(in = a) target_id(in = b);
  by id;
  if a = 1 & b = 1;
run;

/* �^���ݭn�ϥ���� */
data idfile_3;
  set idfile_2;
  id_birth_y = input(id_birth_y_chr, 4.);
run;
proc print data=idfile_3(obs = 5);
run;

/* �ˬd��줺�e */
proc freq data=idfile_3;
  table id_s;
run;
proc means data=idfile_3;
  var id_birth_y;
run;

/* �O�d�ʧO�s�X��1��2�� */
data idfile_4;
  set idfile_3;
  if id_s in ("1", "2");
run;
proc freq data=idfile_4;
  table id_s;
run;

/* �C�H�O�d�@�� */
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

/* �����ܼ� */
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
   delete idfile_1 idfile_2 idfile_3 idfile_4 idfile_5 idfile_6;
quit;



/* Ū�����O�ӫO�� h_nhi_enrol2014 */
data nhi_1;
  set pathraw.h_nhi_enrol2014;
   keep id prem_ym id1_amt;
   rename 
    prem_ym = prem_ym_chr
    id1_amt = nhi_fee;
run;
proc print data=nhi_1(obs = 5);
run;

/* �^���ݭn�ϥΪ���� */
data nhi_2;
   set nhi_1;

   prem_ymd = input(cats(prem_ym_chr, "01"), yymmdd8.);
   format prem_ymd yymmdd10.;
   drop prem_ym_chr;
run;
proc print data=nhi_2(obs = 5);
run;

/* �ˬd��줺�e */
proc means data=nhi_2;
   var nhi_fee;
run;

/* ���X�ؼм˥�����O��� */
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

/* �O�d��|��(���t)�H�e���̪�@����O���� */
data nhi_4;
   set nhi_3;
   if prem_ymd < in_date;;
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

/* �N��O���B�H�ŶZ���� */
/* �гy��O���B�ŶZ�������ܼ� */
data nhi_5;
   set nhi_4;
   nhi_fee_range = 0;
        if nhi_fee <= 15840  then do; nhi_fee_range = 1; nhi_fee_range_1 = 1; nhi_fee_range_2 = 0; nhi_fee_range_3 = 0; end;
   else if nhi_fee <= 30000  then do; nhi_fee_range = 2; nhi_fee_range_1 = 0; nhi_fee_range_2 = 1; nhi_fee_range_3 = 0; end;
   else if nhi_fee <= 200000 then do; nhi_fee_range = 3; nhi_fee_range_1 = 0; nhi_fee_range_2 = 0; nhi_fee_range_3 = 1; end;
run;
proc print data=nhi_5(obs = 5);
run;

data nhi;
  set nhi_5;
  drop prem_ymd in_date;
run;
proc print data=nhi(obs = 5);
run;

proc datasets library=work nodetails nolist;
   delete nhi_1 nhi_2 nhi_3 nhi_4 nhi_5;
quit;





/* �}���f�v */

/* �Ī��M���� */
proc import 
  datafile = "C:\Users\liu\Documents\laboratory\project\HDMRP-2023\study-practice-cohort\use\code_dmdrug.csv"
  out = dm_1_dmdrug dbms = csv replace;
run;

/* ���E��O�� */
data dm_1_opdto;
  set pathraw.h_nhi_opdto2014;
  keep drug_no hosp_id fee_ym appl_type appl_date case_type seq_no;
run;
proc print data=dm_1_opdto(obs=5);
run;

/* ��X�}���f�Ī��B��(�ӳ���� & �Ī��N�X���涰) */
proc sort data = dm_1_opdto;
  by drug_no;
proc sort data = dm_1_dmdrug;
  by drug_no;
data dm_1_opdto;
  merge dm_1_opdto(in = a) dm_1_dmdrug(in = b);
  by drug_no;
  if a = 1 & b = 1;
run;



/* ���E�O���� */
data dm_1_opdte;
  set pathraw.h_nhi_opdte2014;
  keep id func_date hosp_id fee_ym appl_type appl_date case_type seq_no;
  rename func_date = func_date_chr;
run;
proc print data=dm_1_opdte(obs=5);
run;

/* �^���ݭn�ϥΪ���� */
data dm_1_opdte;
   set dm_1_opdte;
   func_date = input(func_date_chr, yymmdd10.);
   format func_date yymmdd10.;
   drop func_date_chr;
run;
proc print data=dm_1_opdte(obs=5);
run;

/* ���X�ؼм˥����N���� */
proc sort data = dm_1_opdte;
  by id;
proc sort data = target_id;
  by id;
data dm_1_opdte;
   merge dm_1_opdte(in = a) target_id(in = b);
   by id;
   if a = 1 & b = 1;
run;
proc print data=dm_1_opdte(obs=5);
run;

/* �O�d��|��(���t)�H�e���N����� */
data dm_1_opdte;
   set dm_1_opdte;
   if func_date < in_date;
run;
proc print data=dm_1_opdte(obs=5);
run;

/* �X�֥ӳ�����O */
proc sort data = dm_1_opdte;
  by hosp_id fee_ym appl_type appl_date case_type seq_no;
proc sort data = dm_1_opdto;
  by hosp_id fee_ym appl_type appl_date case_type seq_no;
data dm_1;
   merge dm_1_opdte(in = a) dm_1_opdto(in = b);
   by hosp_id fee_ym appl_type appl_date case_type seq_no;
   if a = 1 & b = 1;
run;
proc print data=dm_1(obs=5);
run;

data dm_2;
  set dm_1;

  dm = 1;

  keep id dm;
run;

proc sort data = dm_2 nodupkey;
  by id;
run;

/* �ɮ׾�z */
data dm;
  set dm_2;
  keep id dm;
run;
proc print data=dm(obs=5);
run;

proc datasets library=work nodetails nolist;
   delete dm_2 dm_1 dm_1_dmdrug dm_1_opdto dm_1_opdte;
quit;





/* �ܼƾ�X */
data dtmerge;
  set pop;
run;
proc print data=dtmerge(obs=5);
run;

/* �X�ְ򥻸�� */
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

/* �X�֫O�I��� */
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

/* �X��DM��� */
proc sort data=dtmerge;
  by id;
proc sort data=dm;
  by id;
data dtmerge;
  merge dtmerge(in = a) dm(in = b);
  by id;
  if a = 1;

  if dm = . then dm = 0;
run;
proc print data=dtmerge(obs=5);
run;

/* �s�����R�ܼ� */
data dtmerge;
  set dtmerge;

  event_occur = 0;
  if death = 1 then event_occur = 1;

  event_ft = death_date - in_date;
run;



/* �ư����� */
data dtselect;
  set dtmerge;
run;

/* �ư��򥻸�Ƥ������� */
proc freq data=dtselect;
  table id_s;
run;
data dtselect;
  set dtselect;
  if id_s = "" then delete;
run;
proc freq data=dtselect;
  table id_s;
run;

/* �ˬd�O�_�����`��������|��� */
data dtselect;
  set dtselect;
  if death = 1 & death_date <= in_date then delete;
run;

/* �N��z�n��������x�s�U�� */
data pathuse.dtfinal;
  set dtselect;
run;





/* �˥��y�z�ʲέp */
proc tabulate data = dtselect;

  class dm / descending;
  class male / descending;
  class nhi_fee_range;

  var age;

  table age, dm * (mean std) / nocellmerge;
  table (male nhi_fee_range), dm * (n colpctn) / nocellmerge;
run;

/* Incidence rate */
proc sql;
  select 
    dm, 
    count(*) as totN, 
    sum(event_occur) as totEVENT, 
    sum(event_ft) as totFU,
    (sum(event_occur) / sum(event_ft)) * 1000 as totIR
  from dtselect
  group by dm
  order by dm desc;
quit;

/* KM curves */
proc lifetest data = dtselect notable plots = survival(nocensor atrisk = 0 to 270 by 30 outside);
  time event_ft * event_occur(0);
  strata dm / test = logrank;
run;

/* Cox regression univariable model */
proc phreg data = dtselect;
  model event_ft * event_occur(0) = dm / rl;
run;

/* Cox regression multivariable model */
proc phreg data = dtselect;
  model event_ft * event_occur(0) = dm male age nhi_fee_range_2 nhi_fee_range_3 / rl;
run;





/* END */
/* Peter Pin-Sung Liu */
/*�B�~�] */
