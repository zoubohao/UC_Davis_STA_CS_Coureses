LIBNAME lab1 "/folders/myfolders/Lab2"; 
  
/***********************
Importing Data into SAS
*/

* Enter data directly;

data data1;
input student $ grade ;
datalines;
a 100
b 99
c 98
d 97
;
run;

* Import a file in common DBMS format: CSV, DLM, EXCEL,Rdata;
proc import datafile="/folders/myfolders/Lab2/dem.csv" 
out=dem dbms=csv replace;
datarow=2;
getnames=yes;
run;
proc import datafile="/folders/myfolders/Lab2/surv.csv" 
out=surv dbms=csv replace;
datarow=2;
getnames=yes;
run;

proc contents data=dem;run;
proc contents data=surv;run;




/*
Working with data
*/

* sort;
proc sort data=dem out=dem;
by AGE_DX descending YR_BRTH;
run;

* subsetting;
data dem1;
set dem;
if AGE_DX=0; *inclusion;
if AGE_DX>0 then delete;*exclusion;
run;

* select the first/last observation;
proc sort data=dem out=dem;
by id;
run;

data first;
set dem;
by id;
if first.id=1;
run;

* create new variables;
* labeling variables;
* rename variables;
data dem;
set dem;
time=AGE_DX-20;
label time="Age since treatment";
rename AGE_DX=age;
proc contents;
run;

* Formatting variables;
proc format;
value genderf
1="female"
2="male"
;
run;
data dem;
set dem;
format sex genderf.;
run;

/* 
Combinig multiple datasets
*/

* appending datasets horizontally;

* suppose...;
data data1 data2;
set dem;
if sex=1 then output data1;
else output data2;
run;
* want to stack;
data dem;
set data1 data2;
run;

* vertically;
data data1(keep=id) data2(drop=id);
set dem;
output data1;
output data2;
run;

data dem;
merge data1 data2;
run;


*merge datasets using SQL;
proc sql;
create table all as
select a.*,b.*
from surv a left join dem b
on a.id=b.id;
quit;

data all;
set all;
if srv_time_mon_flag in (0,1) then censor=0;else censor=1;
if age=0 and srv_time_mon<=12*3;
proc freq;tables censor;
run;


/*
Analyzing your data
*/


* KM;
proc lifetest data=all atrisk outs=KMest;
time srv_time_mon*censor(1);
run; 

* Graphing the Kaplan-Meier estimate;
proc lifetest data=all atrisk outs=KMest plots=survival(cb);
time srv_time_mon*censor(1);
run; 
 
* Nelson-Aalen;
proc lifetest data=all nelson ;
time srv_time_mon*censor(1);
ods output productlimitestimates=ple;
run; 

proc sgplot data = ple;
series x = srv_time_mon y = CumHaz;

* hazard function;
proc lifetest data=all plots=hazard notable;
time srv_time_mon*censor(1);
run;

 

