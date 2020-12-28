LIBNAME lab3 "/folders/myfolders/Lab3";
 
  
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
guessingrows=100;
run;
proc import datafile="/folders/myfolders/Lab2/surv.csv" 
out=surv dbms=csv replace;
datarow=2;
getnames=yes;
guessingrows=100;
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

* Two equivalent ways to merge datasets 
* 1 using data step;
proc sort data=surv;by id;run;
proc sort data=dem;by id;run;
data all;
merge surv dem;
by id;
run;


* 2. merge datasets using SQL;
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




/* Example 1.2 */
data leuk;
input t c ;
datalines;
10 0
7 0
32 1
23 0
22 0
6 0
16 0
34 1
32 1
25 1
11 1
20 1
19 1
6 0
17 1
35 1
6 0
17 1
35 1
6 0
13 0
9 1
6 1
10 1
;
run;

* life table estimate;
 
proc lifetest data=leuk  method=lt intervals=(0 to 35 by 3)
                    plots=(s,h,p);
time t*c(1);
run;
 
* kaplan meier;

proc lifetest data=leuk atrisk outs=KMest plots=(s);
time t*c(1);
run; 


* NA;

proc lifetest data=leuk nelson   ;
time t*c(1);
ods output productlimitestimates=ple;
run; 

proc sgplot data = ple;
series x = t y = CumHaz;
 





 

