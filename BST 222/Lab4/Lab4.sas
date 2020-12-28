LIBNAME lab4 "/folders/myfolders/Lab4";

proc import datafile="/folders/myfolders/Lab4/mydata.csv" 
out=mydata dbms=csv replace;
datarow=2;
getnames=yes;
guessingrows=100;
run;
 
/* 
Initial data checking and quality control
*/

* check your variables;
proc contents data=mydata;run;

* generate frequency table;
proc freq data=mydata;
tables group*censor;
run;

* check univariate data distribution;
proc univariate data=mydata;
var time x1 x2;
run;

* check average;
proc means data=mydata;
class group;
var time x1 x2;
run;

*Single sample t-test ;
proc ttest data=mydata;
class group;
var time;
run; 
* average observed survival time seems to be higher for diet group, consistent with simulation;
 
/*
KM, NA, estimators for survival functions, H(t), and options for confidence bands and confidence interval
*/

* Survival Function via KM;

proc lifetest data=mydata method=KM plots=survival(atrisk cl cb=hw);
time time*censor(1); * which value is  censored;
strata group;
run; 
* specifies the method to be used to compute the survival function estimates: breslow for NA of S(t) ;
* nelson: add NA estimate for H(t);
* strata=panel: panel plot or not;

 
 
* Cumulative Hazard Function via Nelson-Aalen;
proc lifetest data=mydata  nelson method=breslow plots=logsurv;
time time*censor(1); * which value is  censored;
strata group;
ods output productlimitestimates=NAest;
run; 

 
* smoothed hazard function;
proc lifetest data=mydata method=KM plots=hazard(cl) notable;
time time*censor(1); * which value is  censored;
strata group/order=internal;
run;

 