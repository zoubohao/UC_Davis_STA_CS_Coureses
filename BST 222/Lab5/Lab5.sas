LIBNAME lab5 "/folders/myfolders/Lab5";

*************** Hypothesis testing;

* logrank test for two or more groups;
proc import datafile="/folders/myfolders/Lab4/mydata.csv" 
out=mydata dbms=csv replace;
datarow=2;
getnames=yes;
guessingrows=100;
run;

proc lifetest data=mydata notable;
time time*censor(1); * which value is  censored;
strata group/test=(logrank);
run; 
* more info: https://support.sas.com/documentation/onlinedoc/stat/121/lifetest.pdf ;






************* save output to word/pdf files;
ods pdf file="/folders/myfolders/Lab5/result.pdf" style=pearl; *journal/pearl/rtf;

title "Checking";
proc freq data=mydata;
tables group*censor;
run;

title "Analysis";
proc lifetest data=mydata notable;
time time*censor(1); * which value is  censored;
strata group/test=(logrank);
run; 
ods pdf close;

 
 
 
 
 
 
 
 
ods pdf file="/folders/myfolders/Lab5/result.pdf" style=pearl; *journal/pearl/rtf;

title "Checking";
proc freq data=mydata;
tables group*censor;
run;

title "Analysis";
proc lifetest data=mydata notable;
time time*censor(1); * which value is  censored;
strata group/test=(logrank);
run; 

title "cumulative hazard";
proc lifetest data=mydata method=KM plots=hazard(cl) notable;
time time*censor(1); * which value is  censored;
strata group/order=internal;
run;
ods pdf close;
 
 
 
 
 
 
 
***************** use ODS to get create dataset from results;

proc lifetest data=mydata method=pl nelson plot=(s) atrisk outs=result;
   time time*censor(1);
   strata group;
   ods output productlimitestimates=NAdata;
run;



***************** Graphing using sgplot;

proc sgplot data=NAdata noautolegend;
   step x=time y=survival/group=group;   
   xaxis grid label="x" offsetmin=0.05 offsetmax=0.05;
   yaxis grid min=0 label="Survival Function";
run;
 
* add confidence intervals;
proc sgplot data=result noautolegend;
where  survival ne .;
   step x=time y=survival/group=group;   
   xaxis grid label="x" offsetmin=0.05 offsetmax=0.05;
   yaxis grid min=0 label="Survival Function with CI";
   * add confidence interbval;
   band x=time lower=SDF_LCL 
   upper=SDF_UCL / group=group transparency=0.5;
run;







