proc import datafile = "/folders/myshortcuts/MyFolder/colon.csv" out=colon;

*log-rank test for different treatments;
proc lifetest data=colon;
time time*status(0);
strata rx/ test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;
* trend test for different treatments(Lev, Lev-5fu);
proc import datafile = "/folders/myshortcuts/MyFolder/lev_lev+5fu.csv" out=lev_lev5fu;
proc lifetest data=lev_lev5fu;
time time*status(0);
strata rx/trend test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;

proc import datafile = "/folders/myshortcuts/MyFolder/lev_obs.csv" out=lev_obs;
proc lifetest data=lev_obs;
time time*status(0);
strata rx/trend test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;

* cox reg;
proc phreg data=colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx;
run;
* cox forward selction ;
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx sex age obstruct perfor adhere nodes differ extent surg
/selection=forward slentry=0.1 details;
run;

* Cox-snell residuals plot for original data;
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx age obstruct nodes differ extent surg;
output out=plot1_1 logsurv=logsurv1 /method = ch;

data plot1_1;
set plot1_1;
snell = -logsurv1;
cons = 1;

proc phreg data=plot1_1;
model snell*status(0) = cons;
output out = plot1_2 logsurv= logsurv2/method=ch;

data plot1_2;
set plot1_2;
cumhaz = - logsurv2;

proc sort data=plot1_2;
by snell;

proc sgplot data= plot1_2;
step y=cumhaz x=snell /MARKERFILLATTRS=(color="red");
lineparm x=0 y=0 slope=1; /** intercept, slope **/
label cumhaz = "Estimated Cumulative Hazard Rates";
label snell = "Residual";
run;

* Martingale Residuals for age (continuous variable);
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx obstruct differ extent surg nodes;
output out=plot2_1 RESMART = Martingale;

proc loess data=plot2_1;
model Martingale =age / direct;
run;


* Martingale Residuals for nodes (continuous variable);
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx age obstruct differ extent surg;
output out=plot2_1 RESMART = Martingale;

proc loess data=plot2_1;
model Martingale = nodes / direct;
run;

* Martingale Residuals for sqrted nodes (continuous variable);
data colon;
set colon;
s_nodes = sqrt(nodes);
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx age obstruct differ extent surg;
output out=plot2_1 RESMART = Martingale;

proc loess data=plot2_1;
model Martingale = s_nodes / direct;
run;


* PH assumption test for each variables;
ods noproctitle;
ods graphics / imagemap=on;

proc phreg data=WORK.COLON zph(noplot global);
	class rx obstruct differ extent surg / param=glm;
	model time*status(0)=rx age obstruct s_nodes differ extent surg / rl;
run;

* Cox-snell residuals plot for data which have deleted some variables;
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) = rx age s_nodes surg extent;
output out=plot1_1 logsurv=logsurv1 /method = ch;

data plot1_1;
set plot1_1;
snell = -logsurv1;
cons = 1;

proc phreg data=plot1_1;
model snell*status(0) = cons;
output out = plot1_2 logsurv= logsurv2/method=ch;

data plot1_2;
set plot1_2;
cumhaz = - logsurv2;

proc sort data=plot1_2;
by snell;

proc sgplot data= plot1_2;
step y=cumhaz x=snell /MARKERFILLATTRS=(color="red");
lineparm x=0 y=0 slope=1; /** intercept, slope **/
label cumhaz = "Estimated Cumulative Hazard Rates";
label snell = "Residual";
run;

* Final model;
proc phreg data = colon;
class rx;
class sex;
class obstruct;
class perfor;
class adhere;
class differ;
class extent;
class surg;
model time*status(0) =  rx age s_nodes surg extent;
run;



