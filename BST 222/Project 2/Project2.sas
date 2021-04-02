proc import datafile = "/folders/myshortcuts/MyFolder/sever.csv" out=sever;

* analysis for sever RLI;
proc phreg data = sever;
model time*censor(0) = p_TACE chile_class pringle_maneuver 
intra_trans serum_alb_level serum_alt_level long_op_time;
run;

* Cox-snell residuals plot for sever data;
proc phreg data = sever;
model time*censor(0) = p_TACE chile_class pringle_maneuver intra_trans serum_alb_level serum_alt_level long_op_time;
output out=plot1_1 logsurv=logsurv1 /method = ch;

data plot1_1;
set plot1_1;
snell = -logsurv1;
cons = 1;

proc phreg data=plot1_1;
model snell*censor(0) = cons;
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

* PH assumption test for each variables in sever data;
ods noproctitle;
ods graphics / imagemap=on;

proc phreg data=sever zph(noplot global);
	model time*censor(0) = p_TACE chile_class pringle_maneuver intra_trans serum_alb_level serum_alt_level long_op_time / rl;
run;

/* ods noproctitle; */
/* ods graphics / imagemap=on; */
/*  */
/* proc phreg data=WORK.SEVER zph(noplot global) outest=Work.Phreg_est; */
/* 	model time*censor(0)=p_TACE chile_class pringle_maneuver intra_trans  */
/* 		serum_alb_level serum_alt_level long_op_time / rl; */
/* 	output out=Work.Phreg_out resmart=resmart ressch=_all_; */
/* run; */

* Martingale Residuals for alt (continuous variable);
proc phreg data = sever;
model time*censor(0) = p_TACE chile_class pringle_maneuver 
intra_trans serum_alb_level long_op_time;
output out=plot2_1 RESMART = Martingale;

proc loess data=plot2_1;
model Martingale = serum_alt_level / direct;
run;

* Martingale Residuals for alb (continuous variable);
proc phreg data = sever;
model time*censor(0) = p_TACE chile_class pringle_maneuver 
intra_trans long_op_time serum_alt_level;
output out=plot2_1 RESMART = Martingale;

proc loess data=plot2_1;
model Martingale = serum_alb_level / direct;
run;


* analysis for overall RLI;
proc import datafile = "/folders/myshortcuts/MyFolder/overall.csv" out=overall;
proc phreg data = overall;
model time*censor(0) = stage chile_class min_Severe_RLI open_surgery 
 intra_trans satellite_nodule micro_vascular_invasion mcig_type h_confirmed_c;
run;

*log-rank test for sever or min RLI for overall;
proc lifetest data=overall;
time time*censor(0);
strata min_Severe_RLI/ test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;


proc lifetest data=overall;
time time*censor(0);
strata rli_type/ test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;

* Cox-snell residuals plot for sever data;
proc phreg data = overall;
model time*censor(0) = stage chile_class min_Severe_RLI open_surgery 
 intra_trans satellite_nodule micro_vascular_invasion mcig_type h_confirmed_c;
output out=plot1_1 logsurv=logsurv1 /method = ch;

data plot1_1;
set plot1_1;
snell = -logsurv1;
cons = 1;

proc phreg data=plot1_1;
model snell*censor(0) = cons;
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

* PH assumption test for each variables in sever data;
ods noproctitle;
ods graphics / imagemap=on;

proc phreg data=overall zph(noplot global);
	model time*censor(0) = stage chile_class min_Severe_RLI open_surgery 
 intra_trans satellite_nodule micro_vascular_invasion mcig_type h_confirmed_c / rl;
run;



* analysis for disease free RLI;
proc import datafile = "/folders/myshortcuts/MyFolder/diseaseFree.csv" out=diseaseFree;
proc phreg data = diseaseFree;
model time*censor(0) = sex icgr_bigger_10 stage p_TACE min_Severe_RLI
 nonanatom_resection satellite_nodule micro_vascular_invasion mcig_type;
run;



*log-rank test for sever or min RLI for overall;
proc lifetest data=diseaseFree;
time time*censor(0);
strata min_Severe_RLI/ test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;


proc lifetest data=diseaseFree;
time time*censor(0);
strata rli_type/ test=(logrank TARONE PETO MODPETO FLEMING(0,1));
run;


* Cox-snell residuals plot for sever data;
proc phreg data = diseaseFree;
model time*censor(0) = sex icgr_bigger_10 stage p_TACE min_Severe_RLI
 nonanatom_resection satellite_nodule micro_vascular_invasion mcig_type;
output out=plot1_1 logsurv=logsurv1 /method = ch;

data plot1_1;
set plot1_1;
snell = -logsurv1;
cons = 1;

proc phreg data=plot1_1;
model snell*censor(0) = cons;
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

* PH assumption test for each variables in sever data;
ods noproctitle;
ods graphics / imagemap=on;

proc phreg data=diseaseFree zph(noplot global);
	model time*censor(0) = sex icgr_bigger_10 stage p_TACE min_Severe_RLI
 nonanatom_resection satellite_nodule micro_vascular_invasion mcig_type / rl;
run;

