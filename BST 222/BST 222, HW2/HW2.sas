
proc import datafile = "/folders/myshortcuts/MyFolder/KMN.csv" out=negKM;
proc import datafile= "/folders/myshortcuts/MyFolder/KMP.csv" out = posKM;
proc contents data=negKM;
proc contents data=posKM;


* KM for negtive;
proc lifetest data=negKM atrisk outs=KMest;
time time*censor(1);
run;

* KM for positive;
proc lifetest data=posKM atrisk outs=KMest;
time time*censor(1);
run;

* Nelson-Aalen for neg;
proc lifetest data=negKM nelson ;
time time*censor(1);
ods output productlimitestimates=negNA;
run; 

* Nelson-Aalen for pos;
proc lifetest data=posKM nelson ;
time time*censor(1);
run; 

* smoothed hazard function for neg;
proc lifetest data=negKM method=KM plots=hazard(cl);
time time*censor(1); 
run;

* smoothed hazard function for pos;
proc lifetest data=posKM method=KM plots=hazard(cl);
time time*censor(1); 
run;


