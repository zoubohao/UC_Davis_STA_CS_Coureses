proc import datafile = "/folders/myshortcuts/MyFolder/Q2.csv" out=HWQ2;

PROC PHREG DATA= HWQ2;
model time*censor(1) = z;
run;

proc import datafile= "/folders/myshortcuts/MyFolder/Book1.csv" out = book;

proc lifetest data = book;
time time*censor(1);
strata type/test=logrank;
run;

proc phreg data=book;
class type;
model time*censor(1) = type;
run;

* KM for positive;
proc lifetest data=book atrisk;
time time*censor(1);
strata type;
run;


proc import datafile = "/folders/myshortcuts/MyFolder/pharmacoSmoking.csv" out=PS;
proc lifetest data = PS;
time ttr*relapse(0);
strata grp/trend test=logrank;
run;