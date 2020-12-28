LIBNAME lab7 "/folders/myfolders/Lab7";





******** Model selection;

data bone_marrow;
input g t1 t2 death relapse dfree ta a tc c tp p z1-z10; 
label g = "Disease Group"
      t1 = "Time To Death"
	  T2 = "Disease Free Survial Time"
      death = "Death"
	  relapse = "Relapse"
	  dfree = "Disease Free Survial"
	  ta = "Time to Acute GVHD"
	  a = "Acute GVHD"
	  tc = "Time to chronic GVHD"
	  c = "Chronic GVHD"
	  tp = "Time RP to NL"
	  p = "Platelet Recovery"
	  z1 = "Patient Age"
	  z2 = "Donor Age"
	  z3 = "Patient sex"
	  z4 = "Donor Sex"
	  z5 = "Patient CMV"
	  z6 = "Donor CMV"
	  z7 = "Waiting time: days"
	  z8 = "FAB"
	  z9 = "Hospital"
	  z10 = "MTX";
cards;
1  2081  2081  0  0  0    67  1   121  1    13  1  26  33  1  0  1  1    98  0  1  0
1  1602  1602  0  0  0  1602  0   139  1    18  1  21  37  1  1  0  0  1720  0  1  0
1  1496  1496  0  0  0  1496  0   307  1    12  1  26  35  1  1  1  0   127  0  1  0
1  1462  1462  0  0  0    70  1    95  1    13  1  17  21  0  1  0  0   168  0  1  0
1  1433  1433  0  0  0  1433  0   236  1    12  1  32  36  1  1  1  1    93  0  1  0
1  1377  1377  0  0  0  1377  0   123  1    12  1  22  31  1  1  1  1  2187  0  1  0
1  1330  1330  0  0  0  1330  0    96  1    17  1  20  17  1  0  1  1  1006  0  1  0
1   996   996  0  0  0    72  1   121  1    12  1  22  24  1  0  0  0  1319  0  1  0
1   226   226  0  0  0   226  0   226  0    10  1  18  21  0  1  0  0   208  0  1  0
1  1199  1199  0  0  0  1199  0    91  1    29  1  24  40  1  1  0  1   174  0  3  1
1  1111  1111  0  0  0  1111  0  1111  0    22  1  19  28  1  1  0  1   236  0  3  1
1   530   530  0  0  0    38  1    84  1    34  1  17  28  1  1  0  0   151  0  3  1
1  1182  1182  0  0  0  1182  0   112  1    22  1  24  23  0  0  0  1   203  0  2  1
1  1167  1167  0  0  0    39  1   487  1  1167  0  27  22  0  1  1  1   191  0  2  1
1   418   418  1  0  1   418  0   220  1    21  1  18  14  1  1  0  0   110  0  1  0
1   417   383  1  1  1   417  0   417  0    16  1  15  20  1  1  0  0   824  0  1  0
1   276   276  1  0  1   276  0    81  1    21  1  18   5  0  0  0  0   146  0  1  0
1   156   104  1  1  1    28  1   156  0    20  1  20  33  1  1  0  1    85  0  1  0
1   781   609  1  1  1   781  0   781  0    26  1  27  27  1  0  1  1   187  0  1  0
1   172   172  1  0  1    22  1   172  0    37  1  40  37  0  0  0  1   129  0  1  0
1   487   487  1  0  1   487  0    76  1    22  1  22  20  1  1  0  0   128  0  1  0
1   716   662  1  1  1   716  0   716  0    17  1  28  32  1  1  0  0    84  0  1  0
1   194   194  1  0  1   194  0    94  1    25  1  26  32  0  1  0  0   329  0  1  0
1   371   230  1  1  1   371  0   184  1     9  1  39  31  0  1  0  1   147  0  1  0
1   526   526  1  0  1   526  0   121  1    11  1  15  20  1  1  0  0   943  0  1  0
1   122   122  1  0  1    88  1   122  0    13  1  20  26  1  0  0  1  2616  0  1  0
1  1279   129  1  1  1  1279  0  1279  0    22  1  17  20  0  0  0  0   937  0  3  1
1   110    74  1  1  1   110  0   110  0    49  1  28  25  1  0  1  0   303  0  3  1
1   243   122  1  1  1   243  0   243  0    23  1  37  38  0  1  1  1   170  0  3  1
1    86    86  1  0  1    86  0    86  0    86  0  17  26  1  0  1  0   239  0  3  1
1   466   466  1  0  1   466  0   119  1   100  1  15  18  1  1  0  0   508  0  3  1
1   262   192  1  1  1    10  1    84  1    59  1  29  32  1  1  1  0    74  0  3  1
1   162   109  1  1  1   162  0   162  0    40  1  36  43  1  1  1  0   393  0  2  1
1   262    55  1  1  1   262  0   262  0    24  1  23  16  0  1  1  1   331  0  2  1
1     1     1  1  0  1     1  0     1  0     1  0  42  48  1  1  0  0   196  0  2  1
1   107   107  1  0  1   107  0   107  0   107  0  30  19  1  1  1  1   178  0  2  1
1   269   110  1  1  1   269  0   120  1    27  1  29  20  0  1  1  1   361  0  2  1
1   350   332  1  0  1   350  0   350  0    33  1  22  20  1  0  0  0   834  0  2  1
2  2569  2569  0  0  0  2569  0  2569  0    21  1  19  13  1  1  1  0   270  1  1  0
2  2506  2506  0  0  0  2506  0  2506  0    17  1  31  34  1  1  0  0    60  0  1  0
2  2409  2409  0  0  0  2409  0  2409  0    16  1  35  31  1  1  1  1   120  0  1  0
2  2218  2218  0  0  0  2218  0  2218  0    11  1  16  16  1  1  1  0    60  1  1  0
2  1857  1857  0  0  0  1857  0   260  1    15  1  29  35  0  0  1  0    90  0  1  0
2  1829  1829  0  0  0  1829  0  1829  0    19  1  19  18  1  1  1  0   210  0  1  0
2  1562  1562  0  0  0  1562  0  1562  0    18  1  26  30  1  1  1  1    90  0  1  0
2  1470  1470  0  0  0  1470  0   180  1    14  1  27  34  1  1  0  1   240  0  1  0
2  1363  1363  0  0  0  1363  0   200  1    12  1  13  24  1  1  1  0    90  0  1  0
2  1030  1030  0  0  0  1030  0   210  1    14  1  25  29  0  0  0  0   210  0  1  0
2   860   860  0  0  0   860  0   860  0    15  1  25  31  0  1  0  1   180  0  1  0
2  1258  1258  0  0  0  1258  0   120  1    66  1  30  16  0  1  1  0   180  0  2  1
2  2246  2246  0  0  0    52  1   380  1    15  1  45  39  0  0  0  0   105  0  4  0
2  1870  1870  0  0  0  1870  0   230  1    16  1  33  30  0  0  1  1   225  0  4  0
2  1799  1799  0  0  0  1799  0   140  1    12  1  32  23  1  0  0  0   120  0  4  0
2  1709  1709  0  0  0    20  1   348  1    19  1  23  28  0  1  1  0    90  1  4  0
2  1674  1674  0  0  0  1674  0  1674  0    24  1  37  34  1  1  0  0    60  1  4  0
2  1568  1568  0  0  0  1568  0  1568  0    14  1  15  19  1  0  0  0    90  0  4  0
2  1527  1527  0  0  0  1527  0  1527  0    13  1  22  12  0  1  0  1   450  1  4  0
2  1324  1324  0  0  0    25  1  1324  0    15  1  46  31  1  1  1  1    75  0  4  0
2   957   957  0  0  0   957  0   957  0    69  1  18  17  1  1  0  0    90  0  4  0
2   932   932  0  0  0    29  1   932  0     7  1  27  30  0  0  0  0    60  1  4  0
2   847   847  0  0  0   847  0   847  0    16  1  28  29  1  1  0  0    75  0  4  0
2   848   848  0  0  0   848  0   155  1    16  1  23  26  1  1  0  0   180  0  4  0
2  1850  1850  0  0  0  1850  0  1850  0     9  1  37  36  0  0  0  1   180  0  3  1
2  1843  1843  0  0  0  1843  0  1843  0    19  1  34  32  0  0  1  1   270  0  3  1
2  1535  1535  0  0  0  1535  0  1535  0    21  1  35  32  0  1  0  0   180  1  3  1
2  1447  1447  0  0  0  1447  0   220  1    24  1  33  28  0  1  1  1   150  0  3  1
2  1384  1384  0  0  0  1384  0   200  1    19  1  21  18  0  0  0  0   120  0  3  1
2   414   414  1  0  1   414  0   414  0    27  1  21  15  1  1  0  1   120  1  1  0
2  2204  2204  1  0  1  2204  0  2204  0    12  1  25  19  0  0  0  1    60  0  1  0
2  1063  1063  1  0  1  1063  0   240  1    16  1  50  38  1  0  1  0   270  1  1  0
2   481   481  1  0  1    30  1   120  1    24  1  35  36  1  0  1  1    90  1  1  0
2   105   105  1  0  1    21  1   105  0    15  1  37  34  1  0  1  1   120  0  1  0
2   641   641  1  0  1   641  0   641  0    11  1  26  24  1  1  0  0    90  0  1  0
2   390   390  1  0  1   390  0   390  0    11  1  50  48  1  1  0  0   120  0  1  0
2   288   288  1  0  1    18  1   100  1   288  0  45  43  1  1  1  1    90  0  1  0
2   522   421  1  1  1    25  1   140  1    20  1  28  30  1  1  0  1    90  1  1  0
2    79    79  1  0  1    16  1    79  0    79  0  43  43  0  0  0  0    90  0  1  0
2  1156   748  1  1  1  1156  0   180  1    18  1  14  19  1  0  0  0    60  0  1  0
2   583   486  1  1  1   583  0   583  0    11  1  17  14  0  1  0  0   120  0  1  0
2    48    48  1  0  1    48  0    48  0    14  1  32  33  0  1  1  0   150  1  1  0
2   431   272  1  1  1   431  0   431  0    12  1  30  23  0  1  1  0   120  1  1  0
2  1074  1074  1  0  1  1074  0   120  1    19  1  30  32  1  1  1  0   150  1  1  0
2   393   381  1  1  1   393  0   100  1    16  1  33  28  0  0  0  0   120  1  1  0
2    10    10  1  0  1    10  0    10  0    10  0  34  54  1  0  1  1   240  0  2  1
2    53    53  1  0  1    53  0    53  0    53  0  33  41  0  1  1  1   180  0  2  1
2    80    80  1  0  1    10  1    80  0    80  0  30  35  0  0  0  1   150  0  2  1
2    35    35  1  0  1    35  0    35  0    35  0  23  25  0  1  1  1   150  0  2  1
2  1499   248  0  1  1  1499  0  1499  0     9  1  35  18  1  1  0  1    30  0  4  0
2   704   704  1  0  1    36  1   155  1    18  1  29  21  0  1  1  0   105  0  4  0
2   653   211  1  1  1   653  0   653  0    23  1  23  16  1  0  0  0    90  1  4  0
2   222   219  1  1  1   222  0   123  1    52  1  28  30  1  1  1  1   120  1  3  1
2  1356   606  0  1  1  1356  0  1356  0    14  1  33  22  1  1  1  0   210  1  3  1
3  2640  2640  0  0  0  2640  0  2640  0    22  1  18  23  1  1  0  0   750  0  1  0
3  2430  2430  0  0  0  2430  0  2430  0    14  1  29  26  1  1  0  1    24  0  1  0
3  2252  2252  0  0  0  2252  0   150  1    17  1  35  31  1  0  0  0   120  0  1  0
3  2140  2140  0  0  0  2140  0   220  1    18  1  27  17  1  1  1  1   210  0  1  0
3  2133  2133  0  0  0  2133  0   250  1    17  1  36  39  0  1  0  0   240  0  1  0
3  1238  1238  0  0  0  1238  0   250  1    18  1  24  28  1  0  1  1   240  0  1  0
3  1631  1631  0  0  0  1631  0   150  1    40  1  27  21  1  0  1  0   690  1  2  1
3  2024  2024  0  0  0  2024  0   180  1    16  1  35  41  0  1  0  0   105  1  4  0
3  1345  1345  0  0  0    32  1   360  1    14  1  50  36  1  1  1  1   120  0  4  0
3  1136  1136  0  0  0  1136  0   140  1    15  1  47  27  1  0  1  0   900  0  3  1
3   845   845  0  0  0   845  0   845  0    20  1  40  39  0  0  1  1   210  1  3  1
3   491   422  1  1  1   491  0   180  1   491  0  22  21  0  0  0  0   210  1  1  0
3   162   162  1  0  1   162  0   162  0    13  1  22  23  1  0  0  1   300  0  1  0
3  1298    84  1  1  1  1298  0  1298  0  1298  0   8   2  0  0  1  0   105  1  1  0
3   121   100  1  1  1    28  1   121  0    65  1  39  48  1  1  1  1   210  1  1  0
3     2     2  1  0  1     2  0     2  0     2  0  20  19  1  1  0  0    75  1  1  0
3    62    47  1  1  1    62  0    62  0    11  1  27  25  1  1  0  0    90  1  1  0
3   265   242  1  1  1   265  0   210  1    14  1  32  32  1  0  0  0   180  1  1  0
3   547   456  1  1  1   547  0   130  1    24  1  31  28  1  0  1  1   630  1  1  0
3   341   268  1  1  1    21  1   100  1    17  1  20  23  0  1  1  1   180  1  1  0
3   318   318  1  0  1   318  0   140  1    12  1  35  40  0  1  1  1   300  0  1  0
3   195    32  1  1  1   195  0   195  0    16  1  36  39  1  1  0  0    90  1  1  0
3   469   467  1  1  1   469  0    90  1    20  1  35  33  0  0  1  0   120  0  1  0
3    93    47  1  1  1    93  0    93  0    28  1   7   2  1  1  0  0   135  1  1  0
3   515   390  1  1  1   515  0   515  0    31  1  23  25  1  1  1  0   210  1  1  0
3   183   183  1  0  1   183  0   130  1    21  1  11  7  0  1  0  0    120  1  1  0
3   105   105  1  0  1   105  0   105  0   105  0  14  18  1  0  0  0   150  1  1  0
3   128   115  1  1  1   128  0   128  0    12  1  37  35  0  0  1  1   270  0  1  0
3   164   164  1  0  1   164  0   164  0   164  0  19  32  0  0  0  1   285  1  1  0
3   129    93  1  1  1   129  0   129  0    51  1  37  34  0  1  1  0   240  1  1  0
3   122   120  1  1  1   122  0   122  0    12  1  25  29  0  1  1  1   510  1  1  0
3    80    80  1  0  1    21  1    80  0     0  1  35  28  1  0  0  0   780  1  1  0
3   677   677  1  0  1   677  0   150  1     8  1  15  14  1  1  1  0   150  1  1  0
3    73    64  1  1  1    73  0    73  0    38  1  45  42  0  1  1  0   180  1  2  1
3   168   168  1  0  1   168  0   200  1    48  1  32  43  0  1  1  1   150  1  2  1
3    74    74  1  0  1    29  1    74  0    24  1  41  29  0  1  1  1   750  0  2  1
3    16    16  1  0  1    16  0    16  0    16  0  27  36  0  0  1  0   180  0  4  0
3   248   157  1  1  1   248  0   100  1    52  1  33  39  0  0  1  1   180  1  4  0
3   732   625  1  1  1   732  0   732  0    18  1  39  43  0  1  1  1   150  1  4  0
3   105    48  1  1  1   105  0   105  0    30  1  17  14  0  1  0  0   210  1  4  0
3   392   273  1  1  1   392  0   122  1    24  1  43  50  1  1  1  0   240  0  3  1
3    63    63  1  0  1    38  1    63  0    16  1  44  37  1  1  0  0   360  1  3  1
3    97    76  1  1  1    97  0    97  0    97  0  48  56  1  1  1  1   330  0  3  1
3   153   113  1  1  1   153  0   153  0    59  1  31  25  0  1  1  1   240  0  3  1
3   363   363  1  0  1   363  0   363  0    19  1  52  48  1  1  1  0   180  0  3  1
;
run;
proc format ;
  VALUE  gs 1 = "ALL"
            2 = "AML Low Risk"
            3 = "AML High Risk" ;
  value dind  0 = "Alive"
              1 = "Dead";
  value rind  0 = "Disease Free"
              1 = "Relapsed";
  value dfs   0 = "Alive Disease Free"
              1 = "Dead or Relapsed";
  value aind  0 = "Never Developed Acute GVHD"
              1 = "Developed Acute GVHD";
  value cind  0 = "Never Developed Chronic GVHD"
              1 = "Developed Chronic GVHD";
  value pind  0 = "Platelet Never Returned to Normal"
              1 = "Returned to Normal";
  value sex   0 = "Female"
              1 = "Male";
  value cmvs  0 = "CMV Negative"
              1 = "CMV Positive";
  value fab   0 = "Otherwise"
              1 = "FAB Grade 4 or 5 and AML";
  value yes   0 = "No"
              1 = "Yes";
run;

data bone_marrow;
set bone_marrow;
format g gs.;
z1=z1/10;x2=x2/10;
run;

* Global test; 
proc lifetest data=bone_marrow plots=survival(atrisk=0 to 2500 by 500);  
time t2*dfree(0); 
strata g/test=(logrank TARONE PETO MODPETO  
FLEMING(0,1)    ); 
run;


* create additional covariates;
data bone_marrow1;
   set bone_marrow;

  z1 = z1 - 28;
  z2 = z2 - 28;
  z1xz2 = z1 * z2;
  label z1xz2 = "Age interaction";
  g1 = ( g = 1 );
  g2 = ( g = 2 );
  g3 = ( g = 3 );
  z3xz4 = z3*z4; /*interaction of sex*/
  z5xz6 = z5*z6; /*interaction of cmv status*/
  z1xz2 = z1*z2; /*interaction of age*/
run;


 
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z7 ; /*waiting time is z7*/
  z7: test z7 = 0;
  ods output  FitStatistics = z7aic;
  ods output TestStmts = z7stat;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 ; /*fab is z8*/
  z8: test z8 = 0;
  ods output  FitStatistics = z8aic;
  ods output TestStmts = z8stat;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3  z10 ; /*MTX is z10*/
  z10: test z10 = 0;
  ods output  FitStatistics = z10aic;
  ods output TestStmts = z10stat;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z3 z4 z3xz4 ; /*SEX is z3 and z4*/
  sex: test z3 = z4 = z3xz4 =0;
  ods output  FitStatistics = sexaic;
  ods output TestStmts = sexstat;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z5 z6 z5xz6 ; /*CMV status is z5 and z6*/
  cmv: test z5 = z6 = z5xz6 =0;
  ods output  FitStatistics = cmvaic;
  ods output TestStmts = cmvstat;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z1 z2 z1xz2 ; /*age is z1 and z2*/
  age: test z1 = z2 = z1xz2 =0;
  ods output  FitStatistics = ageaic;
  ods output TestStmts = agestat;
run;
 
data aic_table8_4;
  set z7aic z8aic z10aic sexaic cmvaic ageaic;
run;
proc print data = aic_table8_4 noobs;
where criterion = "AIC";
var criterion withcovariates;
run;
data stat_table8_4;
  set z7stat z8stat z10stat sexstat cmvstat agestat;
run;
proc print data = stat_table8_4 (drop=status) noobs;
run;

* pick z8, repeat;
ods listing close;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z7;
  z7: test z7 =0;
  ods output  FitStatistics = z7aic_t5;
  ods output TestStmts = z7stat_t5;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z10 ;
  z10: test z10 =0;
  ods output  FitStatistics = z10aic_t5;
  ods output TestStmts = z10stat_t5;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z3 z4 z3xz4 ;
  sex: test z3 = z4 = z3xz4 = 0;
  ods output  FitStatistics = sexaic_t5;
  ods output TestStmts = sexstat_t5;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z5 z6 z5xz6 ;
  cmv: test z5 = z6 = z5xz6 = 0;
  ods output  FitStatistics = cmvaic_t5;
  ods output TestStmts = cmvstat_t5;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z1 z2 z1xz2 ;
  age: test z1 = z2 = z1xz2 = 0;
  ods output  FitStatistics = ageaic_t5;
  ods output TestStmts = agestat_t5;
run;

ods listing ;
data aic_table8_5;
  set z7aic_t5 z10aic_t5 sexaic_t5 cmvaic_t5 ageaic_t5;
run;
proc print data = aic_table8_5 noobs;
where criterion = "AIC";
var criterion withcovariates;
run;
data stat_table8_5;
  set z7stat_t5 z10stat_t5 sexstat_t5 cmvstat_t5 agestat_t5;
run;
proc print data = stat_table8_5 (drop=status) noobs;
run;

* pick z1, repeatr;
ods listing close;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z1 z2 z1xz2  z7;
  z7: test z7 = 0;
  ods output  FitStatistics = z7aic_t6;
  ods output TestStmts = z7stat_t6;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z1 z2 z1xz2 z10 ;
  z10: test z10 = 0;
  ods output  FitStatistics = z10aic_t6;
  ods output TestStmts = z10stat_t6;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z1 z2 z1xz2 z3 z4 z3xz4 ;
  sex: test z3 = z4 = z3xz4 = 0;
  ods output  FitStatistics = sexaic_t6;
  ods output TestStmts = sexstat_t6;
run;
proc phreg data = bone_marrow1;
  model t2*dfree(0)= g2 g3 z8 z1 z2 z1xz2 z5 z6 z5xz6;
  cmv: test z5 = z6 = z5xz6 = 0;
  ods output  FitStatistics = cmvaic_t6;
  ods output TestStmts = cmvstat_t6;
run;
ods listing ;
data aic_table8_6;
  set z7aic_t6 z10aic_t6 sexaic_t6 cmvaic_t6 ;
run;
proc print data = aic_table8_6 noobs;
where criterion = "AIC";
var criterion withcovariates;
run;
data stat_table8_6;
  set z7stat_t6 z10stat_t6 sexstat_t6 cmvstat_t6;
run;
proc print data = stat_table8_6 (drop=status) noobs;
run;
