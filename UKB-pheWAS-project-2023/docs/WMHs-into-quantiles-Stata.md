### **\*\*\*\*\* Steps to divide the WMH variable from UKB into quantiles (10 subgroups) \*\*\*\*\***

###                 **\*\*\*\* December 2023 \*\*\*\***

###                    **\*\*\* STATA \*\*\***





\# Load UKB data:



use "C:\\Users\\angel\\Desktop\\PHEWAS\_PhD\_Project\_2023\\UKB Data Sept 2023 UPDATED\\19sept23.dta", clear



\# Inspecting and summarizing the WMH variable:



browse x25781\_2\_0 /\* Total volume of white matter hyperintensities (from T1 and T2\_FLAIR images) \*/



sum x25781\_2\_0



&nbsp;    sum x25781\_2\_0



&nbsp;   Variable |        Obs        Mean    Std. dev.       Min        Max

-------------+---------------------------------------------------------

&nbsp; x25781\_2\_0 |     45,013    5235.106    6917.691          9     114938



\# Dividing the WMH variable into ten quantiles and inspecting the results:



xtile dec= x25781\_2\_0, nq(10)



sort x25781\_2\_0



sort dec



by dec: summarize x25781\_2\_0, detail



-----------------------------------------------------------------------------------------------------------------

-> dec = 1



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          125              9

&nbsp;5%          235             30

10%          321             37       Obs               4,504

25%          471             39       Sum of wgt.       4,504



50%          639                      Mean           607.5577

&nbsp;                       Largest       Std. dev.      197.4352

75%        772.5            890

90%          845            890       Variance       38980.66

95%          868            890       Skewness      -.5752701

99%          886            890       Kurtosis        2.48461



-----------------------------------------------------------------------------------------------------------------

-> dec = 2



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          896            891

&nbsp;5%          914            891

10%          938            891       Obs               4,503

25%         1006            891       Sum of wgt.       4,503



50%         1119                      Mean           1116.665

&nbsp;                       Largest       Std. dev.      128.0824

75%         1230           1335

90%         1291           1335       Variance       16405.11

95%         1313           1335       Skewness      -.0349302

99%         1331           1335       Kurtosis       1.790976



-----------------------------------------------------------------------------------------------------------------

-> dec = 3



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         1341           1336

&nbsp;5%         1358           1336

10%         1379           1336       Obs               4,501

25%         1449           1336       Sum of wgt.       4,501



50%         1556                      Mean           1557.419

&nbsp;                       Largest       Std. dev.      127.9386

75%         1667           1780

90%         1736           1780       Variance       16368.29

95%         1757           1780       Skewness       .0065358

99%         1776           1780       Kurtosis       1.805281



-----------------------------------------------------------------------------------------------------------------

-> dec = 4



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         1785           1781

&nbsp;5%         1806           1781

10%         1829           1781       Obs               4,505

25%         1901           1781       Sum of wgt.       4,505



50%         2026                      Mean           2028.366

&nbsp;                       Largest       Std. dev.      146.7518

75%         2151           2293

90%         2237           2293       Variance        21536.1

95%         2265           2293       Skewness       .0694715

99%         2289           2293       Kurtosis       1.831989



-----------------------------------------------------------------------------------------------------------------

-> dec = 5



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         2300           2294

&nbsp;5%         2323           2294

10%         2352           2295       Obs               4,499

25%         2444           2295       Sum of wgt.       4,499



50%         2597                      Mean           2603.662

&nbsp;                       Largest       Std. dev.      185.1406

75%         2762           2932

90%         2866           2932       Variance       34277.04

95%         2900           2932       Skewness       .0778252

99%         2928           2932       Kurtosis       1.810286



-----------------------------------------------------------------------------------------------------------------

-> dec = 6



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         2939           2933

&nbsp;5%         2964           2933

10%         3001           2933       Obs               4,501

25%         3121           2933       Sum of wgt.       4,501



50%         3327                      Mean            3337.93

&nbsp;                       Largest       Std. dev.      250.3335

75%         3548           3793

90%         3696           3793       Variance       62666.84

95%         3745           3793       Skewness       .1126977

99%         3784           3793       Kurtosis        1.81109



-----------------------------------------------------------------------------------------------------------------

-> dec = 7



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         3803           3794

&nbsp;5%       3848.5           3794

10%         3910           3794       Obs               4,500

25%         4075           3794       Sum of wgt.       4,500



50%         4366                      Mean           4393.046

&nbsp;                       Largest       Std. dev.      368.4852

75%         4705           5084

90%         4925           5084       Variance       135781.4

95%         5005           5084       Skewness       .1629061

99%         5069           5084       Kurtosis       1.852006



-----------------------------------------------------------------------------------------------------------------

-> dec = 8



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         5099           5085

&nbsp;5%         5170           5085

10%         5257           5085       Obs               4,499

25%         5526           5086       Sum of wgt.       4,499



50%         6013                      Mean            6061.92

&nbsp;                       Largest       Std. dev.      617.4938

75%         6582           7236

90%         6960           7236       Variance       381298.6

95%         7099           7237       Skewness       .1931997

99%         7216           7237       Kurtosis       1.855446



-----------------------------------------------------------------------------------------------------------------

-> dec = 9



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         7269           7238

&nbsp;5%         7383           7238

10%         7548           7239       Obs               4,500

25%         8090           7239       Sum of wgt.       4,500



50%       9051.5                      Mean           9246.871

&nbsp;                       Largest       Std. dev.      1349.353

75%        10335          11982

90%      11278.5          11983       Variance        1820755

95%      11607.5          11984       Skewness       .3319364

99%      11901.5          11984       Kurtosis       1.940795



-----------------------------------------------------------------------------------------------------------------

-> dec = 10



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%        12067          11987

&nbsp;5%        12379          11988

10%        12801          11990       Obs               4,501

25%        14246          11991       Sum of wgt.       4,501



50%        17689                      Mean           21405.29

&nbsp;                       Largest       Std. dev.      11152.25

75%        24144         101414

90%        34497         111188       Variance       1.24e+08

95%        43746         113750       Skewness       2.704605

99%        65425         114938       Kurtosis       13.74194



-----------------------------------------------------------------------------------------------------------------

-> dec = .



&nbsp;       Total volume of white matter hyperintensities

&nbsp;               (from T1 and T2\_FLAIR images)

-------------------------------------------------------------

no observations



\# Dichotomizing the WMH variable into 0s and 1s:



Variable dec renamed to x25781\_2\_0dec.



browse x25781\_2\_0dec



. replace  x25781\_2\_0dec = 0 if x25781\_2\_0dec == 1

(4,504 real changes made)



. replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 2

(4,503 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 3

(4,501 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 4

(4,505 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 5

(4,499 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 6

(4,501 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 7

(4,500 real changes made)



.  replace x25781\_2\_0dec = 1 if x25781\_2\_0dec == 10

(4,501 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 8

(4,499 real changes made)



.  replace x25781\_2\_0dec = 0 if x25781\_2\_0dec == 9

(4,500 real changes made)



. tab x25781\_2\_0dec



&nbsp;        10 |

&nbsp; quantiles |

&nbsp;        of |

&nbsp;x25781\_2\_0 |      Freq.     Percent        Cum.

------------+-----------------------------------

&nbsp;         0 |     40,512       90.00       90.00

&nbsp;         1 |      4,501       10.00      100.00

------------+-----------------------------------

&nbsp;     Total |     45,013      100.00



\# Exporting dataset into a .csv file:



export delimited using "C:\\Users\\angel\\Desktop\\PHEWAS\_PhD\_Project\_2023\\UKB Data Sept 2023 UPDATED\\07dec23.csv", replace



&nbsp;	\*\*OLD WMH VARIABLE - RESULTS FROM JULY 2023:\*\*



\# Dividing the WMH variable into ten quantiles and inspecting the results:



. xtile UKB\_WMHs = x25781\_2\_0, n(10)



. browse UKB\_WMHs



. xtile UKB\_WMHs2 = x25781\_2\_0, nq(10) /\* this command performs the same transformation as n(10) \*/



. browse UKB\_WMHs2



. sort UKB\_WMHs UKB\_WMHs2



. sort x25781\_2\_0



. browse x25781\_2\_0 UKB\_WMHs UKB\_WMHs2



. sum UKB\_WMHs2



&nbsp;   Variable |        Obs        Mean    Std. dev.       Min        Max

-------------+---------------------------------------------------------

&nbsp;  UKB\_WMHs2 |     43,355     5.49932    2.872716          1         10



. tabulate UKB\_WMHs2, mis



&nbsp;        10 |

&nbsp; quantiles |

&nbsp;        of |

&nbsp;x25781\_2\_0 |      Freq.     Percent        Cum.

------------+-----------------------------------

&nbsp;         1 |      4,128        8.62        8.62

&nbsp;         2 |      4,128        8.62       17.23

&nbsp;         3 |      4,116        8.59       25.82

&nbsp;         4 |      4,123        8.61       34.43

&nbsp;         5 |      4,127        8.61       43.05

&nbsp;         6 |      4,122        8.60       51.65

&nbsp;         7 |      4,120        8.60       60.25

&nbsp;         8 |      4,124        8.61       68.86

&nbsp;         9 |      4,123        8.61       77.46

&nbsp;        10 |      4,123        8.61       86.07

&nbsp;         . |      6,674       13.93      100.00

------------+-----------------------------------

&nbsp;     Total |     47,908      100.00



\# Interrogating each quantile category separately:



. sort UKB\_WMHs2



. by UKB\_WMHs2: summarize x25781\_2\_0, detail



-> UKB\_WMHs2 = 1



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          125              9

&nbsp;5%          229             30

10%          317             37       Obs               4,128

25%          465             39       Sum of wgt.       4,128



50%          633                      Mean           602.1703

&nbsp;                       Largest       Std. dev.      196.3854

75%          769            882

90%          839            882       Variance       38567.23

95%          861            882       Skewness      -.5769655

99%          877            882       Kurtosis       2.478396



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 2



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          887            883

&nbsp;5%          906            883

10%          927            883       Obs               4,128

25%          993            883       Sum of wgt.       4,128



50%         1103                      Mean           1102.643

&nbsp;                       Largest       Std. dev.      125.8139

75%         1214           1316

90%         1275           1316       Variance       15829.14

95%         1296           1316       Skewness      -.0202819

99%         1312           1316       Kurtosis         1.7858



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 3



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         1321           1317

&nbsp;5%         1340           1317

10%         1362           1317       Obs               4,116

25%         1429           1317       Sum of wgt.       4,116



50%         1537                      Mean           1536.659

&nbsp;                       Largest       Std. dev.      126.3234

75%         1645           1755

90%         1712           1755       Variance       15957.61

95%         1735           1755       Skewness       .0002391

99%         1750           1755       Kurtosis        1.80471



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 4



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         1760           1756

&nbsp;5%         1779           1756

10%         1803           1756       Obs               4,123

25%         1872           1756       Sum of wgt.       4,123



50%         1997                      Mean           1999.089

&nbsp;                       Largest       Std. dev.      144.4825

75%         2121           2259

90%         2203           2259       Variance       20875.18

95%         2231           2259       Skewness       .0658324

99%         2254           2259       Kurtosis       1.824969



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 5



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         2266           2260

&nbsp;5%         2290           2260

10%         2318           2260       Obs               4,127

25%         2407           2260       Sum of wgt.       4,127



50%         2560                      Mean           2564.854

&nbsp;                       Largest       Std. dev.      181.7103

75%         2719           2889

90%         2823           2889       Variance       33018.63

95%         2859           2889       Skewness       .0772457

99%         2883           2889       Kurtosis        1.82064



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 6



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         2898           2890

&nbsp;5%         2925           2890

10%         2955           2890       Obs               4,122

25%         3068           2891       Sum of wgt.       4,122



50%       3271.5                      Mean           3282.392

&nbsp;                       Largest       Std. dev.      243.5633

75%         3490           3732

90%         3630           3732       Variance       59323.09

95%         3680           3732       Skewness       .1240458

99%         3719           3732       Kurtosis       1.811817



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 7



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         3742           3733

&nbsp;5%         3781           3733

10%         3835           3734       Obs               4,120

25%         4003           3734       Sum of wgt.       4,120



50%         4290                      Mean           4318.513

&nbsp;                       Largest       Std. dev.      363.8472

75%         4626           4998

90%         4845           4999       Variance       132384.8

95%         4920           4999       Skewness       .1536063

99%         4981           4999       Kurtosis       1.835478



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 8



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         5016           5001

&nbsp;5%         5081           5001

10%         5165           5001       Obs               4,124

25%         5424           5002       Sum of wgt.       4,124



50%         5907                      Mean           5956.008

&nbsp;                       Largest       Std. dev.        603.46

75%         6462           7103

90%         6827           7103       Variance         364164

95%         6962           7104       Skewness       .1843499

99%         7077           7104       Kurtosis       1.847123



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 9



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%         7134           7106

&nbsp;5%         7244           7106

10%         7394           7107       Obs               4,123

25%         7919           7107       Sum of wgt.       4,123



50%         8875                      Mean           9066.034

&nbsp;                       Largest       Std. dev.      1322.711

75%        10112          11734

90%        11093          11735       Variance        1749565

95%        11393          11735       Skewness       .3382874

99%        11672          11735       Kurtosis       1.958206



--------------------------------------------------------------------------------------------------------------------------

-> UKB\_WMHs2 = 10



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%        11824          11737

&nbsp;5%        12141          11738

10%        12552          11742       Obs               4,123

25%        13975          11743       Sum of wgt.       4,123



50%        17297                      Mean           20959.48

&nbsp;                       Largest       Std. dev.      10921.24

75%        23701         100316

90%        33667         101414       Variance       1.19e+08

95%        42584         111188       Skewness       2.725469

99%        63632         113750       Kurtosis       13.84181



. rename UKB\_WMHs2 x25781\_new



