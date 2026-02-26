*** Computing Average Years/Months/Days of follow-up for Second UKB PhD Project ***

*** Author: Angelina Kancheva ***

*** Date: 28 February 2025 *** 



\*\* Years, Months and Days \*\*



. import delimited "C:\\Users\\angel\\Desktop\\PHEWAS\_PhD\_Project\_2023\\UKB Data 2025\\Date\_Differences\_in\_UKB.csv"

(encoding automatically selected: ISO-8859-1)

(9 vars, 6,330 obs)



. browse 



. describe



Contains data



&nbsp;Observations:         6,330                  

&nbsp;   Variables:             9      

&nbsp;           

------------------------------------------------------------------------------------------------------------------------

Variable      Storage   Display    Value

&nbsp;   name         type    format    label      Variable label

------------------------------------------------------------------------------------------------------------------------

userid          long    %12.0g                userId

x25781\_2\_0      long    %12.0g                

x25781\_3\_0      long    %12.0g                

x53\_0\_0         str10   %10s                  

x53\_2\_0         str10   %10s                  

x53\_3\_0         str10   %10s                  

xyears\_2        byte    %8.0g                 xYears\_2

xmonths\_2       byte    %8.0g                 xMonths\_2

xdays\_2         byte    %8.0g                 xDays\_2

------------------------------------------------------------------------------------------------------------------------

Sorted by: 

&nbsp;    Note: Dataset has changed since last saved.





. summarize



&nbsp;   Variable |        Obs        Mean    Std. dev.       Min        Max

-------------+---------------------------------------------------------

&nbsp;     userid |      6,330     3505087     1461146    1000544    6026186

&nbsp; x25781\_2\_0 |      5,876    4687.463    6150.704          9     100316

&nbsp; x25781\_3\_0 |      4,763    5179.773    6552.066         82      76853

&nbsp;    x53\_0\_0 |          0

&nbsp;    x53\_2\_0 |          0

-------------+---------------------------------------------------------

&nbsp;    x53\_3\_0 |          0

&nbsp;   xyears\_2 |      6,330     2.68752    1.324318          1          8

&nbsp;  xmonths\_2 |      6,330    3.856398    3.217174          0         11

&nbsp;    xdays\_2 |      6,330    14.65687    8.832644          0         30



. tab  xyears\_2



&nbsp;  xYears\_2 |      Freq.     Percent        Cum.

------------+-----------------------------------

&nbsp;         1 |        428        6.76        6.76

&nbsp;         2 |      4,034       63.73       70.49

&nbsp;         3 |        290        4.58       75.07

&nbsp;         4 |        430        6.79       81.86

&nbsp;         5 |        971       15.34       97.20

&nbsp;         6 |        169        2.67       99.87

&nbsp;         7 |          7        0.11       99.98

&nbsp;         8 |          1        0.02      100.00

------------+-----------------------------------

&nbsp;     Total |      6,330      100.00



. summarize, detail



&nbsp;                          userId

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%      1051896        1000544

&nbsp;5%      1256135        1000818

10%      1477685        1001223       Obs               6,330

25%      2226437        1004807       Sum of wgt.       6,330



50%      3517044                      Mean            3505087

&nbsp;                       Largest       Std. dev.       1461146

75%      4795961        6023993

90%      5509916        6025655       Variance       2.13e+12

95%      5769746        6026000       Skewness      -.0036869

99%      5975550        6026186       Kurtosis       1.774401



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          308              9

&nbsp;5%          627             68

10%          859             94       Obs               5,876

25%       1504.5            106       Sum of wgt.       5,876



50%         2731                      Mean           4687.463

&nbsp;                       Largest       Std. dev.      6150.704

75%       5319.5          63252

90%        10378          63322       Variance       3.78e+07

95%        15620          89104       Skewness       4.528585

99%        29537         100316       Kurtosis       37.57601



&nbsp;                        x25781\_3\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          358             82

&nbsp;5%          664            136

10%          942            136       Obs               4,763

25%         1587            138       Sum of wgt.       4,763



50%         2941                      Mean           5179.773

&nbsp;                       Largest       Std. dev.      6552.066

75%         6046          68604

90%        11639          69904       Variance       4.29e+07

95%        17545          70235       Skewness       3.718858

99%        32702          76853       Kurtosis       23.73692





&nbsp;                         xYears\_2

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%            1              1

&nbsp;5%            1              1

10%            2              1       Obs               6,330

25%            2              1       Sum of wgt.       6,330



50%            2                      Mean            2.68752

&nbsp;                       Largest       Std. dev.      1.324318

75%            3              7

90%            5              7       Variance       1.753818

95%            5              7       Skewness       1.120827

99%            6              8       Kurtosis       2.927568



&nbsp;                         xMonths\_2

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%            0              0

&nbsp;5%            0              0

10%            0              0       Obs               6,330

25%            2              0       Sum of wgt.       6,330



50%            3                      Mean           3.856398

&nbsp;                       Largest       Std. dev.      3.217174

75%            6             11

90%           10             11       Variance       10.35021

95%           11             11       Skewness       .9038351

99%           11             11       Kurtosis       2.731278



&nbsp;                          xDays\_2

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%            0              0

&nbsp;5%            1              0

10%            2              0       Obs               6,330

25%            7              0       Sum of wgt.       6,330



50%           14                      Mean           14.65687

&nbsp;                       Largest       Std. dev.      8.832644

75%           22             30

90%           27             30       Variance        78.0156

95%           29             30       Skewness        .022039

99%           30             30       Kurtosis       1.821921



\*\* Days Only \*\*





. import delimited "C:\\Users\\angel\\Desktop\\PHEWAS\_PhD\_Project\_2023\\UKB Data 2025\\Date\_Differences\_in\_Days\_Only.csv"

(encoding automatically selected: UTF-8)

(7 vars, 6,330 obs)



. browse



. describe



Contains data



&nbsp;Observations:         6,330                  

&nbsp;   Variables:             7           



&nbsp;      

------------------------------------------------------------------------------------------------------------------------

Variable      Storage   Display    Value

&nbsp;   name         type    format    label      Variable label

------------------------------------------------------------------------------------------------------------------------

userid          long    %12.0g                userId

x25781\_2\_0      long    %12.0g                

x25781\_3\_0      long    %12.0g                

x53\_0\_0         str10   %10s                  

x53\_2\_0         str10   %10s                  

x53\_3\_0         str10   %10s                  

xdays\_2         int     %8.0g                 xDays\_2

------------------------------------------------------------------------------------------------------------------------

Sorted by: 

&nbsp;    Note: Dataset has changed since last saved.





. summarize, detail



&nbsp;                          userId

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%      1051896        1000544

&nbsp;5%      1256135        1000818

10%      1477685        1001223       Obs               6,330

25%      2226437        1004807       Sum of wgt.       6,330



50%      3517044                      Mean            3505087

&nbsp;                       Largest       Std. dev.       1461146

75%      4795961        6023993

90%      5509916        6025655       Variance       2.13e+12

95%      5769746        6026000       Skewness      -.0036869

99%      5975550        6026186       Kurtosis       1.774401



&nbsp;                        x25781\_2\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          308              9

&nbsp;5%          627             68

10%          859             94       Obs               5,876

25%       1504.5            106       Sum of wgt.       5,876



50%         2731                      Mean           4687.463

&nbsp;                       Largest       Std. dev.      6150.704

75%       5319.5          63252

90%        10378          63322       Variance       3.78e+07

95%        15620          89104       Skewness       4.528585

99%        29537         100316       Kurtosis       37.57601



&nbsp;                        x25781\_3\_0

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          358             82

&nbsp;5%          664            136

10%          942            136       Obs               4,763

25%         1587            138       Sum of wgt.       4,763



50%         2941                      Mean           5179.773

&nbsp;                       Largest       Std. dev.      6552.066

75%         6046          68604

90%        11639          69904       Variance       4.29e+07

95%        17545          70235       Skewness       3.718858

99%        32702          76853       Kurtosis       23.73692





&nbsp;                          xDays\_2

-------------------------------------------------------------

&nbsp;     Percentiles      Smallest

&nbsp;1%          522            366

&nbsp;5%          679            379

10%          751            398       Obs               6,330

25%          800            400       Sum of wgt.       6,330



50%          847                      Mean           1113.533

&nbsp;                       Largest       Std. dev.      496.4706

75%         1447           2628

90%         1892           2638       Variance       246483.1

95%         2083           2680       Skewness       1.138364

99%         2392           3177       Kurtosis       2.828718



. summarize



&nbsp;   Variable |        Obs        Mean    Std. dev.       Min        Max

-------------+---------------------------------------------------------

&nbsp;     userid |      6,330     3505087     1461146    1000544    6026186

&nbsp; x25781\_2\_0 |      5,876    4687.463    6150.704          9     100316

&nbsp; x25781\_3\_0 |      4,763    5179.773    6552.066         82      76853

&nbsp;    x53\_0\_0 |          0

&nbsp;    x53\_2\_0 |          0

-------------+---------------------------------------------------------

&nbsp;    x53\_3\_0 |          0

&nbsp;    xdays\_2 |      6,330    1113.533    496.4706        366       3177











