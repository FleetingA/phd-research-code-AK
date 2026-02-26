### **\*\*\*\*\* Variable Transformations to try and counteract PHESANT errors \*\*\*\*\***

### **\*\*\*\*\* Date: 10/04/2025 \*\*\*\*\***

### **\*\*\*\*\* Author: Angelina Kancheva \*\*\*\*\***



1\. Data Field 20116 - Smoking Status



Coding 90:



-3	Prefer not to answer

0	Never

1	Previous

2	Current



2 --> 1 = 2377; 1 --> 0 = 26236; 0 --> 0 = 69619



2\. Data Field 20117 - Alcohol Drinker Status



Was:



-3     0     1     2

30   2362  2596 64885



Changed to:



 0     1

4958 64885



3\. Data Field 20126 - Bipolar and Major Depression Status



Was:



 0       1     2     3     4     5

89499   808   807  8899 15003  7921



1=1|2=1|3=2|4=2|5=2



Make all bipolar=1

Make all depression=2

No bipolar or depression remains 0



1+2 = 1615

3+4+5 = 31823



Is:



 0     1     2

89499  1615 31823



4\. Data Field 20165 - FI1 : numeric addition test



Recode to make binary:



20165,0,,15=1|13=0|14=0|16=0|17=0,,



5\. Data Field 5699 - FI10 : arithmetic sequence recognition



Recode to make binary:



5699,0,,92=0|93=0|94=0|95=1|96=0,,



Was:



-3    -1    92    93    94    95    96

159  1338   478  930   713  11521   669



Is:



-3    -1     0     1

159  1338  2790 11521



6\. Data Field 5779 - FI11 : antonym



Recode to make binary:



5 --> 1; All else --> 0



Was:



-3   -1    1    2    3    4    5

11    6   183 1775   5  330  5973



Is:



0    1

2310 5973



7\. Data Field 20167 - FI2 : identify largest number



Recode to make binary:



20167,0,,987=1|642=0|308=0|714=0|253=0|-121=NA|-818=NA,,



Was:



253    308    642    714    987

45     12     56     959  122071



Is:

0      1

1072 122071



8\. Data Field 20169 - FI3 : word interpolation



Recode to make binary:



20169,0,,4=1|3=0|2=0|1=0|5=0|-121=NA|-818=NA,,



Was:



1      2      3      4      5

11793  8731  101  102336   148



Is:



0      1

20773 102336



9\. Data Field 20171 - FI4 : positional arithmetic



Recode to make binary:



20171,0,,6=1|5=0|7=0|8=0|9=0,,



Was:



5      6      7      8      9

6850 108577   928   1887   4227



Is:



0      1

13892 108577





10\. Data Field 20177 - FI7 : synonym



Recode to make binary:



Was:



1     2     3     4     5

4036 822  81480  508   547



Is:



0     1

5913 81480





11\. Data Field 20179 - FI8 : chained arithmetic



Recode to make binary:



20179,0,,26=1|25=0|27=0|28=0|29=0,,



Was:



25    26    27    28    29

9132 60174  4582  2234  3373



12\. Data Field 20181 - FI9 : concept interpolation



Recode to make binary:



20181,0,,4=1|1=0|2=0|3=0|5=0,,



13\. Data Field 20183 - FI10 : arithmetic sequence recognition



Same as Data Field 5699.



14\. Data Field 4935 - FI1 : numeric addition test



Same as 20165.



15\. Data Field 4946 - FI2 : identify largest number



Same as 20167.



16\. Data Field 4968 - FI4 : positional arithmetic



Same as 20171 but removing the category 9.



17\. Data Field 4990 - FI6 : conditional arithmetic



Recode to make binary:



4990,0,,70=1|68=0|69=0|71=0|72=0,,



18\. Data Field 5556 - FI7 : synonym



Recode to make binary:



5556,0,,4=1|3=0|2=0|1=0|5=0|-1=NA|-3=NA,,



19\. Data Field 1200 - Sleeplessness / insomnia



Recode to make binary:



1200,0,,1=0|2=1|3=1,,



Was:



 -3     1     2     3

 61 14825 31863 23124



20\. Data Field 1220 - Daytime dozing / sleeping





Coding	Meaning

0	Never/rarely

1	Sometimes

2	Often

-1	Do not know

-3	Prefer not to answer

3	All of the time



Was:



 -3    -1     0     1     2

 32   100 53074  14886  1781



Make:



1220,0,,1=0|0=0|3=1|2=1,,



21\. Data Field 1239 - Current tobacco smoking



Recode to make binary:



1239,0,,1=1|2=1|0=0,,



Was:



3     0     1     2

13 67483  1426   951



22\. Data Field 1558 - Alcohol intake frequency



Recode to make into three categories:



1558,0,,1=1|2=1|3=1|4=2|5=2|6=0,,



Was:



3     1     2     3     4     5     6

26 11772 19199 18445  8081  7388  4962

