### **\*\*\* Slicing UKB Data for First UKB PheWAS PhD Project \*\*\***

### **\*\*\* Author: Angelina Kancheva \*\*\***

### **\*\*\* Date: 1st sept 2023 \*\*\***





use "C:\\Users\\dl143j\\OneDrive - University of Glasgow\\Data\\raw\_UKB\_Feb22\\50473\_compressed.dta"



merge 1:1 n\_eid using "C:\\Users\\dl143j\\OneDrive - University of Glasgow\\Data\\raw\_UKB\_Feb22\\51305.dta",

keep if \_merge==3

drop \_merge



merge 1:1 n\_eid using "C:\\Users\\dl143j\\OneDrive - University of Glasgow\\Data\\raw\_UKB\_Feb22\\51305.dta",

keep if \_merge==3

drop \_merge



destring, replace

compress

save "C:\\Users\\dl143j\\OneDrive - University of Glasgow\\My Documents\\Projects\\Students\\Angelina\\Angelina\_01sept23.dta", replace

export delimited using "C:\\Users\\dl143j\\OneDrive - University of Glasgow\\My Documents\\Projects\\Students\\Angelina\\Angelina\_01sept23.csv", delimiter(tab) replace





Slicing the UKB data in Stata - New variables added as of 22/01/2024:



use n\_eid n\_31\_0\_0 n\_34\_0\_0 n\_1647\_2\_0 n\_21022\_0\_0 n\_21000\_0\_0 n\_1707\_2\_0 n\_21001\_2\_0 n\_21002\_2\_0 n\_93\_2\_0 n\_94\_2\_0 n\_95\_2\_0 n\_4079\_2\_0 n\_4080\_2\_0 n\_137\_2\_0 n\_2443\_2\_0 n\_6150\_2\_0 n\_3894\_2\_0 n\_4012\_2\_0 n\_4022\_2\_0 n\_4056\_2\_0 n\_1200\_2\_0 n\_1210\_2\_0 n\_1220\_2\_0 n\_3751\_2\_0 n\_3786\_2\_0 n\_4717\_2\_0 n\_22150\_0\_0 n\_1920\_2\_0 n\_1930\_2\_0 n\_1940\_2\_0 n\_1950\_2\_0 n\_1960\_2\_0 n\_1970\_2\_0 n\_1980\_2\_0 n\_1990\_2\_0 n\_2020\_2\_0 n\_2050\_2\_0 n\_2060\_2\_0 n\_2070\_2\_0 n\_2080\_2\_0 n\_4598\_2\_0 n\_4609\_2\_0 n\_4620\_2\_0 n\_4631\_2\_0 n\_5375\_2\_0 n\_6145\_2\_0 n\_20122\_0\_0 n\_20126\_0\_0 n\_2188\_2\_0 n\_2178\_2\_0 n\_2956\_2\_0 n\_3404\_2\_0 n\_3414\_2\_0 n\_3606\_2\_0 n\_3616\_2\_0 n\_3741\_2\_0 n\_3773\_2\_0 n\_4067\_2\_0 n\_4728\_2\_0 n\_5452\_2\_0 n\_5463\_2\_0 n\_5474\_2\_0 n\_5485\_2\_0 n\_5496\_2\_0 n\_5507\_2\_0 n\_5518\_2\_0 n\_21024\_0\_0 n\_924\_2\_0 n\_874\_2\_0 n\_981\_2\_0 n\_1001\_2\_0 n\_2296\_2\_0 n\_2335\_2\_0 n\_3005\_2\_0 n\_1239\_2\_0 n\_1249\_2\_0 n\_20116\_2\_0 n\_1558\_2\_0 n\_20117\_2\_0 n\_25009\_2\_0 n\_25781\_2\_0 n\_2247\_2\_0 n\_2257\_2\_0 n\_4642\_2\_0 n\_4653\_2\_0 n\_6156\_2\_0 n\_5663\_2\_0 n\_5674\_2\_0 n\_4728\_2\_0 n\_5452\_2\_0 n\_5463\_2\_0 n\_5474\_2\_0 n\_5485\_2\_0 n\_5496\_2\_0 n\_5507\_2\_0 n\_5518\_2\_0 n\_6159\_2\_0 n\_20023\_2\_0 n\_20016\_2\_0 n\_5699\_2\_0 n\_5556\_2\_0 n\_5779\_2\_0 n\_5790\_2\_0 n\_21048\_0\_0 n\_6015\_0\_0 n\_6016\_0\_0 n\_3571\_2\_0 n\_5663\_2\_0 n\_5674\_2\_0 n\_20532\_0\_0 n\_20533\_0\_0 n\_20534\_0\_0 n\_20535\_0\_0 n\_20536\_0\_0 n\_20537\_0\_0 n\_20538\_0\_0 n\_20539\_0\_0 n\_20540\_0\_0 n\_20541\_0\_0 n\_20542\_0\_0 n\_20543\_0\_0 n\_20122\_0\_0 n\_20123\_0\_0 n\_20124\_0\_0 n\_20125\_0\_0 n\_20126\_0\_0 n\_20127\_0\_0 n\_20128\_2\_0 n\_20165\_0\_0 n\_20167\_0\_0 n\_20169\_0\_0 n\_20171\_0\_0 n\_20173\_0\_0 n\_20175\_0\_0 n\_20177\_0\_0 n\_20179\_0\_0 n\_20181\_0\_0 n\_20183\_0\_0 n\_20185\_0\_0 n\_20187\_0\_0 n\_20189\_0\_0 n\_20191\_0\_0 n\_20193\_0\_0 n\_20195\_0\_0 n\_20240\_0\_0 n\_2247\_2\_0 n\_23324\_2\_0 n\_6373\_2\_0 n\_6348\_2\_0 n\_6349\_2\_0 n\_6350\_2\_0 n\_6351\_2\_0 n\_20247\_0\_0 n\_20248\_0\_0 n\_25781\_2\_0 n\_26410\_0\_0 n\_4935\_2\_0 n\_4924\_2\_0 n\_4946\_2\_0 n\_4935\_2\_0 n\_4957\_2\_0 n\_4968\_2\_0 n\_4979\_2\_0 n\_4990\_2\_0 n\_5001\_2\_0 n\_5012\_2\_0 n\_1160\_2\_0 n\_884\_2\_0 n\_894\_2\_0 n\_904\_2\_0 n\_914\_2\_0 n\_943\_2\_0 n\_971\_2\_0 n\_991\_2\_0 n\_1001\_2\_0 n\_1011\_2\_0 n\_1021\_2\_0 n\_1289\_2\_0 n\_1299\_2\_0 n\_1309\_2\_0 n\_1498\_2\_0 n\_1538\_2\_0 n\_2000\_2\_0 n\_2010\_2\_0 n\_2020\_2\_0 n\_2030\_2\_0 n\_2040\_2\_0 n\_2050\_2\_0 n\_2090\_2\_0 n\_2100\_2\_0 n\_2110\_2\_0 n\_2207\_2\_0 n\_2217\_2\_0 n\_2227\_2\_0 n\_23104\_2\_0 n\_2316\_2\_0 n\_2966\_2\_0 n\_2976\_2\_0 n\_3393\_2\_0 n\_3627\_2\_0 n\_3799\_2\_0 n\_21064\_0\_0 n\_21065\_0\_0 n\_4526\_2\_0 n\_4537\_2\_0 n\_4548\_2\_0 n\_4559\_2\_0 n\_4570\_2\_0 n\_4581\_2\_0 n\_4598\_2\_0 n\_4689\_2\_0 n\_4700\_2\_0 n\_4717\_2\_0 n\_4728\_2\_0 n\_4792\_2\_0 n\_5866\_2\_0 n\_6148\_2\_0 n\_6149\_2\_0 n\_20418\_0\_0 n\_20419\_0\_0 n\_20420\_0\_0 n\_20421\_0\_0 n\_20422\_0\_0 n\_20423\_0\_0 n\_20425\_0\_0 n\_20426\_0\_0 n\_20427\_0\_0 n\_20437\_0\_0 n\_20449\_0\_0 n\_20450\_0\_0 n\_20458\_0\_0 n\_20459\_0\_0 n\_25003\_2\_0 n\_25004\_2\_0 n\_25005\_2\_0 n\_25006\_2\_0 n\_25007\_2\_0 n\_25008\_2\_0  using "C:\\Users\\angel\\Desktop\\2024\_PHESANT-master\\UKB Data 2024\\23Jan2024"



save "C:\\Users\\angel\\Desktop\\2024\_PHESANT-master\\UKB Data 2024\\23Jan2024", replace



Result after loading the data in Stata:



n\_189\_0\_0 not found > deprivation index =>> removed

n\_6015\_2\_0 not found > chest pain felt during physical activity =>> changed to n\_6015\_0\_0

n\_6016\_2\_0 not found > chest pain felt outside physical activity =>> changed to n\_6016\_0\_0

n\_396\_2\_0 not found > number of columns displayed in round =>> removed

n\_397\_2\_0 not found > number of rows displayed in round =>> removed 

n\_398\_2\_0 not found > number of correct matches in round =>> removed

n\_399\_2\_0 not found > number of incorrect matches in round =>> removed

n\_400\_2\_0 not found > time to complete round =>> removed

n\_4282\_2\_0 not found > maximum digits remembered correctly 

n\_21064\_2\_0 not found > sensitive stomach =>> changed to n\_21064\_0\_0

n\_21065\_2\_0 not found > family history of IBS =>> changed to n\_21065\_0\_0



Full list of 218 variables as of 23/01/2024:



------------------------------------------------------------------------------------------------------------

Variable      Storage   Display    Value

&nbsp;   name         type    format    label      Variable label

------------------------------------------------------------------------------------------------------------

n\_eid           long    %12.0g                Encoded anonymised participant ID

n\_31\_0\_0        byte    %8.0g      m\_0009     Sex

n\_34\_0\_0        int     %12.0g                Year of birth

n\_93\_2\_0        int     %8.0g                 Systolic blood pressure, manual reading

n\_94\_2\_0        int     %8.0g                 Diastolic blood pressure, manual reading

n\_95\_2\_0        int     %8.0g                 Pulse rate (during blood-pressure measurement)

n\_137\_2\_0       byte    %8.0g                 Number of treatments/medications taken

n\_874\_2\_0       int     %20.0g     m\_100291   Duration of walks

n\_884\_2\_0       byte    %20.0g     m\_100291   Number of days/week of moderate physical activity 10+ minutes

n\_894\_2\_0       int     %20.0g     m\_100291   Duration of moderate activity

n\_904\_2\_0       byte    %20.0g     m\_100291   Number of days/week of vigorous physical activity 10+ minutes

n\_914\_2\_0       int     %20.0g     m\_100291   Duration of vigorous activity

n\_924\_2\_0       byte    %20.0g     m\_100313   Usual walking pace

n\_943\_2\_0       byte    %24.0g     m\_100314   Frequency of stair climbing in last 4 weeks

n\_971\_2\_0       byte    %29.0g     m\_100317   Frequency of walking for pleasure in last 4 weeks

n\_981\_2\_0       byte    %29.0g     m\_100318   Duration walking for pleasure

n\_991\_2\_0       byte    %29.0g     m\_100317   Frequency of strenuous sports in last 4 weeks

n\_1001\_2\_0      byte    %29.0g     m\_100318   Duration of strenuous sports

n\_1011\_2\_0      byte    %29.0g     m\_100317   Frequency of light DIY in last 4 weeks

n\_1021\_2\_0      byte    %29.0g     m\_100318   Duration of light DIY

n\_1160\_2\_0      byte    %20.0g     m\_100291   Sleep duration

n\_1200\_2\_0      byte    %20.0g     m\_100343   Sleeplessness / insomnia

n\_1210\_2\_0      byte    %20.0g     m\_100345   Snoring

n\_1220\_2\_0      byte    %20.0g     m\_100346   Daytime dozing / sleeping

n\_1239\_2\_0      byte    %24.0g     m\_100347   Current tobacco smoking

n\_1249\_2\_0      byte    %26.0g     m\_100348   Past tobacco smoking

n\_1289\_2\_0      byte    %20.0g     m\_100373   Cooked vegetable intake

n\_1299\_2\_0      byte    %20.0g     m\_100373   Salad / raw vegetable intake

n\_1309\_2\_0      byte    %20.0g     m\_100373   Fresh fruit intake

n\_1498\_2\_0      byte    %20.0g     m\_100373   Coffee intake

n\_1538\_2\_0      byte    %29.0g     m\_100400   Major dietary changes in the last 5 years

n\_1558\_2\_0      byte    %26.0g     m\_100402   Alcohol intake frequency.

n\_1647\_2\_0      byte    %20.0g     m\_100420   Country of birth (UK/elsewhere)

n\_1707\_2\_0      byte    %37.0g     m\_100430   Handedness (chirality/laterality)

n\_1920\_2\_0      byte    %20.0g     m\_100349   Mood swings

n\_1930\_2\_0      byte    %20.0g     m\_100349   Miserableness

n\_1940\_2\_0      byte    %20.0g     m\_100349   Irritability

n\_1950\_2\_0      byte    %20.0g     m\_100349   Sensitivity / hurt feelings

n\_1960\_2\_0      byte    %20.0g     m\_100349   Fed-up feelings

n\_1970\_2\_0      byte    %20.0g     m\_100349   Nervous feelings

n\_1980\_2\_0      byte    %20.0g     m\_100349   Worrier / anxious feelings

n\_1990\_2\_0      byte    %20.0g     m\_100349   Tense / highly strung

n\_2000\_2\_0      byte    %20.0g     m\_100349   Worry too long after embarrassment

n\_2010\_2\_0      byte    %20.0g     m\_100349   Suffer from nerves

n\_2020\_2\_0      byte    %20.0g     m\_100349   Loneliness, isolation

n\_2030\_2\_0      byte    %20.0g     m\_100349   Guilty feelings

n\_2040\_2\_0      byte    %20.0g     m\_100349   Risk taking

n\_2050\_2\_0      byte    %23.0g     m\_100484   Frequency of depressed mood in last 2 weeks

n\_2060\_2\_0      byte    %23.0g     m\_100484   Frequency of unenthusiasm / disinterest in last 2 weeks

n\_2070\_2\_0      byte    %23.0g     m\_100484   Frequency of tenseness / restlessness in last 2 weeks

n\_2080\_2\_0      byte    %23.0g     m\_100484   Frequency of tiredness / lethargy in last 2 weeks

n\_2090\_2\_0      byte    %20.0g     m\_100349   Seen doctor (GP) for nerves, anxiety, tension or depression

n\_2100\_2\_0      byte    %20.0g     m\_100349   Seen a psychiatrist for nerves, anxiety, tension or depression

n\_2110\_2\_0      byte    %21.0g     m\_100501   Able to confide

n\_2178\_2\_0      byte    %20.0g     m\_100508   Overall health rating

n\_2188\_2\_0      byte    %20.0g     m\_100349   Long-standing illness, disability or infirmity

n\_2207\_2\_0      byte    %20.0g     m\_100352   Wears glasses or contact lenses

n\_2217\_2\_0      byte    %20.0g     m\_100291   Age started wearing glasses or contact lenses

n\_2227\_2\_0      byte    %20.0g     m\_100352   Other eye problems

n\_2247\_2\_0      byte    %20.0g     m\_100631   Hearing difficulty/problems

n\_2257\_2\_0      byte    %20.0g     m\_100349   Hearing difficulty/problems with background noise

n\_2296\_2\_0      byte    %20.0g     m\_100539   Falls in the last year

n\_2316\_2\_0      byte    %20.0g     m\_100349   Wheeze or whistling in the chest in last year

n\_2335\_2\_0      byte    %20.0g     m\_100349   Chest pain or discomfort

n\_2443\_2\_0      byte    %20.0g     m\_100349   Diabetes diagnosed by doctor

n\_2956\_2\_0      byte    %20.0g     m\_100349   General pain for 3+ months

n\_2966\_2\_0      byte    %20.0g     m\_100291   Age high blood pressure diagnosed

n\_2976\_2\_0      byte    %20.0g     m\_100291   Age diabetes diagnosed

n\_3005\_2\_0      byte    %20.0g     m\_100349   Fracture resulting from simple fall

n\_3393\_2\_0      byte    %20.0g     m\_100352   Hearing aid user

n\_3404\_2\_0      byte    %20.0g     m\_100349   Neck/shoulder pain for 3+ months

n\_3414\_2\_0      byte    %20.0g     m\_100349   Hip pain for 3+ months

n\_3571\_2\_0      byte    %20.0g     m\_100349   Back pain for 3+ months

n\_3606\_2\_0      byte    %27.0g     m\_100563   Chest pain or discomfort walking normally

n\_3616\_2\_0      byte    %20.0g     m\_100349   Chest pain due to walking ceases when standing still

n\_3627\_2\_0      byte    %20.0g     m\_100291   Age angina diagnosed

n\_3741\_2\_0      byte    %20.0g     m\_100349   Stomach/abdominal pain for 3+ months

n\_3751\_2\_0      byte    %35.0g     m\_100564   Chest pain or discomfort when walking uphill or hurrying

n\_3773\_2\_0      byte    %20.0g     m\_100349   Knee pain for 3+ months

n\_3786\_2\_0      byte    %20.0g     m\_100291   Age asthma diagnosed

n\_3799\_2\_0      byte    %20.0g     m\_100349   Headaches for 3+ months

n\_3894\_2\_0      byte    %20.0g     m\_100291   Age heart attack diagnosed

n\_4012\_2\_0      byte    %20.0g     m\_100291   Age deep-vein thrombosis (DVT, blood clot in leg) diagnosed

n\_4022\_2\_0      byte    %20.0g     m\_100291   Age pulmonary embolism (blood clot in lung) diagnosed

n\_4056\_2\_0      byte    %20.0g     m\_100291   Age stroke diagnosed

n\_4067\_2\_0      byte    %20.0g     m\_100349   Facial pains for 3+ months

n\_4079\_2\_0      int     %8.0g                 Diastolic blood pressure, automated reading

n\_4080\_2\_0      int     %8.0g                 Systolic blood pressure, automated reading

n\_4526\_2\_0      byte    %20.0g     m\_100478   Happiness

n\_4537\_2\_0      byte    %20.0g     m\_100479   Work/job satisfaction

n\_4548\_2\_0      byte    %20.0g     m\_100478   Health satisfaction

n\_4559\_2\_0      byte    %20.0g     m\_100478   Family relationship satisfaction

n\_4570\_2\_0      byte    %20.0g     m\_100478   Friendships satisfaction

n\_4581\_2\_0      byte    %20.0g     m\_100478   Financial situation satisfaction

n\_4598\_2\_0      byte    %20.0g     m\_100349   Ever depressed for a whole week

n\_4609\_2\_0      int     %20.0g     m\_100291   Longest period of depression

n\_4620\_2\_0      int     %20.0g     m\_100291   Number of depression episodes

n\_4631\_2\_0      byte    %20.0g     m\_100349   Ever unenthusiastic/disinterested for a whole week

n\_4642\_2\_0      byte    %20.0g     m\_100349   Ever manic/hyper for 2 days

n\_4653\_2\_0      byte    %20.0g     m\_100349   Ever highly irritable/argumentative for 2 days

n\_4689\_2\_0      byte    %20.0g     m\_100291   Age glaucoma diagnosed

n\_4700\_2\_0      byte    %20.0g     m\_100291   Age cataract diagnosed

n\_4717\_2\_0      byte    %20.0g     m\_100349   Shortness of breath walking on level ground

n\_4728\_2\_0      byte    %20.0g     m\_100349   Leg pain on walking

n\_4792\_2\_0      byte    %20.0g     m\_100352   Cochlear implant

n\_4924\_2\_0      byte    %23.0g     m\_100642   Attempted fluid intelligence (FI) test.

n\_4935\_2\_0      byte    %20.0g     m\_100643   FI1 : numeric addition test

n\_4946\_2\_0      int     %20.0g     m\_100644   FI2 : identify largest number

n\_4957\_2\_0      byte    %20.0g     m\_100645   FI3 : word interpolation

n\_4968\_2\_0      byte    %20.0g     m\_100646   FI4 : positional arithmetic

n\_4979\_2\_0      byte    %20.0g     m\_100647   FI5 : family relationship calculation

n\_4990\_2\_0      byte    %20.0g     m\_100648   FI6 : conditional arithmetic

n\_5001\_2\_0      byte    %20.0g     m\_100649   FI7 : synonym

n\_5012\_2\_0      byte    %20.0g     m\_100650   FI8 : chained arithmetic

n\_5375\_2\_0      int     %20.0g     m\_100291   Longest period of unenthusiasm / disinterest

n\_5452\_2\_0      byte    %20.0g     m\_100349   Leg pain when standing still or sitting

n\_5463\_2\_0      byte    %20.0g     m\_100349   Leg pain in calf/calves

n\_5474\_2\_0      byte    %20.0g     m\_100349   Leg pain when walking uphill or hurrying

n\_5485\_2\_0      byte    %20.0g     m\_100349   Leg pain when walking normally

n\_5496\_2\_0      byte    %20.0g     m\_100349   Leg pain when walking ever disappears while walking

n\_5507\_2\_0      byte    %21.0g     m\_100549   Leg pain on walking : action taken

n\_5518\_2\_0      byte    %47.0g     m\_100550   Leg pain on walking : effect of standing still

n\_5556\_2\_0      byte    %20.0g     m\_100651   FI9 : concept interpolation

n\_5663\_2\_0      byte    %39.0g     m\_100499   Length of longest manic/irritable episode

n\_5674\_2\_0      byte    %104.0g    m\_100500   Severity of manic/irritable episodes

n\_5699\_2\_0      byte    %20.0g     m\_100652   FI10 : arithmetic sequence recognition

n\_5779\_2\_0      byte    %20.0g     m\_100653   FI11 : antonym

n\_5790\_2\_0      byte    %20.0g     m\_100654   FI12 : square sequence recognition

n\_5866\_2\_0      byte    %22.0g     m\_100655   FI13 : subset inclusion logic

n\_6015\_0\_0      byte    %8.0g      m\_100267   Chest pain felt during physical activity

n\_6016\_0\_0      byte    %8.0g      m\_100267   Chest pain felt outside physical activity

n\_6145\_2\_0      byte    %54.0g     m\_100502   Illness, injury, bereavement, stress in last 2 years

n\_6148\_2\_0      byte    %44.0g     m\_100523   Eye problems/disorders

n\_6149\_2\_0      byte    %20.0g     m\_100538   Mouth/teeth dental problems

n\_6150\_2\_0      byte    %20.0g     m\_100605   Vascular/heart problems diagnosed by doctor

n\_6156\_2\_0      byte    %48.0g     m\_100498   Manic/hyper symptoms

n\_6159\_2\_0      byte    %25.0g     m\_100553   Pain type(s) experienced in last month

n\_6348\_2\_0      int     %19.0g     m\_1990     Duration to complete numeric path (trail #1)

n\_6349\_2\_0      int     %8.0g                 Total errors traversing numeric path (trail #1)

n\_6350\_2\_0      int     %19.0g     m\_1990     Duration to complete alphanumeric path (trail #2)

n\_6351\_2\_0      int     %8.0g                 Total errors traversing alphanumeric path (trail #2)

n\_6373\_2\_0      byte    %8.0g                 Number of puzzles correctly solved

n\_20016\_2\_0     byte    %8.0g                 Fluid intelligence score

n\_20023\_2\_0     int     %12.0g                Mean time to correctly identify matches

n\_20116\_2\_0     byte    %20.0g     m\_0090     Smoking status

n\_20117\_2\_0     byte    %20.0g     m\_0090     Alcohol drinker status

n\_20122\_0\_0     byte    %27.0g     m\_100694   Bipolar disorder status

n\_20123\_0\_0     byte    %8.0g      m\_0007     Single episode of probable major depression

n\_20124\_0\_0     byte    %8.0g      m\_0007     Probable recurrent major depression (moderate)

n\_20125\_0\_0     byte    %8.0g      m\_0007     Probable recurrent major depression (severe)

n\_20126\_0\_0     byte    %46.0g     m\_100695   Bipolar and major depression status

n\_20127\_0\_0     byte    %8.0g                 Neuroticism score

n\_20128\_2\_0     byte    %8.0g                 Number of fluid intelligence questions attempted within time

&nbsp;                                               limit

n\_20165\_0\_0     byte    %8.0g      m\_5001     FI1 : numeric addition test

n\_20167\_0\_0     byte    %8.0g      m\_5002     FI2 : identify largest number

n\_20169\_0\_0     byte    %8.0g      m\_5003     FI3 : word interpolation

n\_20171\_0\_0     byte    %8.0g      m\_5004     FI4 : positional arithmetic

n\_20173\_0\_0     byte    %11.0g     m\_5005     FI5 : family relationship calculation

n\_20175\_0\_0     byte    %8.0g      m\_5006     FI6 : conditional arithmetic

n\_20177\_0\_0     byte    %8.0g      m\_5007     FI7 : synonym

n\_20179\_0\_0     byte    %8.0g      m\_5008     FI8 : chained arithmetic

n\_20181\_0\_0     byte    %8.0g      m\_5009     FI9 : concept interpolation

n\_20183\_0\_0     byte    %8.0g      m\_5010     FI10 : arithmetic sequence recognition

n\_20185\_0\_0     byte    %8.0g      m\_5011     FI11 : antonym

n\_20187\_0\_0     byte    %8.0g      m\_5012     FI12 : square sequence recognition

n\_20189\_0\_0     byte    %39.0g     m\_5013     FI13 : subset inclusion logic

n\_20191\_0\_0     byte    %8.0g                 Fluid intelligence score

n\_20193\_0\_0     byte    %8.0g      m\_5014     FI14 : alphanumeric substitution

n\_20195\_0\_0     int     %8.0g                 Number of symbol digit matches attempted

n\_20240\_0\_0     byte    %8.0g                 Maximum digits remembered correctly

n\_20247\_0\_0     int     %8.0g                 Total errors traversing numeric path (trail #1)

n\_20248\_0\_0     int     %8.0g                 Total errors traversing alphanumeric path (trail #2)

n\_20418\_0\_0     int     %20.0g     m\_0510     Impact on normal roles during worst period of anxiety

n\_20419\_0\_0     byte    %12.0g     m\_0520     Difficulty concentrating during worst period of anxiety

n\_20420\_0\_0     int     %39.0g     m\_0517     Longest period spent worried or anxious

n\_20421\_0\_0     int     %20.0g     m\_0502     Ever felt worried, tense, or anxious for most of a month or

&nbsp;                                               longer

n\_20422\_0\_0     byte    %12.0g     m\_0520     More irritable than usual during worst period of anxiety

n\_20423\_0\_0     byte    %12.0g     m\_0520     Keyed up or on edge during worst period of anxiety

n\_20425\_0\_0     int     %20.0g     m\_0502     Ever worried more than most people would in similar situation

n\_20426\_0\_0     byte    %12.0g     m\_0520     Restless during period of worst anxiety

n\_20427\_0\_0     byte    %12.0g     m\_0520     Frequent trouble falling or staying asleep during worst period

&nbsp;                                               of anxiety

n\_20437\_0\_0     int     %20.0g     m\_0502     Thoughts of death during worst depression

n\_20449\_0\_0     int     %20.0g     m\_0502     Feelings of tiredness during worst episode of depression

n\_20450\_0\_0     int     %20.0g     m\_0502     Feelings of worthlessness during worst period of depression

n\_20458\_0\_0     int     %20.0g     m\_0537     General happiness

n\_20459\_0\_0     int     %20.0g     m\_0537     General happiness with own health

n\_20532\_0\_0     int     %20.0g     m\_0502     Did your sleep change?

n\_20533\_0\_0     byte    %8.0g      m\_0508     Trouble falling asleep

n\_20534\_0\_0     byte    %8.0g      m\_0508     Sleeping too much

n\_20535\_0\_0     byte    %8.0g      m\_0508     Waking too early

n\_20536\_0\_0     int     %51.0g     m\_0507     Weight change during worst episode of depression

n\_20537\_0\_0     int     %20.0g     m\_0519     Frequency of difficulty controlling worry during worst period

&nbsp;                                               of anxiety

n\_20538\_0\_0     int     %20.0g     m\_0502     Worried most days during period of worst anxiety

n\_20539\_0\_0     int     %20.0g     m\_0519     Frequency of inability to stop worrying during worst period of

&nbsp;                                               anxiety

n\_20540\_0\_0     int     %20.0g     m\_0502     Multiple worries during worst period of anxiety

n\_20541\_0\_0     int     %20.0g     m\_0502     Difficulty stopping worrying during worst period of anxiety

n\_20542\_0\_0     int     %20.0g     m\_0502     Stronger worrying (than other people) during period of worst

&nbsp;                                               anxiety

n\_20543\_0\_0     int     %20.0g     m\_0518     Number of things worried about during worst period of anxiety

n\_21000\_0\_0     int     %26.0g     m\_1001     Ethnic background

n\_21001\_2\_0     double  %18.14f               Body mass index (BMI)

n\_21002\_2\_0     double  %18.14f               Weight

n\_21022\_0\_0     byte    %8.0g                 Age at recruitment

n\_21024\_0\_0     int     %20.0g     m\_0502     Ever diagnosed with IBS

n\_21048\_0\_0     int     %20.0g     m\_0339     Degree bothered by back pain in the past 3 months

n\_21064\_0\_0     int     %20.0g     m\_0502     Sensitive stomach

n\_21065\_0\_0     int     %20.0g     m\_0502     Family history of IBS

n\_22150\_0\_0     byte    %8.0g                 Age COPD (Chronic Obstructive Pulmonary Disease) diagnosed by

&nbsp;                                               doctor

n\_23104\_2\_0     double  %18.14f               Body mass index (BMI)

n\_23324\_2\_0     byte    %8.0g                 Number of symbol digit matches made correctly

n\_25003\_2\_0     double  %18.11f               Volume of ventricular cerebrospinal fluid (normalised for head

&nbsp;                                               size)

n\_25004\_2\_0     double  %18.12f               Volume of ventricular cerebrospinal fluid

n\_25005\_2\_0     long    %10.0g                Volume of grey matter (normalised for head size)

n\_25006\_2\_0     long    %10.0g                Volume of grey matter

n\_25007\_2\_0     long    %10.0g                Volume of white matter (normalised for head size)

n\_25008\_2\_0     long    %10.0g                Volume of white matter

n\_25009\_2\_0     long    %10.0g                Volume of brain, grey+white matter (normalised for head size)

n\_25781\_2\_0     long    %10.0g                Total volume of white matter hyperintensities (from T1 and

&nbsp;                                               T2\_FLAIR images)

n\_26410\_0\_0     double  %18.15f               Index of Multiple Deprivation (England)









