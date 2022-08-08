/* 
Name: child low birth weight.do
Date Created: November 1 , 2021
Date Last Modified: November 26 , 2021
Created by: Ashmy Tom
Uses data:combined data of HR and KR data files of NFHS-4
Creates data: hrkr.dta
Description: This file is a part of the study "Exploring the impact of Household Air Pollution on low birth weight among Indian Children"
*/


clear all
cd "F:\MSE_PhD\Course_Work\Health & Environmental\Cooking fuel\Data hr and kr"
use "krhr.dta"

/*Generating Sample Weight*/
gen wt = v005/1000000 
gen child_agemonth= v008-b3
label var child_agemonth "child's age in months"

/*Classifying living child under 5*/
gen livingchild_60_months=0 
replace livingchild_60_months=1 if child_agemonth<=60 & b5==1
label var livingchild_60_months "living child of age 60 months"

/*Generating the outcome variable_low birth weight*/
gen low_birth_weight=. 
replace low_birth_weight=0 if m19>=2500 & m19!=9996 & m19!=9998 & m19!=.
replace low_birth_weight=1 if m19<2500 
label define low_birth_weight 0 "Normal Birth Weight"  1 "Low Birth Weight",modify
label values low_birth_weight low_birth_weight
label var low_birth_weight  "Birth Weight Classification"
tab low_birth_weight [aw=wt] if livingchild_60_months== 1 

/*Index Generation*/

/*Type of Cooking Fuel*/
gen cooking_fuel_type=. if v161==5|v161>11|v161==.
replace cooking_fuel_type=0 if v161<=4  
replace cooking_fuel_type=1 if v161>=6 & v161<=11
label define cooking_fuel_type 0 "Clean cooking fuel" 1 "Unclean cooking fuel"
label values cooking_fuel_type cooking_fuel_type
label var cooking_fuel_type "Cooking Fuel Type" 

/*Separate Kitchen*/
gen sep_kitchen=. if hv242==.
replace sep_kitchen=0 if hv242==1
replace sep_kitchen=1 if hv242==0
label define sep_kitchen 0 "Separate kitchen" 1 "No separate kitchen"
label values sep_kitchen sep_kitchen
label var sep_kitchen "Kitchen classification"

/*Environmental Tobacco Smoke*/
gen ETS=. if s715==.
replace ETS=0 if s715==0
replace ETS=1 if s715==1
label define ETS 0 "No" 1 "Yes"
label values ETS ETS
label var ETS "Exposure to ETS"

/*Descriptive Statistis*/

tab cooking_fuel_type low_birth_weight [aw=wt] if livingchild_60_months== 1, row nofreq  
tab sep_kitchen low_birth_weight [aw=wt] if livingchild_60_months== 1, row nofreq   
tab ETS low_birth_weight[aw=wt] if livingchild_60_months== 1,row nofreq

*Lpoly Graphs*/

twoway (lpolyci low_birth_weight v133 if  (livingchild_60_months==1 & sep_kitchen==1  ) [aw=wt]) (lpolyci low_birth_weight  v133 if  (livingchild_60_months==1 & sep_kitchen==0 ) [aw=wt])

twoway (lpolyci low_birth_weight v133 if  (livingchild_60_months==1 & ETS==1  ) [aw=wt]) (lpolyci low_birth_weight  v133 if  (livingchild_60_months==1 & ETS==0 ) [aw=wt])

twoway (lpolyci low_birth_weight v133 if  (livingchild_60_months==1 & cooking_fuel_type==1  ) [aw=wt]) (lpolyci  low_birth_weight v133 if  ( livingchild_60_months ==1 & cooking_fuel_type==0 ) [aw=wt])

**Principal Component Variables

*Creation of HAP Index
pca cooking_fuel_type sep_kitchen ETS [aw=wt]
predict HAP_index,score
label var HAP_index "HAP_index"

xtile HAP_index_5= HAP_index [aw=wt], nq(5)
label define HAP_index_5    1 "lowest" 2"low" 3 "medium" 4 "high" 5 "highest"  
label values HAP_index_5   HAP_index_5 
label var  HAP_index_5 "HAP_index_5"

tab HAP_index_5  low_birth_weight [aw=wt] if livingchild_60_months==1, row nofreq


/*Regression*/

/*Control Variables*/

/*Child level characteristics*/

**1. Birth order: 1, 2, 3, 4+
gen birth_order=. if bord==.
replace birth_order=1 if bord==1 
replace birth_order=2 if bord==2 
replace birth_order=3 if bord==3 
replace birth_order=4 if bord>=4
label define birth_order 1 "1" 2"2" 3 "3" 4 "4+"
label values birth_order birth_order
label var birth_order "Birth order"

**2. sex of child
ren b4 Sex_of_child

*3. No of siblings
gen d=1
sort caseid v001 v002
by caseid v001 v002: egen nb_sibs=sum(d)
replace nb_sibs=nb_sibs-1
label var nb_sibs "No. of siblings ever born"

/*Mother's Characteristics*/

**1. Mother's years of education
ren v133 mother_education
**2. Motherage_birth
ren v212 Motherage

/*Household characteristics*/

**1.Economic Status
ren v190 wealth_index
**2. Caste
ren s116 caste_tribe
**3. Religion
**Religion
gen religion=. if v130==.
replace religion=1 if v130==1
replace religion=2 if v130==2
replace religion=3 if v130==3
replace religion=4 if v130>=4
label define religion 1 "Hindu" 2 "Muslim" 3 "Christian" 4 "others" 
label values religion religion
label var religion "Religion"

**Regional level characteristics
ren v025 place_residence

**state level characteristics
ren v024 state


/*Logit Regression*/

svyset[pw=wt], psu(v021) strata(v022) singleunit(certainty)

svy,subpop(livingchild_60_months):logit low_birth_weight  i.HAP_index_5  i.birth_order i.Sex_of_child nb_sibs mother_education Motherage i.religion i.caste_tribe i.place_residence i.state,or

margins,dydx (HAP_index_5)

/*Hetrogenous Effect*/

/*1. By Mother's Education Level*/

gen mother_education_level=. if v106==.
replace mother_education_level=1 if v106==0|v106==1
replace mother_education_level=2 if v106==2|v106==3
label define mother_education_level  1 "Low Level Education"  2 "High Level Education" 
label values mother_education_level mother_education_level
label var mother_education_level "mother_education_level"

svy,subpop(livingchild_60_months):logit low_birth_weight  i.HAP_index_5  i.birth_order i.Sex_of_child nb_sibs Motherage i.religion i.caste_tribe i.place_residence i.state if mother_education_level==1

svy,subpop(livingchild_60_months):logit low_birth_weight  i.HAP_index_5  i.birth_order i.Sex_of_child nb_sibs Motherage i.religion i.caste_tribe i.place_residence i.state if mother_education_level==2


/*2. By Place of Residence*/

svy,subpop(livingchild_60_months):logit low_birth_weight  i.HAP_index_5  i.birth_order i.Sex_of_child nb_sibs Motherage mother_education i.religion i.caste_tribe i.state if place_residence==1

svy,subpop(livingchild_60_months):logit low_birth_weight  i.HAP_index_5  i.birth_order i.Sex_of_child nb_sibs Motherage mother_education i.religion i.caste_tribe i.state if place_residence==2











