********************************************************************************
* Selecting predictors in regression models using conceptual/theory-driven 
* and data-driven strategies in Stata
********************************************************************************

clear all
set more off
version 18.0


* ----- Change to your working folder -----
cd "/Users/alimirzazadeh1/Box Sync/Vietman CDC Work/SI Activities 2025-2026/Regression Models"

* ----- Load dataset (edit filename or comment out if already loaded) -----
clear
import excel "NHANES2015.xls", sheet("data") firstrow


* ----- Basic checks before cleaning -----
describe age education gender race poverty usborn maritalstatus hpv hdl bmi
summarize age bmi hdl poverty
tabulate education, missing
tabulate gender, missing
tabulate race, missing
tabulate usborn, missing
tabulate maritalstatus, missing
tabulate hpv, missing

* ----- Variable labels -----
label variable age           "Age in years"
label variable education     "Education level"
label variable gender        "Gender"
label variable race          "Race/Hispanic origin"
label variable poverty       "Ratio of family income to poverty"
label variable usborn        "Country of birth"
label variable maritalstatus "Marital status"
label variable hpv           "HPV test result"
label variable hdl           "HDL cholesterol (mg/dL)"
label variable bmi           "Body Mass Index (kg/m^2)"

* ----- Value labels -----
label define edu_lbl 0 "Less than 9th grade" ///
                    1 "9-11th grade" ///
                    2 "HS graduate / GED" ///
                    3 "Some college or above" ///
                    4 "Don't know"
label values education edu_lbl

label define gender_lbl 0 "Male" 1 "Female"
label values gender gender_lbl

label define race_lbl 0 "Mexican American" ///
                     1 "Other Hispanic" ///
                     2 "Non-Hispanic White" ///
                     3 "Non-Hispanic Black" ///
                     4 "Other race including multiracial"
label values race race_lbl

label define usborn_lbl 0 "Born in the US" 1 "Other" 2 "Don't know"
label values usborn usborn_lbl

label define marital_lbl 0 "Married" ///
                        1 "Widowed" ///
                        2 "Divorced" ///
                        3 "Separated" ///
                        4 "Never married" ///
                        5 "Living with partner" ///
                        6 "Refused" ///
                        7 "Don't know"
label values maritalstatus marital_lbl

label define hpv_lbl 0 "Negative" 1 "Positive"
label values hpv hpv_lbl

* ----- Recode special responses to missing (system missing) -----
* Convert "Don't know" and "Refused" codes to missing.
recode education (4 = .)
recode usborn (2 = .)
recode maritalstatus (6 7 = .)

* Ensure hpv only has 0/1, otherwise set missing
replace hpv = . if hpv != 0 & hpv != 1

* ----- Create grouped / recoded variables -----

* Age group: 1=<30, 2=30-49, 3=50+
gen byte age_grp = .
replace age_grp = 1 if age < 30
replace age_grp = 2 if age >= 30 & age < 50
replace age_grp = 3 if age >= 50
label define agegrp_lbl 1 "<30" 2 "30-49" 3 "50+"
label values age_grp agegrp_lbl
label variable age_grp "Age group"

* Education grouped: 0 = HS or less (0-2), 1 = Some college+ (3)
gen byte edu_grp = .
replace edu_grp = 0 if education <= 2
replace edu_grp = 1 if education == 3
label define edugrp_lbl 0 "HS or less" 1 "Some college+"
label values edu_grp edugrp_lbl
label variable edu_grp "Education (grouped)"

* Hispanic vs Non-Hispanic based on race coding
gen byte hispanic = .
replace hispanic = 1 if race == 0 | race == 1
replace hispanic = 0 if race == 2 | race == 3 | race == 4
label define hisp_lbl 0 "Non-Hispanic" 1 "Hispanic"
label values hispanic hisp_lbl
label variable hispanic "Hispanic ethnicity"

* BMI categories: 1=Underweight,2=Normal,3=Overweight,4=Obese
gen byte bmi_cat = .
replace bmi_cat = 1 if bmi < 18.5
replace bmi_cat = 2 if bmi >= 18.5 & bmi < 25
replace bmi_cat = 3 if bmi >= 25 & bmi < 30
replace bmi_cat = 4 if bmi >= 30
label define bmi_lbl 1 "Underweight" 2 "Normal" 3 "Overweight" 4 "Obese"
label values bmi_cat bmi_lbl
label variable bmi_cat "BMI category"

* Low HDL indicator: 1 if HDL < 40 mg/dL, 0 otherwise
gen byte hdl_low = .
replace hdl_low = 1 if hdl < 40
replace hdl_low = 0 if hdl >= 40
label define hdl_lbl 0 "Normal/High" 1 "Low HDL (<40 mg/dL)"
label values hdl_low hdl_lbl
label variable hdl_low "Low HDL (<40 mg/dL)"

* Married vs Not married flag
gen byte married_flag = .
replace married_flag = 1 if maritalstatus == 0
replace married_flag = 0 if maritalstatus == 1 | maritalstatus == 2 | maritalstatus == 3 | maritalstatus == 4 | maritalstatus == 5
label define married_lbl 0 "Not married" 1 "Married"
label values married_flag married_lbl
label variable married_flag "Married vs not married"

* ----- Basic post-cleaning checks -----
display "== Post-cleaning summaries =="
summarize age bmi hdl poverty
tabulate age_grp
tabulate edu_grp
tabulate hispanic
tabulate bmi_cat
tabulate hdl_low
tabulate married_flag
tabulate hpv, missing

* Codebook for new variables
codebook age_grp edu_grp hispanic bmi_cat hdl_low married_flag

* numlabel, add mask("#. ")
tab1 age_grp edu_grp hispanic bmi_cat hdl_low married_flag

* ----- Save cleaned dataset -----
save "NHANES2015_cln.dta", replace


ssc install tabout, replace

tabout usborn gender age_grp edu_grp hispanic married_flag hpv using HPV_table.xlsx, ///
c(freq row) ///
f(0 1) ///
clab(n %) ///
style(xlsx) ///
stats(chi2) ///
stpos(col) ///
npos(col) ///
ptotal(none) ///
sheet(Table1) ///
title("Table 1. Prevalence of HPV Infection by Participant Characteristics") ///
replace

tabout gender age_grp edu_grp hispanic married_flag usborn using HPV_table.xlsx, ///
c(freq row) ///
f(0 1) ///
clab(n %) ///
style(xlsx) ///
stats(chi2) ///
stpos(col) ///
npos(col) ///
ptotal(none) ///
sheet(Table2) ///
title("Table 2. Participant Characteristics by Nativity") ///
append



* --- total effect: adjust for confounder hispanic only ---
* Logistic (odds ratio)
logit hpv i.usborn i.hispanic, or
est store total_logit

* Average marginal effect (risk difference on probability scale)
margins usborn // Predicted Risk 1 & Risk 2
margins, dydx(usborn) post // Adj. Risk Difference (RD)


* --- direct effect (controlled direct effect) ---
* Logistic
logit hpv i.usborn i.hispanic i.edu_grp i.married_flag i.age_grp, or
est store direct_logit

* Marginal effect (controlled direct effect on probability scale)
margins usborn // Predicted Risk 1 & Risk 2
margins, dydx(usborn) post // Adj. Risk Difference (RD)


* Check collinearity among predictors
collin age_grp gender hispanic edu_grp married_flag

* Stepwise (both directions – recommended default)
stepwise, pr(0.10) pe(0.05) lockterm1: ///
    logit hpv (i.usborn) i.hispanic i.edu_grp i.married_flag i.age_grp, or
* Keeps the first variable listed after the colon in the model
* That variable will never be removed
* pr() = p-value to remove
* pe() = p-value to enter	

* Goodness-of-fit (Hosmer-Lemeshow)
estat gof, group(10)
	


********************************************************************************
* End of do file
********************************************************************************
