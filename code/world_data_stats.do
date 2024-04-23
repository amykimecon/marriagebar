* Processing World Data
* 2024/04 

global world_data "/Users/carolyn/Library/CloudStorage/Dropbox/marriagebar_data/world_dataset"

use "$world_data/workplace_discrimination/WORLD_Dataset_Workplace_Discrimination_and_Harassment_05.19.2023.dta", clear 

********************
* var lists
********************
* prohibitions of workplace discrimination 
local vars_prohibitions anyprotect_marital anyprotect_marital_2016 ///
	hir_marital promdemo_marital train_marital pay_marital ///
	term_marital indir_marital harass_marital

* employer responsibilities
local vars_emp_resp marital_empl_resp

* preventing retaliation
local vars_retaliation marital_rettype marital_retpart

sum `vars_prohibitions' `vars_emp_resp' `vars_retaliation'


*******************
* recode to yes/no
*******************
foreach var in anyprotect_marital anyprotect_marital_2016 {
	gen `var'_rc = 0 if `var'==1 // no 
	replace `var'_rc = 1 if `var'==5 // yes 
} 

foreach var in hir_marital promdemo_marital train_marital pay_marital ///
	term_marital indir_marital harass_marital ///
	marital_empl_resp marital_rettype marital_retpart {
	gen `var'_rc = 0 if `var'==1 // no 
	replace `var'_rc = 1 if inlist(`var', 2, 3, 4, 5) // yes 
} 

sum *_rc 


*******************
* summary stats
*******************
ta wb_econ if anyprotect_marital_rc==0
ta country if promdemo_marital_rc==0

ta wb_econ if promdemo_marital_rc==0
ta country if promdemo_marital_rc==0
