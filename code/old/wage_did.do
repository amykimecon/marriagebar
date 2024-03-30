/*******************************************************************************
Title:     	 	Preliminary DID
Author: 	 	Carolyn
Purpose: 	 	The purpose of this do file is to do a "quick and dirty" test of 
				whether there are differences in the evolution of salaries 
				between KY/NC and their neighbouring states.
Last update: 	202304
*******************************************************************************/


*****************
* ADOS 
*****************
capture program drop store_est_matrix_did
program define store_est_matrix_did, eclass
		syntax, name(string)

		local center 7
		local lb 0 
		local ub 9 
		local nr_rows = `ub'-`lb'+1 

		cap drop `name'  
		mat `name' = J( `nr_rows' , 5 , .)  // change number of rows to hold all possible estimates (e.g. 10)

		* fill matrix with values of t, point estimates, and CI  
		local n = 1
		local starting_point = 1 + `nr_rows' + 1
		foreach t of numlist `lb'/`ub' { 
			
			** for plot 
			* event time  
			mat `name'[`n',1]  = `t' - `center'
			* point estimate  
			cap mat `name'[`n',2] =  _b[1.treat#`t'.event_time]  
			* lower CI  
			cap mat `name'[`n',3] = _b[1.treat#`t'.event_time] + invnorm(0.025)*_se[1.treat#`t'.event_time]
			* upper CI  
			cap mat `name'[`n',4] = _b[1.treat#`t'.event_time] + invnorm(0.975)*_se[1.treat#`t'.event_time]

			local n = `n' + 1  
		}  

		* store coefficients to dataset  
		cap drop `name'?  
		svmat `name'  
		
end 


*****************
* RUN FROM HERE
******************
* if Carolyn's personal computer
*cd "/Users/carolyn/Library/CloudStorage/Dropbox/marriagebar_data/icpsr_southern_states_salaries_1910-1940/"
* if Carolyn's work computer
*cd "C:\Users\ctsao\Dropbox\marriagebar_data\icpsr_southern_states_salaries_1910-1940"
* if amy's computer
cd "/Users/amykim/Dropbox (Princeton)/marriagebar/icpsr_southern_states_salaries_1910-1940/"
set more off 

*--- DATA PREPARATION ---*

* prepare CPI adjustments
import excel using "additional_data/cpi_historical.xlsx", firstrow clear
global deflate_year 1930
destring year, replace
collapse (mean) cpi = cpi, by(year)
qui sum if year==$deflate_year
global cpi_target = r(mean)
replace year = year+1 // e.g. acadyr 2011 is year 2010/2011; since contract
// was signed in 2010, deflate using 2010 CPI
tempfile cpi
save `cpi', replace 

* prepare salaries data
use "segregated_school_resources_ICPSR.dta", clear 
/* States FIPS:
KY 21
NC 37

SC 45
TN 47
*/
* treatment indicator: lifts marriage ban
gen treat = 1 if inlist(fips_state, 21, 37)
replace treat = 0 if inlist(fips_state, 45, 47)
keep if !missing(treat)
* gen event time where year 0 is the first year of lifted ban
gen event_time = year-1933 if inlist(fips_state, 37, 45)
replace event_time = year-1938 if inlist(fips_state, 21, 47)
ta event_time,m 
keep if event_time>=-7 & event_time<=2
ta event_time fips_state
* adjust salaries
merge m:1 year using `cpi', keep(3) assert(3 2) nogen
local vars_to_keep avesalw avesalb expendall
foreach var in `vars_to_keep' {
	replace `var' = `var'*($cpi_target/cpi)
}
drop cpi

* chcke for missing data
ta event_time fips_state
ta event_time fips_state if !missing(avesalw)
xxx
* drop periods where missing data for at least one state
ta event_time fips_state if !missing(avesalw) // missing data in -2 for state 47, TN
drop if inlist(event_time, -6, -4, -3, -2)
ta event_time fips_state if !missing(avesalw)
replace event_time = -3 if event_time==-7
replace event_time = -2 if event_time==-5
ta event_time fips_state if !missing(avesalw)
* keep only counties with salary data (maybe should keep balanced panel?) +++
keep if !missing(avesalw)

*--- RAW DATA PLOTS ---*

* plot raw trends going into DiD
preserve
collapse avesalw, by(treat event_time) 
twoway (scatter avesalw event_time if treat==1) ///
	(scatter avesalw event_time if treat==0), ///
	legend(lab(1 "Treat") lab(2 "Control"))
restore

*--- ESTIMATION & PLOTS ---*

* estimate DiD 
gen post = (event_time>0) 
replace event_time = event_time+7 

sum avesalw treat event_time avesalb expendall 
foreach var in avesalb {
	gen miss_`var' = missing(`var')
	replace `var' = 0 if missing(`var')
}
reg avesalw treat##b6.event_time avesalb miss_avesalb expendall, robust cluster(fips_state)
store_est_matrix_did, name(est) 
graph twoway (connected est2 est1, m(o) msize(small) mlwidth(medium) mc("blue") lc("grey")) ///
             (rcap est3 est4 est1, color("blue") m(i)), ///
              xline(-0.5, lc(gray) lw(thin)) ///
              ylin(0, lw(thin) lc(gray)) ///
              xlab(, val) xtick(-7(1)3) ///
              graphregion(c(white) margin(1 10 1 8)) ///
              xsize(20) ysize(11) ///
			  legend(rows(1) region(style(none)) order(1 "Estimates")) ///
              ytitle("beta_hat on avg. white teacher salaries", size(medium)) xtit("Year relative to lifting marriage ban", size(medium))
graph export "figures/did_kytn_ncsc_all_counties_comparison.pdf", replace 

*--- REPEAT, BUT FOR BORDER COUNTIES ONLY ---*

* keep only border counties
gen border_ky = 1 if fips_state==21 & ///
	(inlist(countyname, "fulton", "hickman", "graves", "calloway") | ///
	inlist(countyname, "trigg", "christian", "todd", "logan") | ///
	inlist(countyname, "simpson", "allen", "monroe", "cumberland", "clinton") | ///
	inlist(countyname, "wayne", "mccreary", "whitley", "bell", "harlan", "letcher"))
gen border_tn = 1 if fips_state==47 & ///
	(inlist(countyname, "lake", "obion", "weakley", "henry", "stewart") | ///
	inlist(countyname, "montgomery", "robertson", "sumner", "macon") | ///
	inlist(countyname, "clay", "pickett", "scott", "campbell", "claiborne") | ///
	inlist(countyname, "hancock", "hawkins", "sullivan", "johnson"))
gen border_nc = 1 if fips_state==37 & ///
	(inlist(countyname, "polk", "rutherford", "cleveland", "gaston") | ///
	inlist(countyname, "mecklenburg", "union", "anson", "richmond", "scotland") | ///
	inlist(countyname, "robeson", "columbus", "brunswick", "new hanover") | ///
	inlist(countyname, "pender", "onslow", "carteret"))
gen border_sc = 1 if fips_state==45 & ///
	(inlist(countyname, "oconee", "pickens", "greenville", "spartanburg", "cherokee") | ///
	inlist(countyname, "york", "lancaster", "chesterfield", "marlboro") | ///
	inlist(countyname, "dillon", "marion", "horry"))
gen border_county = 1 if border_ky==1 | border_tn==1 | border_nc==1 | border_sc==1

preserve
keep if border_county==1
collapse avesalw, by(treat event_time) 
twoway (scatter avesalw event_time if treat==1) ///
	(scatter avesalw event_time if treat==0), ///
	legend(lab(1 "Treat") lab(2 "Control"))
restore

keep if border_county==1
sum avesalw treat event_time avesalb expendall 
reg avesalw treat##b6.event_time avesalb miss_avesalb expendall, robust cluster(fips_state)
store_est_matrix_did, name(est) 
graph twoway (connected est2 est1, m(o) msize(small) mlwidth(medium) mc("blue") lc("grey")) ///
             (rcap est3 est4 est1, color("blue") m(i)), ///
              xline(-0.5, lc(gray) lw(thin)) ///
              ylin(0, lw(thin) lc(gray)) ///
              xlab(, val) xtick(-7(1)3) ///
              graphregion(c(white) margin(1 10 1 8)) ///
              xsize(20) ysize(11) ///
			  legend(rows(1) region(style(none)) order(1 "Estimates")) ///
              ytitle("beta_hat on avg. white teacher salaries", size(medium)) xtit("Year relative to lifting marriage ban", size(medium))
graph export "figures/did_kytn_ncsc_border_counties_comparison.pdf", replace 



