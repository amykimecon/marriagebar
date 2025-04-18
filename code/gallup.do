global dbox "~/Dropbox (Princeton)/marriagebar"
global data "$dbox/gallup"

// 1938-0131 survey
use "$data/USAIPO1938-0131.dta", clear

// questions 4 and 5 -- creating new binary variables (denom includes 'no opinion' but not missings)
gen ShareSWBetterElem = Q4 == 2 // share responding "Unmarried women" when asked whether married wmen or unmarried women make better teachers in grade schools
replace ShareSWBetterElem = . if Q4 == .a //missing 
// gen ShareMWBetterElem = Q4 == 1 // share responding 'married women' 
// replace ShareMWBetterElem = . if Q4 == .a //missing

gen ShareForMB = Q5 == 1 // share responding that they support marriage bars
replace ShareForMB = . if Q5 == .a //missing 

// generating state indicators
gen treatsouth = .
replace treatsouth = 0 if region == 5
replace treatsouth = 1 if inlist(state, 51, 58)

gen treatcomp = .
replace treatcomp = 0 if inlist(state, 52, 83, 26, 53) //sc, tn, wv, va
replace treatcomp = 1 if inlist(state, 51, 58)

gen kycomp = .
replace kycomp = 0 if inlist(state, 83, 26)
replace kycomp = 1 if inlist(state, 58)

// t-tests (unweighted)
ttest ShareSWBetterElem if treatcomp != ., by(treatcomp)
ttest ShareForMB if treatcomp != ., by(treatcomp)

// regressions (weighted): treated/control states
reg ShareSWBetterElem treatcomp i.female i.black i.class i.AGE_3WAY i.OCCUPATION1 [iweight = WtPubComp], r
reg ShareForMB treatcomp i.female i.black i.class i.AGE_3WAY i.OCCUPATION1 [iweight = WtPubComp], r

// regressions: ky only
reg ShareSWBetterElem kycomp i.female i.black i.class i.AGE_3WAY i.OCCUPATION1 [iweight = WtPubComp], r
reg ShareForMB kycomp i.female i.black i.class i.AGE_3WAY i.OCCUPATION1 [iweight = WtPubComp], r

// TAKEAWAYS: 
// 1) in treated states rel. to control, ppl less likely to think SW make better elem school teachers than MW (but not significant), diff is even more stark when only looking at KY (but still not significant)
// 2) in treated states rel. to control, ppl less likely to support MBs (but not significant), effect basically disappears when only looking at KY


// 1937-0082 survey
use "$data/USAIPO1937-0082.dta", clear

// question 2 
gen ShareSWBetter = Q2 == 2
replace ShareSWBetter = . if Q2 == .a

// generating state indicators
gen treatsouth = .
replace treatsouth = 0 if region == 5
replace treatsouth = 1 if inlist(state, 51, 58)

gen treatcomp = .
replace treatcomp = 0 if inlist(state, 52, 83, 26, 53) //sc, tn, wv, va
replace treatcomp = 1 if inlist(state, 51, 58)

gen kycomp = .
replace kycomp = 0 if inlist(state, 83, 26)
replace kycomp = 1 if inlist(state, 58)

// t-tests (unweighted)
ttest ShareSWBetter if treatcomp != ., by(treatcomp)

// regressions (weighted): treated/control states
reg ShareSWBetter treatcomp i.female i.black i.class i.AGE_3WAY i.OCCUPATION1 [iweight = WtPubComp], r

// regressions: ky only
reg ShareSWBetter kycomp i.female i.black i.class i.AGE_3WAY i.OCCUPATION1 [iweight = WtPubComp], r

// TAKEAWAYS:
// 1) in treated states rel. to control, ppl actually more likely to think SW make better teachers than MW (raw mean ttest not signif, but regression w controls significant)
// 2) same direction with only KY, but not significant 
// TLDR; results reverse direction between 1937 and 1938, so no clear diff





