
/// Load survey data
import delimited "C:\Users\matti\Documents\Personal\cc_survey.csv", clear 


/// Drop redundant variables
drop x* hr


/// Recode gender string variable
*| Review string values
tab gender
*| Remove empty strings values (spaces)
replace gender = trim(gender)
*| Recode incorrectly coding ob
replace gender = "f" if gender == "ft"
*| Recode variable
replace gender = "1" if gender == "m"
replace gender = "2" if gender == "f"
*| Destring
destring gender, gen(gender2)
drop gender
rename gender2 gender
*| Define variable levels
label def genlab 1 "Male" 2 "Female"
label values gender genlab

/// Recode payment method string variable
*| Review string values
tab pay_type
*| Recode variable
replace pay_type = "1" if pay_type == "cd"
replace pay_type = "2" if pay_type == "cs"
replace pay_type = "3" if pay_type == "cdcs"
replace pay_type = "4" if pay_type == "lt"
replace pay_type = "5" if pay_type == ""
*| Destring
destring pay_type, gen(pay_type2)
drop pay_type
rename pay_type2 pay_type
*| Define variable levels
label def paylab 1 "Card" 2 "Cash" 3 "Card/Cash" 4 "Lotto" 5 "No payment"
label values pay_type paylab

/// Recode missing values
foreach v of varlist _all {
	replace `v' = 0 if `v' == .
}

*Descriptive/ Predictive
**********************

gen prof_spend = b_spend - (cig_spend + lot_spend + pp_spend)

fgen age2 = age^2


*------------ATM

* Average spend non-atm vs atm user 
///graph bar b_spend prof_spend, over(atm, relabel(1 " " 2 " ") gap(350))  bar(1, color(dimgrey)) bar(2, color(slateblue)) fysize(22.5) yscale(lstyle(none)) xalternate ylabel(0(5)10) name(bar, replace)

* Accounting for controls
clonevar b_spendC = b_spend
clonevar prof_spendC = prof_spend

*** GSEM for efficient estimation
gsem(b_spendC <- i.atm  age age2 i.gender i.pay_type i.neighbourhood i.occupation, regress) /*
*/ (prof_spendC <- i.atm age age2 i.gender i.pay_type i.neighbourhood i.occupation, regress) 
*** Declare age-squared as nonlinear polynomial
f_able, nlvar(age2) 

*** Calculate marginal effect of ATM use on basket spend 
*| holding convariates at the median
margins, dydx(atm) at(gender = `med2' pay_type = `med3' neighbourhood = `med4' occupation = `med5') atmeans numerical nochainrule
scalar eff1 = r(b)[1,3]
scalar eff2 = r(b)[1,4]
scalar p1 = r(table)[4,3]
scalar p2 = r(table)[4,4]

*** Set envionment for margins plot
frame copy default atmmar
frame change atmmar
use ccatmmar, clear
keep _margin _ci_lb _ci_ub _at4
*| Increment X-axis age values to prevent overlap of CI lines in plot
forvalues i = 1/4 {
	if `i' < 3 {
		replace _at4 = _at4 -.01 if _n == `i'
	}
	else {
		replace _at4 = _at4  +.01 if _n == `i'
	}
}
*| Variable to denote basket type
gen m = 1 if _n < 3
replace m = 2 if m == .
*| Macros for marginal effect + p-value
local eff1m: di %3.2f eff1
local eff2m: di %3.2f eff2
local p1m: di %4.3f p1
local p2m: di %4.3f p2

*** Marginal effect plot with in-text signposting
twoway (connected _margin _at4 if m == 1, lcolor(gs2) lwidth(medthick) mfcolor(gs2) mlcolor(white) msize(vlarge) mlwidth(medium)) /* (rcap _ci_lb _ci_ub _at4 if m == 1, /*
*/ color(gs2) lwidth(medthick)) (connected _margin _at4 if m == 2, lcolor(plg2) lwidth(medthick) mfcolor(plg2) mlcolor(white) msize(vlarge) mlwidth(medium)) /*
*/ (rcap _ci_lb _ci_ub _at4 if m == 2, color(plg2) lwidth(medthick)),  xlabel(0 "No ATM use" 1"ATM use") xtitle("") ytitle("Average £ spent (adjusted prediction)") ?*
*/ text(6 .75 "{bf:Average effect (additional £ spent) = `eff2m'p}", size(small)) text(5.2 .5775 "{it:p} = `p2m'", size(small)) /*
*/ text(14 .23 "{bf:Average effect (additional £ spent) = `eff1m'p}", size(small)) text(13.225 .0575 "{it:p} = `p1m'", size(small)) legend(label(1 "Total basket") /*
*/ label(3 "Profit items") order(1 3) pos(6) rows(1) bmargin(t=-2)) graphregion(margin(t=2 b=1 l=2 r=3))  name(pred, replace)


frame change default
		


*** GSEM for simultaneous estimation of multiple models
gsem(b_spendC <- age age2 i.gender c.age#i.gender c.age2#i.gender i.pay_type i.atm  i.neighbourhood i.occupation , regress) /*
*/ (prof_spendC <- age age2 i.gender c.age#i.gender c.age2#i.gender i.pay_type i.atm i.neighbourhood i.occupation , regress) 

*** Declare age-squared as nonlinear polynomial
f_able, nlvar(age2) 

*** Marginal predictions across ages, holding covariates at mean/median
*| Computes margins for both basket and profit models under GSEM
margins, at(age=(10(10)80) gender=(1 2) atm = `med1' pay_type = `med3' neighbourhood=`med4' occupation = `med5' ) nochainrule numerical saving(cc_agemar)

*** Set envionment for margins plot
use ccagemar,clear
gen m = 1 if _n < 17
replace m = 2 if m == .
keep _margin _ci_lb _ci_ub _pvalue _at1 _at5 m
|* Increment X-axis age values to prevent overlap of CI lines in plot
replace _at1 = _at1 - 4 if m == 1 & _at5 == 1
replace _at1 = _at1 - 2 if m == 1 & _at5 == 2
replace _at1 = _at1 + 2 if m == 2 & _at5 == 2

/// Plot (because it allows greater flexibility, the manual method is preferred to Stata's in-built marginsplot command)
twoway (rspike _ci_lb _ci_ub _at1 if m == 1 & _at5 == 1, lcolor(gs2) lwidth(thin))/*
*/ (rspike _ci_lb _ci_ub _at1 if m == 1 & _at5 == 2, lcolor(gs2) lwidth(thin)) /*
*/ (rspike _ci_lb _ci_ub _at1 if m == 2 & _at5 == 1, lcolor(plg2) lwidth(thin)) /*
*/ (rspike _ci_lb _ci_ub _at1 if m == 2 & _at5 == 2, lcolor(plg2) lwidth(thin)) /*
*/ (scatter _margin _at1 if m ==1 & _at5 == 1, symbol(O) mlcolor(white) mfcolor(gs2) msize(large) mlwidth(medthin)) /*
*/ (scatter _margin _at1 if m ==1 & _at5 == 2, symbol(S) mlcolor(white) mfcolor(gs2) msize(large) mlwidth(medthin)) /*
*/ (scatter _margin _at1 if m == 2 & _at5 == 1, symbol(O)  mlcolor(white) mfcolor(plg2) msize(large) mlwidth(medthin)) /*
*/ (scatter _margin _at1 if m == 2 & _at5 == 2, symbol(S)  mlcolor(white) mfcolor(plg2) msize(large) mlwidth(medthin)) /*
*/ ,xlabel(10(10)80) xtitle(Age) ytitle("Average £ spent (adjusted prediction)") /*
*/ legend(label(5 "Male") label(6 "Female") label(7 "Male") label(8 "Female") /*
*/ order(0 "{bf:Total basket}" 5 6 0 "{bf:Profit items}" 7 8) pos(6) bmargin(t=-1) rows(1)) graphregion(margin(t=2 b=1 l=2 r=3)) name(ccage, replace) 






* Plot of basket versus profit spend by age and gender


* 

