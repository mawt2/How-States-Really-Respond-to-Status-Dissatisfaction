* Open log *
************
capture log close
log using "Status Conflict among Small States\Data Analysis\Replication Files\Stata Log Files\01-Data Management\02-Controls & Components\g-Small State Indicator\scss-0102g-small", replace

**************************************************
* Generate monadic small state indicator dataset *
**************************************************

* Programme: scss-0102g-small.do
* Project: Status Conflict among Small States
* Author: Matthew Tibbles

* Description *
***************
* This do-file generates a monadic small state indicator dataset using annual ranks of material capabilities
* A broad distinction is drawn between small states and major powers where a state is or aspires to be a major power in year t if its material capabilities rank is between 1 and 25 in year t and it has recieved a top-25 rank for 5 consecutive years or more (as part of either the runninng or a previous spelll)
* In addition, for every year that constitutes the beginning of either a 5 year spell or a spell of at least 1 year posterior to a previous 5 year spell, I compute a 5 year 'rising power' spell, which is designed to account for the state's perception of self as a major power and, thus, the different expectations and behavioural strategies associated with major powers as compared to smaller states
* Following the same logic, I impute a 5 year 'fading power' spell

* Set up Stata *
****************
version 16
clear all
macro drop all
set linesize 90

* Generate base indicators
********************************

/// Load monadic system membership dataset
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\01-Data Management\02-Controls & Components\x-Embedded\scss-0102x-emb-symemmon.dta", clear

/// Merge with material capabilities dataset
rename ccode ccode1
merge 1:1 ccode year using "Status Conflict among Small States\Data Analysis\Datasets\Derived\01-Data Management\01-Status Deficit & MID\b-Expected Status\scss-0101b-expsts-a-pw.dta", keep (match master) keepusing(expgblsts) nogen

/// Drop redundant year
drop if year < 1949 | year > 2000

/// Declare time series format
tsset ccode year

/// Generate spell information if rank is between 1 and 20 on material capabilities
tsspell, cond(expgblstspw <= 20)

foreach var of varlist _seq _spell _end {
	rename `var' `var'_maj
}
	
	
/// Calculate spell length
egen splength_maj = max(_seq_maj), by(ccode _spell_maj)

/// Generate major power qualification indicator for spells of at least 5 years 
gen maj_base = 1 if splength_maj >= 5 & splength_maj != .

/// Generate past major power indicator (carrying forward major power qualification indicator to all years after initial qualification spell)
by ccode: carryforward maj_base, gen(maj_past)

/// Generate revised base major power indicator including years in which state is top 25 but year is not part of 5 year spell
gen maj_2 = 1 if maj_past == 1 & splength_maj >= 1 & splength_maj != .

/// Check major powers spells lasting from between 1 and 4 years but without prior 5 year spelll
*| That is, states not designated major powers status despite being ranked in top 25 on material capabilities (accounting, in theory, for sudden militarization).
tab cabb if splength_maj >= 1 & splength_maj <= 4 & maj_2 != 1


* Generate revised major power indicator accounting for rising and fading power spells
**************************************************************************************

/// Generate 1/5 year lags/leads of major power indicator 
forvalues i = 1/5 {
	by ccode: gen maj_2_lg`i' = maj_2[_n - `i'] 
	by ccode: gen maj_2_ld`i' = maj_2[_n + `i'] 
}


/// Generate fully adjusted major power indicator
gen major = maj_2
*| Recode rising/fading power spells as major power spells
foreach var of varlist maj_2_lg1-maj_2_ld5 {
	replace major = 1 if inlist(`var', 1)
}

replace major = 0 if major != 1

*************************************************************************************************************************
/// Generate spell information if rank is between 21 and 40 on material capabilities
tsspell, cond(expgblstspw >= 21 & expgblstspw <= 40)

foreach var of varlist _seq _spell _end {
	rename `var' `var'_mid
}
	
	
/// Calculate spell length
egen splength_mid = max(_seq_mid), by(ccode _spell_mid)

/// Generate middle power qualification indicator for spells of at least 5 years (or where state does does not qualify for major power spell but is <= 20 on global power rank)
gen mid_base = 1 if splength_mid >= 5 & splength_mid != . | expgblstspw <= 20 & major != 1

/// Generate past middle power indicator (carrying forward mmiddle power qualification indicator to all years after initial qualification spell)
by ccode: carryforward mid_base, gen(mid_past)

/// Generate revised middle power indicator including years in which state is 21-40 but year is not part of 5 year spell
gen mid_2 = 1 if mid_past == 1 & splength_mid >= 1 & splength_mid != .

/// Check major powers spells lasting from between 1 and 4 years but without prior 5 year spelll
*| That is, states not designated major powers status despite being ranked in top 25 on material capabilities (accounting, in theory, for sudden militarization).
tab cabb if splength_mid >= 1 & splength_mid <= 4 & mid_2 != 1


* Generate revised major power indicator accounting for rising and fading power spells
**************************************************************************************

/// Generate 1/5 year lags/leads of major power indicator 
forvalues i = 1/5 {
	by ccode: gen mid_2_lg`i' = mid_2[_n - `i'] 
	by ccode: gen mid_2_ld`i' = mid_2[_n + `i'] 
}

/// Generate fully adjusted major power indicator
gen middle = mid_2 
*| Recode rising/fading power spells as major power spells
foreach var of varlist mid_2_lg1-mid_2_ld5 {
	replace middle = 1 if inlist(`var', 1)
}


replace middle = 0 if major == 1

replace middle = 0 if middle != 1




gen world = 1 if expgblstspw <= 5
replace major = 0 if world == 1
* Generate small state indicator, label variables and save 
***********************************************************

/// Generate small state indicator
gen small = 0 if (major == 1 | middle == 1)
replace small = 1 if small == .




gen pclass = 1 if small == 1
replace pclass = 2 if middle == 1
replace pclass = 3 if major == 1
replace pclass = 4 if world == 1


local pclass maj mid
local pclassfull major-power middle-power
local lv label var

foreach pc in `pclass' {
	foreach pcf in `pclassfull' {
		`lv' _seq_`pc' "Sequence no. of `pcf' spells"
	    `lv' _spell_`pc' "Count of `pcf' spells"
	    `lv' _end_`pc' "End of `pcf' spell"
	    `lv' splength_`pc' "Length of `pcf' spell"
	    `lv' `pc'_base "Base `pcf' indiciator"
	    `lv' `pc'_past "Past `pcf' indicator"
		`lv' `pc'_2 "Revised `pcf' indicator"
		forvalues i = 1/5 {
			`lv' `pc'_2_lg`i' "`i' year lag of `pc'_2"
			`lv' `pc'_2_ld`i' "`i' year lead of `pc'_2"
		}
	}
}

`lv' major "Major power"
`lv' middle "Middle power"
`lv' world "Super power"
`lv' small "Small state"
`lv' pclass "Power classification"
label define pclass2 1 "Small state" 2 "Middle power" 3 "Major power" 4 "Super power"
label values pclass pclass2



/// Save
compress
save "Status Conflict among Small States\Data Analysis\Datasets\Derived\01-Data Management\02-Controls & Components\g-Small State Indicator\scss-0102g-small.dta", replace


* Close Log *
*************
log close
exit



* Add power rank from 1945, to make spells mor waccurate for 1950 onwards (note, particular, that Turkey should not be classified as small state)
*sorted that, already
* Change labelling, 15 mins
* Chnage descriptions/file-name, 15 mins
*check pclass seperation on graph, 20 mins
*table, by tonight.

















