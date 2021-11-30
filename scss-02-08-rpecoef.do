
frames reset
frame change default



frames reset
set varabbrev off
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear

timer clear 1
timer on 1


merge 1:1 ddyadid year using "Status Conflict among Small States\Data Analysis\Datasets\Derived\01-Data Management\01-Status Deficit & MID\d-Militarized Interstate Disputes\scss-0101d-embmid.dta", nogen


/// Generate status gains variables
qui bysort ccode1 year: egen tag = max(midint==1)

qui bysort ddyadid (year): gen attgblsts_1_ld5 = attgblsts_1_lg1[_n+6] if tag == 1 

qui gen gblstschange_5y = (attgblsts_1_ld5  - attgblsts_1_lg1)*-1 if tag == 1

qui bysort ccode1 (year): carryforward gblstschange_5y, gen(gblstschange_5yf)

qui gen stsg = 1 if gblstschange_5yf > 0 & gblstschange_5yf != .

qui replace stsg = 0 if gblstschange_5yf <= 0 & gblstschange_5yf != .

replace stsg = 0 if stsg == .


merge m:1 ccode1 year using "Status Conflict among Small States\Data Analysis\Datasets\Derived\01-Data Management\02-Controls & Components\b-Trade Dependence\scss-0102b-tdep-c-tdep2000.dta", keepusing(tropen2000) keep(match master) nogen

gen tropen2000_ln = ln(tropen2000)
///gen demind_1_lg1_fp1 = demind_1_lg1^3
gen kfp_lg1_fp1 = kfp_lg1^2
gen jnrthreat_2_lg1_ln = ln(jnrthreat_2_lg1) 

qui mark nonmiss1
qui markout nonmiss1 comstsdefpw_1_lg1 mcap_1_lg1
local in1 nonmiss1 == 1

qui mark nonmiss2
qui markout nonmiss2 comstsdefpw_1_lg1 mcap_1_lg1 demind_1_lg1 tropen2000 kfp_lg1 jnrthreat_2_lg1
local in2 nonmiss2 == 1


/// Log of CINC
gen mcap_1_lg1_ln = ln(mcap_1_lg1) if `in1'

/// Generate CINC(ln) splines (8 knots, orthogonalised)
splinegen mcap_1_lg1_ln if `in1', basis(mcaplnk) degree(3) df(8) orthog
local mcapk mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 

/// Generate peace years splines (4 knots)
qui sum pceyrs
local min = r(min)
splinegen pceyrs `min' 10 25 50 100 if `in1', basis(pceyrsk) degree(3) orthog
local pceyrsk pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4


/// Standardize IV and covariates on multivariate sample 
local j 0
local iv ""
local controls ""
foreach v of varlist comstsdefpw_1_lg1 demind_1_lg1 tropen2000_ln kfp_lg1_fp1 jnrthreat_2_lg1_ln {
	local ++ j
	
	if `j' == 1 {
		
		local abb_`j' = substr("`v'",7,3.)
		qui egen `abb_`j''z = std(`v') if `in2'
	    local iv "`iv' `abb_`j''z"
	}
	
	else {
		
		local abb_`j' = substr("`v'",1,3.)
		qui egen `abb_`j''z = std(`v') if `in2'
	    local controls "`controls' `abb_`j''z"
	}
}


//// Within between components
local W_iv ""
local B_iv ""
local W_mcapk ""
local B_mcapk ""
local W_controls ""
local B_controls ""

sort ddyadid year
foreach v in `iv' `mcapk' `controls' {
	
	di `v'
	
	qui by ddyadid: center `v', prefix(W_) mean(B_)
    
	if `: list v in local iv' {
		local W_iv "`W_iv' W_`v'"
		local B_iv "`B_iv' B_`v'"
	}
	
	else if `: list v in local mcapk' {
	    local W_mcapk "`W_mcapk' W_`v'"
		local B_mcapk "`B_mcapk' B_`v'"
	}
	
	else {
		local W_controls "`W_controls' W_`v'"
		local B_controls "`B_controls' B_`v'"
	}
}


	
/// Generate cross-level interaction
local 2wayi ""
foreach v in `iv' `controls' {
	
	forvalues k = 0/7 {
	    
		qui gen W_`v'XB_mcaplnk_`k' = W_`v'*B_mcaplnk_`k'
	    qui by ddyadid: center W_`v'XB_mcaplnk_`k', prefix(W_) mean(B_)
		local 2wayi "`2wayi' W_W_`v'XB_mcaplnk_`k' B_W_`v'XB_mcaplnk_`k'"
	}
}


/// Generate 3-way interaction
local 3wayi  " "

foreach v in `iv' `controls' {
    
	forvalues k = 1/4 {
	
	    forvalues k2 = 0/7 {
			
			qui gen W_W_`v'XB_mcaplnk_`k2'Xpyk_`k' = W_W_`v'XB_mcaplnk_`k2'*pceyrsk_`k'
		    qui gen B_W_`v'XB_mcaplnk_`k2'Xpyk_`k' = B_W_`v'XB_mcaplnk_`k2'*pceyrsk_`k'
		
		    local 3wayi "`3wayi' W_W_`v'XB_mcaplnk_`k2'Xpyk_`k' B_W_`v'XB_mcaplnk_`k2'Xpyk_`k'"
		}
	}
}
	

gen W_defzXstsg  = W_defz*stsg
local stsgi stsg W_defzXstsg

qui drop if `in1' != 1

	
// Estimate model (put vars into macros later)
logit midint `W_iv' `B_iv' `W_mcapk' `B_mcapk' `W_controls' `B_controls' `pceyrsk' `stsgi' `3wayi', cluster(ddyadid) difficult
estimates save prcoef, replace
keep `iv' `controls' `mcapk' pceyrs `W_iv' `B_iv' `W_mcapk' `B_mcapk' `W_controls' `B_controls' `pceyrsk' `stsgi' `3wayi' `2wayi' small_1 middle_1 major_1 world_1 midint ddyadid year
local paramno = e(k)
local tobs = _N
gen id = _n
qui est store pr
local W_iv ""
local B_iv ""
local W_mcapk ""
local B_mcapk ""
local W_controls ""
local B_control


/// Program to post results matrixes to e (in order to feed to nlcom for calculation of relative % change)
capture program drop epost_bv
program define epost_bv, eclass
args b V
ereturn post `b' `V'
end

	
/// Loop over expected status ranks
local sample small_1 middle_1 major_1 world_1
local s 0
foreach pc in `sample' {
	
	local subpop: subinstr local pc "_1" ""
	
	// Increment
	local ++ s
	
	/// New frame
	qui frame copy default rpccoef_`pc'
	qui frame change rpccoef_`pc'
		
	***************************************** Predictions at subpop mean  *******************************************************
	
	*| Between component is set at the subpopulation mean
    *| within component, and its cross-level interactions, at 0 - meaning zero deviation from the subpopulation (cluster) mean
	
	di _newline as result "*************************************************************************************"_newline"********************* Fixed covariate values for `subpop' powers ************************" _newline"*************************************************************************************"_newline
	
    sort ddyadid year
	local j 0
	est restore pr
	foreach var of varlist `iv' mcaplnk_0 `controls' {
        local ++ j
		
		di _newline as text "Variable =" as result " `var'" _newline as text "*-------------------------------------" 
		 
				   
	    /// Clone original variable
		qui clonevar clone_`var' = `var'
			   
		/// non-CINC loop
		if `j' != 2 {
		    qui su `var' if `pc' == 1, meanonly
            qui replace `var' = r(mean)  
	        qui clonevar clone_m_`var' = `var'
		
			        
		    /// Update within and between components on subpopulation mean 
			sort ddyadid year
		    qui by ddyadid: center `var', prefix(Wm_) mean(Bm_)
		    qui replace W_`var' = Wm_`var'
		    qui replace B_`var' = Bm_`var'
		    qui su B_`var', meanonly
	        di as text "Between component is fixed at" as result %7.6g r(mean)  as text " - the `subpop' power mean"
		    qui su W_`var', meanonly
	        di as text "Within component is fixed at" as result " `r(mean)'"  as text " - zero deviation from the mean" _newline
		
		    /// Update cross-level CINC(ln) interaction 
		    forvalues k = 0/7 {
		        qui gen Wm_`var'XB_mcaplnk_`k' = W_`var'*B_mcaplnk_`k'
		        qui by ddyadid: center Wm_`var'XB_mcaplnk_`k', prefix(Wm_) mean(Bm_)
		        qui replace W_W_`var'XB_mcaplnk_`k' = Wm_Wm_`var'XB_mcaplnk_`k'
		        qui replace B_W_`var'XB_mcaplnk_`k' = Bm_Wm_`var'XB_mcaplnk_`k'
		        qui sum B_W_`var'XB_mcaplnk_`k', meanonly
	            di as text "Between component of CINC(ln) cross-level interaction is fixed at" as result " `r(mean)'" as text " - the `subpop' power mean" 	
		        qui sum W_W_`var'XB_mcaplnk_`k', meanonly
	            di as text "Within component of CINC(ln) cross-level interaction is fixed at" as result " `r(mean)'" as text" - zero deviation from the mean" _newline
			
			    forvalues k2 = 1/4 {
				
				    qui replace W_W_`var'XB_mcaplnk_`k'Xpyk_`k2' = W_W_`var'XB_mcaplnk_`k'*pceyrsk_`k2'
				    qui replace B_W_`var'XB_mcaplnk_`k'Xpyk_`k2' = B_W_`var'XB_mcaplnk_`k'*pceyrsk_`k2'
			    }
		    }
	    }
		
		/// CINC loop
		else if `j' == 2 {
			
	        *| Set CINC(ln) smoothed at subpopulation mean
	        qui tempvar av`var'
	        qui tempvar abs`var'
	        qui egen `av`var'' = mean(`var') if `pc' == 1
	        qui gen `abs`var'' = abs(`av`var'' - mcaplnk_0)
	        qui sort `abs`var''
	        forvalues k = 0/7 {
		        qui local k`k'av = mcaplnk_`k'[1]	
		        replace mcaplnk_`k' = `k`k'av'
	        }
			
			*| Re-generate (and reset) within/between components of CINC(ln) smoothed at subpopulation mean
	        qui sort ddyadid year
	        forvalues k = 0/7 {
		        qui by ddyadid: center mcaplnk_`k', prefix(Wm_) mean(Bm_)
		        qui replace W_mcaplnk_`k' = Wm_mcaplnk_`k'
		        qui replace B_mcaplnk_`k' = Bm_mcaplnk_`k'
			}
		}
	}

	/// Loop over stsgains values
	forvalues i = 0/1 {
	    qui replace stsg = `i'
		di _newline as text "Stsgain dummy variable is fixed at " as result " `i'" _newline as text "*------------------------------------------"
	
	    /// Update within-deficitXstatus-gain interaction
	    qui replace W_defzXstsg = W_defz*stsg
		qui su W_defzXstsg, meanonly
		di as text "Within-deficitXstsgain interaction is fixed at" as result " `r(mean)'" as text " - zero deviation from the mean"
		
		/// Generate predictions
		if `i' == 0 {
	        qui predictnl double prmng = predict(pr), g(dmng_)
			local pred`i' prmng
		}
		
		else if `i' == 1 {
			qui predictnl double prmg = predict(pr), g(dmg_)
			local pred`i' prmg
		}
		
		di as text "||------ Predictions ------||" _newline
	    su `pred`i''
		
	}
	
	qui replace stsg = 0
	
	capture assert prmeannogain == prmeangain_
	
	if _rc == 9 {
		 di _newline as error "!-!!-!-!!-!- Predictions diverge -!-!!-!-!!-! " _newline
	}
	
	
	**************************** Predictions at +1sd ******************************
	
	di _newline as result "*************************************************************************************"_newline"********************* Covariate values at +1sd for `subpop' powers ************************" _newline"*************************************************************************************"_newline
	
	/// Tag 1st observation in sample
	qui sort ddyadid year
	qui tempvar obno
	qui gen `obno' = _n 
	su ddyadid if `obno' == 1
	local clust1 = r(mean)
	
	local j 0 
	
	/// Loop over covariates
	foreach var in `iv' `controls' {
		
		local ++ j
		
		di _newline as text "Variable =" as result " `var'" _newline as text "*-------------------------------------" 
		 
		sort ddyadid year
		
		/// Reset base variable to original values
	    qui replace `var' = clone_`var'
		
		/// Scalar for +1sd 
	    qui sum `var' if `pc' == 1, meanonly
		scalar plus1`j' = r(mean) + 1
			   
	    /// Re-set base variable to subpopulation mean
		qui replace `var' = clone_m_`var'
				 
		/// For first observation only, switch variable to +1sd
		qui replace `var' = plus1`j' if `obno' == 1
		local mplus1`j' = plus1`j'
		di as text "For 1st observation in sample, base variable is switched to" as result %7.6g plus1`j' as text " - 1sd increase from the `subpop' power mean" 

		tab `var' if ddyadid == `clust1'
			
	    /// Generate within/between components on 1 unit increase in base variable (between is at approximate subpop mean | within is 1 unit above subpop mean where obno = 1 )
	    qui by ddyadid: center `var', prefix(W2_) mean(B2_)
		su B2_`var' if `obno' == 1, meanonly
		di _newline as text "Between component is switched to" as result %7.6g r(mean) as text " - approximate to the `subpop' power mean"
		su W2_`var' if `obno' == 1, meanonly
		di as text "Within component is switched to" as result %7.6g r(mean) as text " - approximate to a 1sd increase from the `subpop' power mean" _newline
		
		
		/// Update cross-level interaction(using )
		forvalues k = 1/7 {
		    qui gen double W2_`var'XB_mcaplnk_`k' = W2_`var'*B_mcaplnk_`k'
		    qui by ddyadid: center W2_`var'XB_mcaplnk_`k', prefix(W2_) mean(B2_)
		    qui su B2_W2_`var'XB_mcaplnk_`k' if `obno' == 1, meanonly
		    qui replace B_W_`var'XB_mcaplnk_`k' = r(mean)
		    di as text "Between component of CINC(ln) cross-level interaction is switched to" as result %7.6g r(mean) as text " - approximate to the `subpop' power mean"
		    qui su W2_W2_`var'XB_mcaplnk_`k' if `obno' == 1, meanonly
		    qui replace W_W_`var'XB_mcaplnk_`k' = r(mean)
		    di as text "Within component of CINC(ln) cross-level interaction is switched to" as result %7.6g r(mean) as text " - approximate to a 1sd increase from the `subpop' power mean"
			
			forvalues k2 = 1/4 {
				 qui replace W_W_`var'XB_mcaplnk_`k'Xpyk_`k2' = W_W_`var'XB_mcaplnk_`k'*pceyrsk_`k2'
				 qui replace B_W_`var'XB_mcaplnk_`k'Xpyk_`k2' = B_W_`var'XB_mcaplnk_`k'*pceyrsk_`k2'
			}
		}
				
		
		
		/// Update within/between components 
		*| Cross-level interaction components need to be updated BEFORE their constituent within/between components 
		*| since the within component of the former can only be meaningfully updated when there is variation in the latter 
		qui replace W_`var' = W2_`var'[1]
		qui replace B_`var' = B2_`var'[1]
	
			
		/// Loop over status gains values
		forvalues i = 0/1 {
		
	        qui replace stsg = `i'
			
			
			/// Update within-deficitXstatus-gain interaction
	        qui replace W_defzXstsg = W_defz*stsg
		    qui su W_defzXstsg, meanonly
		    di as text "Within-deficitXstsgain interaction is fixed at" as result " `r(mean)'" as text " - zero deviation from the mean"
		        
			
				
			/// Generate predictions
		    if `i' == 0 {
				qui predictnl double prpng_`j' = predict(pr), g(dpng_`j'_) force
			    su prpng_`j'
			}
			
			else if `i' == 1 {
				qui predictnl double prpg_`j' = predict(pr), g(dpg_`j'_) force
				su prpg_`j'
			}
			  
		}
				
		/// Reset variable at means
		qui replace `var' = clone_`var'
		qui replace W_`var' = Wm_`var'
		qui replace B_`var' = Bm_`var' 
		replace stsg = 0 
		replace W_defzXstsg = W_defz*stsg
		
		forvalues k = 0/7 {
		    qui replace W_W_`var'XB_mcaplnk_`k' = Wm_Wm_`var'XB_mcaplnk_`k'
		    qui replace B_W_`var'XB_mcaplnk_`k' = Bm_Wm_`var'XB_mcaplnk_`k'
			
			forvalues k2 = 1/4 {
				qui replace W_W_`var'XB_mcaplnk_`k'Xpyk_`k2' = W_W_`var'XB_mcaplnk_`k'*pceyrsk_`k2'
		        qui replace B_W_`var'XB_mcaplnk_`k'Xpyk_`k2' = B_W_`var'XB_mcaplnk_`k'*pceyrsk_`k2'
			}
		}			
	}
	
	******************************* Set environment for nlcom estimation ***************************
	timer off 1 
	timer list 1
	save abookmark, replace
	
	///use abookmark, clear
		
	/// Drop redundant variables/observations (keeping only t+5/t+10)
	qui sort pceyrs
	drop if _est_pr != 1
	keep if pceyrs == 5 | pceyrs == 10
	qui by pceyrs:  gen dup = cond(_N==1,0,_n)
	keep if dup == 1
	drop dup
	su dmng_2
	su dmg_2
		
	estimates use prcoef
			    			
	/// Generate estimates matrix
	local colno = 4 + (4*5)
	mat b = J(1,`colno',.)
	
	mat b[1,1] = prmng[1]
	mat b[1,7] = prmng[2]
	mat b[1,13] = prmg[1]
	mat b[1,19] = prmg[2]
	
	local k 0
		
	forvalues j = 1/2 {
			
		local ++ k

	    forvalues i = 1/5 {
		
			if `k' == 1 {
				
				local ip1 = `i' + 1
				
				mat b[1,`ip1'] = prpng_`i'[`j']
				
				local ip2 = `i'+13
				
				mat b[1,`ip2'] = prpg_`i'[`j']
			}
			
			else {
				
				local ip3 = `i' + 7
				
				mat b[1,`ip3'] = prpng_`i'[`j']
				
				local ip4 = `i'+ 19
				
				mat b[1,`ip4'] = prpg_`i'[`j']
			}
		}
	}
								
	*** Generate Jacobian matrix for partial derivative of  logit predictions
	*| Enables creation of VCV matrix for calculation of RR confidence intervals
	local rowno = `colno'   // Number of matrix cols. = no. predictions
	mat J = J(`rowno',`paramno',.)   // Create matrix
	local cnames_j: colnames e(V)  // Macro for matrix col. names
	mat colnames J = `cnames_j'  // Name matrix cols.
	local levs ng g // Macro for prediction type
	local k 0   // Increment
	foreach l in `levs' {   // Loop over no-gains/gains predictions
	    local ++ k // Start increment
	    forvalues i = 1/`paramno' {   // Loop over model paramters
		capture confirm var dm`l'_`i'   // Mean prediction
	        if c(rc) == 111 { 
                    continue   // Skip the following if deriv. is missing
		}
		else {   // If deriv. is non-missing do the following
		    if `k' == 1 {   // No-gains loop
			mat J[1,`i'] = dm`l'_`i'[1]   // Deriv. for t=5
			mat J[7,`i'] = dm`l'_`i'[2]   // Deriv. for t=10
		    }
		    else if `k' == 2 {   // Gains loop
		        mat J[13,`i'] = dm`l'_`i'[1]   // Deriv. for t=5
			mat J[19,`i'] = dm`l'_`i'[2]   // Deriv. for t=10
		    }
		}
	    }
	    forvalues j = 1/5 { // Loop over main effects (status deficit + controls
                forvalues i = 1/`paramno' { // Loop over model paramters	
		    capture confirm var dp`l'_`j'_`i' // +1 S.D. prediction
		    if c(rc) == 111 { 
		        continue // Skip the following if deriv. is missing
		    }
		    else {  // If deriv. is non-missing do the following
		        if `k' == 1 { // No-gains loop
			    local jp1 = `j' + 1 // Macro for matrix row no.
			    mat J[`jp1',`i'] = dp`l'_`j'_`i'[1] // Deriv. for t=5
		            local jp2 = `j' + 7 // Macro for matrix row no.
		            mat J[`jp2',`i'] = dp`l'_`j'_`i'[2] // Deriv. for t=10
			}
			else if `k' == 2 { // Gains loop
			    local jp3 = `j' + 13 // Macro for matrix row no.
			    mat J[`jp3',`i'] = dp`l'_`j'_`i'[1] // Deriv. for t=5
			    local jp4 = `j' + 19 // Macro for matrix row no.
			    mat J[`jp4',`i'] = dp`l'_`j'_`i'[2] // Deriv. for t=10
		        }
		    }
		}
	    }
	}
	
	forvalues i = 1/`rowno' { // Loop over predictions 
            forvalues j = 1/`paramno' { // Loop over parameters
                if missing(J[`i', `j']) { // If deriv. is missing
                    matrix J[`i', `j'] = 0 // Deriv. = 0
                }
            }
        }			
	
	*** Generate variance-covariance matrix
	mat V = J*e(V)*J'
	
	/// Macros for results matrix row/column names (using abb macro, loop over t5/10)
	local levs NG G
	foreach i of numlist 5 10 {
	    foreach l in `levs' {
		    local names`l't`i' "_at`l'means#t`i'"
			forvalues j = 1/5 {
			    local names`l't`i' "`names`l't`i'' _at`l'v`j'#t`i'"
			}
		}
	}
			
	local namesall = "`namesNGt5' `namesNGt10' `namesGt5' `namesGt10'"
	di "`namesall'"
	
	mat colnames b = `namesall'
    mat colnames V = `namesall'
    mat rownames V = `namesall'
	
	
    /// Post results matrixes to e (for feeding to nlcom)
    epost_bv b V
	
    /// Matrix to store % change estimates 
	mat rpet5_`pc' = J(4,6,.)
	mat rpet10_`pc' = J(4,6,.)
		
	local k 0
	
	forvalues j = 1/5 {
		
		foreach i of numlist 5 10 {
			
		    if `j' == 1 {
								
				*| For status deficit only, estimate separate predictions for stsgains=0|stsgains=1
			    nlcom (rpe:(((_b[_atNGv1#t`i']/_b[_atNGmeans#t`i'])-1)*100))
						
				mat rpet`i'_`pc'[1,1] = r(b)[1,1]
				mat rpet`i'_`pc'[4, 1] = sqrt(r(V)[1,1])
				 
				  nlcom (rpe:(((_b[_atGv1#t`i']/_b[_atGmeans#t`i'])-1)*100))
						
				 mat rpet`i'_`pc'[1,2] = r(b)[1,1]
				 mat rpet`i'_`pc'[4, 2] = sqrt(r(V)[1,1])
			}
				 
		   else if `j' != 1 {
		   	
			local jp = `j' + 1
				
				*| For all control variables, estimates are averaged over stsgains=0|stsgains=1
				nlcom (rpe:((((_b[_atNGv`j'#t`i']/_b[_atNGmeans#t`i'])-1)*100)+(((_b[_atGv`j'#t`i']/_b[_atGmeans#t`i'])-1)*100))/2)
				
				mat rpet`i'_`pc'[1,`jp'] = r(b)[1,1]
				mat rpet`i'_`pc'[4,`jp'] = sqrt(r(V)[1,1])
			}
		}
	}
	
	/// Name matrix variable columns
	local vnames ""
	local vnames "`vnames' defng defg"
	forvalues j = 2/5 {
	    local vnames "`vnames' `abb`j''"
	}
	mat colnames rpet5_`pc' = `vnames'
	mat colnames rpet10_`pc' = `vnames'
	
	
	frame change default	
}
	
	
	
	
	
******************************* PLOT**********************************
	
local cnames defzng defzg demz troz kfpz jnrz
local sample small_1 middle_1 major_1 world_1
foreach pc in `sample' {
	
	foreach n of numlist 5 10 {
		
		mat colnames rpet`n'_`pc' = `cnames'
		
		xsvmat rpet`n'_`pc', saving(coefmat_t`n'_`pc', replace)
	}
	
}
		
		
coefplot matrix(rpet5_small_1) matrix(rpet10_small_1), se(4)|| matrix(rpet5_middle_1) matrix(rpet10_middle_1), se(4) || matrix(rpet5_major_1) matrix(rpet10_major_1), se(4) || matrix(rpet5_world_1) matrix(rpet10_world_1), se(4) || , mfcolor(white) mlcolor(gs0) mlwidth(medium) msize(medium) cismooth(color(gs1)  lwidth(1.5 18)) headings(defzng = "{bf:Within-country status deficit}" demz = "{bf:Within-country controls}",gap(-.35) offset(.35) labgap(-270) labsize(10.5pt)) coeflabels(defzng =  "{&minus}/0 change in status 5yr after MID" defzg = "+ change in status 5yr after MID" demz ="Democracy index" troz ="Trade openness(ln)" kfpz ="Foreign policy {it:{&kappa}}-similarity(FP1)" jnrz ="Joiner threat(ln)",  labsize(10.5pt) labgap(-265) tlcolor(none))  xline(0, lpattern(shortdash) lwidth(thin)) byopts(compact cols(4) xrescale graphregion(margin(r=-4.5 b=-5 t=.5 l=36)) note("{it:Note}. Coefficient plot of the discrete percentage effect of status deficit/control variables on the probability of initiating an MID across levels of expected status. Estimates are within-country effects drawn from a REWB model with status deficit," "CINC(ln) smoothed, peace years smoothed and four additional control variables. A three-way, cross-level interaction is also included between {it:a}) the within-effect of status deficit/each control, {it:b}) the between effect of CINC(ln) smoothed and {it:c}) the random" "effect of peace years smoothed. Estimates represent the effect of a 1 unit increase, switching a variable's within component - and its corresponding interaction terms - from 0 to 1, while holding it's between component - and the within and between" "components of covariates - at the subpopulation (expected status) mean. Estimates are computed at two levels, fixing the dummy variable - status gains - at {it:a}) 0 and {it:b}) 1. The estimated effect of status deficit is plotted at both levels, with an" "interaction term between status deficit and status gains. Plotted control estimates represent the mean effect across levels of status gains. Estimates are also computed at two seperate temporal intervals. White and black circles represent the estimated" "effect for a state that last initiated an MID 5 and 10 years ago respectively. Confidence intervals are plotted across a range of confidence levels in gradating shades of color from dark (low) to light (high).", size(9.5pt) pos(7) span just(left) ring(2))) legend(label(51 "5") label(102 "10") cols(1) pos(2) ring(0) colgap(1.5) holes(1) size(10pt) subtitle("Years since MID", linegap(5) size(10pt)) bmargin(small) region(lcolor(black) lwidth(vthin)))  xtitle("{bf: % effect on Pr(MID initiation)}", size(10.5pt)) yscale(lstyle(none) alt) xscale(lstyle(none)) xlabel(, tlcolor(black) labsize(10.5pt))  subtitle(, size(10.5pt) bfcolor(none)) plotregion(margin(t=-6 l=3 r=3)) xsize(15) ysize(6.5) name(newcoefwn, replace)




// note reposition
gr_edit .note.DragBy .5564323712530682 -35.3334555745699
gr_edit .note.DragBy 0 -1.251972835319411
gr_edit .note.DragBy .2782161856265349 0
gr_edit .note.DragBy .2782161856265349 0

// legend reposition
gr_edit .legend.DragBy 23.64837577825544 85.82969326578588
gr_edit .legend.DragBy .4173242784398004 0
gr_edit .legend.DragBy .1391080928132712 -.1391080928132941
gr_edit .legend.DragBy -.2782161856265423 .1391080928132679


gr_edit .plotregion1.plotregion1[1].plot102.style.editstyle marker(fillcolor(black)) editcopy
gr_edit .plotregion1.plotregion1[1].plot102.style.editstyle marker(linestyle(color(black))) editcopy
// plot102 edits

gr_edit .plotregion1.plotregion1[1].plot102.style.editstyle marker(symbol(circle)) editcopy
// plot102 symbol

gr_edit .plotregion1.subtitle[1].text = {}
gr_edit .plotregion1.subtitle[1].text.Arrpush Small
// subtitle[1] edits

gr_edit .plotregion1.subtitle[2].text = {}
gr_edit .plotregion1.subtitle[2].text.Arrpush Middle
// subtitle[2] edits

gr_edit .plotregion1.subtitle[3].text = {}
gr_edit .plotregion1.subtitle[3].text.Arrpush Major
// subtitle[3] edits

gr_edit .plotregion1.subtitle[4].text = {}
gr_edit .plotregion1.subtitle[4].text.Arrpush World
// subtitle[4] edits
















