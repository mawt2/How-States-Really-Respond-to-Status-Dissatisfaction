
frames reset
set varabbrev off
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear


* Set environment for estimation
**********************************

local sample small_1 middle_1 major_1 world_1
local s 0
gen pc = .
foreach pc in `sample' {
	local ++ s
	replace pc = `s' if `pc' == 1
}

/// Restrict sample to non-missing obs
mark nonmiss 
markout nonmiss mcap_1_lg1 comstsdefpw_1_lg1
local in nonmiss == 1

/// Status deficit standardized on sample
egen defz = std(comstsdefpw_1_lg1) if `in'

/// Log of CINC
gen mcap_1_lg1_ln = ln(mcap_1_lg1) if `in'

/// Generate CINC(ln) splines (8 knots, orthogonalised)
splinegen mcap_1_lg1_ln if `in', basis(mcaplnk) degree(3) df(8) orthog

/// Generate peace years splines (4 knots)
qui sum pceyrs
local min = r(min)
splinegen pceyrs `min' 10 25 50 100 if `in', basis(pceyrsk) degree(3) orthog


/// Generate within-between components of status deficit/CINC(ln) smoothed
qui sort ddyadid year
foreach v of varlist defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 {
    qui by ddyadid: center `v' if `in', prefix(W_) mean(B_) 
}


/// Generate cross-level interaction for within-deficitXbetween-CINC(ln) smoothed
forvalues k = 0/7 {
	qui gen W_defXB_mcaplnk_`k' = W_defz*B_mcaplnk_`k'
	qui by ddyadid: center W_defXB_mcaplnk_`k', prefix(W_) mean(B_)
}

local 3way  " "



/// Generate 3 way interaction (still try centered version, but centered peace years doesn't make much sense....)
forvalues k = 1/4 {
	
	forvalues k2 = 0/7 {
		
		qui gen W_W_defXB_mcaplnk_`k2'Xpyk_`k' = W_W_defXB_mcaplnk_`k2'*pceyrsk_`k'
		qui gen B_W_defXB_mcaplnk_`k2'Xpyk_`k' = B_W_defXB_mcaplnk_`k2'*pceyrsk_`k'
		
		local 3way "`3way' W_W_defXB_mcaplnk_`k2'Xpyk_`k' B_W_defXB_mcaplnk_`k2'Xpyk_`k'"
	}
}
		
keep if `in'


/// Estimate model
logit midint W_defz B_defz W_mcaplnk_0 W_mcaplnk_1 W_mcaplnk_2 W_mcaplnk_3 W_mcaplnk_4 W_mcaplnk_5 W_mcaplnk_6 W_mcaplnk_7 B_mcaplnk_0 B_mcaplnk_1 B_mcaplnk_2 B_mcaplnk_3 B_mcaplnk_4 B_mcaplnk_5 B_mcaplnk_6 B_mcaplnk_7 pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4 `3way', cluster(ddyadid)
local paramno = e(k)
qui est store pr
estimates save prs, replace


* Estimation of mean effects
*****************************************

/// Program to post results matrixes to e (in order to feed to nlcom for calculation of relative % change)
capture program drop epost_bv
program define epost_bv, eclass
args b V
ereturn post `b' `V'
end


*>~< Run the follopwing commands for each expected status class
local sample small_1 middle_1 major_1 world_1
local s 0
foreach pc in `sample' {
	
	*| Increment
	local ++ s

	*| New frame
	qui frame copy default rpe_bytime_`pc'
	qui frame change rpe_bytime_`pc'
	
	*| Re-generate (and reset) within/between components of status deficit to global mean
	qui replace defz = 0
	qui by ddyadid: center defz, prefix(Wm_) mean(Bm_)
	qui replace W_defz = Wm_defz
	qui replace B_defz = Bm_defz
	

	*| Set CINC(ln) smoothed at subpopulation mean
	qui tempvar avmcaplnk_0
	qui tempvar absmcaplnk_0
	qui egen `avmcaplnk_0' = mean(mcaplnk_0) if `pc' == 1
	qui gen `absmcaplnk_0' = abs(`avmcaplnk_0' - mcaplnk_0)
	qui sort `absmcaplnk_0'
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
	
	qui drop Wm_* Bm_*
	
	*| Change within status deficit value to 0/1 (for first observation only)
	*... First observation is always US-CAN, which means that variable values are updated on full 50 year sample
    qui tempvar obno
	qui sort ddyadid year
	qui gen `obno' = _n 
	
	*** Loop for discrete change in status deficit
	local f 0
	qui forvalues p = 0/1 {
        local ++ f
	    if `f' != 1 {
	    *| Re-generate within/between status deficit components at + 1. S.D. 
            *| for 1st ob. only so as to preserve within-country variation
	    *| NOTE. 1st ob only bc. if all obs = 1, then within effect (cluster deviation) = 0
	        qui replace defz = `p' if `obno' == 1
	        *| Re-generate within/between components at +1 S.D. (for 1st ob.)
	        by ddyadid: center defz , prefix(W2_) mean(B2_)
		*| Update within/between components at +1 S.D. values (for 1st ob.)
		replace W_defz = W2_defz[1] // within = +1 S.D. 
		replace B_defz = B2_defz[1] // between = mean (0)
	     }
	     *| Update within-deficitXbetween-CINC(ln) smoothed interaction 
		forvalues k = 0/7 {
		    *| For status deficit = 0, use base variable (W_defz where within effect = 0)
		        if `f' == 1 {
			    gen double W2_defXB_mcaplnk_`k' = W_defz*B_mcaplnk_`k'
			}
			*| For status deficit = 1, use re-generated variable (W2_defz where within effect for 1st ob. = 1)
			else {
			    gen double W2_defXB_mcaplnk_`k' = W2_defz*B_mcaplnk_`k'
			}
			*| Update within and between components of interaction
			*| NOTE. Standard multiplicativive interaction Xit*Xi(hat) is not valid!!!!!!!!!!!!!!
			*| Correct specification = XitXi(hat)it*XitXi(hat)(HAT)
			by ddyadid: center W2_defXB_mcaplnk_`k', prefix(W2_) mean(B2_)
		        su W2_W2_defXB_mcaplnk_`k' if `obno' == 1, meanonly
		        replace W_W_defXB_mcaplnk_`k' = r(mean)
		        su B2_W2_defXB_mcaplnk_`k' if `obno' == 1, meanonly
	                replace B_W_defXB_mcaplnk_`k' = r(mean)
			*| Update 3-way interaction (within-deficitXbetween-CINC(ln) smoothedXpeace years
			forvalues k2 = 1/4 {
		            replace W_W_defXB_mcaplnk_`k'Xpyk_`k2' = W_W_defXB_mcaplnk_`k'*pceyrsk_`k2'
		            replace B_W_defXB_mcaplnk_`k'Xpyk_`k2' = B_W_defXB_mcaplnk_`k'*pceyrsk_`k2'
			}
		    }
	        *| Generate predictions with partial derivatives
		if `f' == 1 {
            qui predictnl double prm = predict(pr), g(dm_)
		}
		
		else {
		    qui predictnl double prp = predict(pr), g(dp_)
		}
		
		qui drop W2_* B2_*
	}
	
    
	
	/// Generate partial derivatives if missing
	*| Program checks if derivative variable exists, and generates derivatives as zero if missing
    local paramno = e(k)
	local levs m p
	
	foreach l in `levs' {
    
	    forvalues d = 1/`paramno' {
	
			capture confirm var d`l'_`d'
            
			if c(rc) == 111 { 
                
				qui gen d`l'_`d' = 0
		    }
				
		    else {
				   
		        continue
			}
	    }
	}
	
	/// Drop redundant observations/variables
	qui sort pceyrs
	
	qui duplicates drop pceyrs, force
	qui drop if pceyrs > 50
	
	qui est restore pr
		
	/// Generate estimates matrix
	local eno = _N
	local colno_b = _N*2
	mat b = J(1,`colno_b',.)
	local f 0
	local levs m p
	foreach l in `levs' {
	    
		local ++ f
		
		forvalues j =1/`eno' {
			    
			if `f' == 2 {
		        local jp = `j'+`eno'
			    mat b[1,`jp'] = pr`l'[`j']
			}
				
			else {
				mat b[1,`j'] = pr`l'[`j']
			}
		}
	}
	
	local paramno = e(k)
	
	/// Generate Jacobian matrix
	local rowno_j = _N*2
	
	mat J = J(`rowno_j',`paramno',.)
	
	local cnames_j: colnames e(V)
	
	mat colnames J = `paramno'
	
	local f 0
	local levs m p
	foreach l in `levs' {
	    
		local ++ f

		forvalues i = 1/`paramno' {
	
	        forvalues j =1/`eno' {
			    
				if `f' == 2 {
				    local jp = `j'+`eno'
			        mat J[`jp',`i'] = d`l'_`i'[`j']
				}
				
				else {
				    mat J[`j',`i'] = d`l'_`i'[`j']
				}
			}
		}
	}
	
	/// Generate variance-covariance matrix
	mat V = J*e(V)*J'
	
	/// Macro for matrix row/column names
	local names_m ""
    forvalues i = 0/50 {
        local names_m "`names_m' _atmpy`i'"
    }
	
	local names_p ""
    forvalues i=0/50 {
        local names_p "`names_p' _atppy`i'"
    }
	 
	local names_mp ""
    local names_mp "`names_m' `names_p'"
	
    mat colnames b = `names_mp'
    mat colnames V = `names_mp'
    mat rownames V = `names_mp'


    /// Post results matrixes to e (for feeding to nlcom)
    epost_bv b V
    
    /// Matrix to store relative % change results
	mat rpe_bytime_`pc' = J(51,3,.)
	
	/// Calculate relative % change over peace years
	forvalues i = 0/50 {
	    
		local ip1 = `i'+1
	
	    nlcom (rpe:((_b[_atppy`i']/_b[_atmpy`i'])-1)*100)
	
		mat rpe_bytime_`pc'[`ip1', 1] = r(b)[1,1]
		mat rpe_bytime_`pc'[`ip1', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
		mat rpe_bytime_`pc'[`ip1', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
	}
	
	/// Convert matrix to data file
	qui xsvmat rpe_bytime_`pc', saving(rpe_bytime_`pc', replace)
	qui use rpe_bytime_`pc', clear
	qui gen pc = `s'
	qui gen pceyrs = _n-1
	qui rename rpe_bytime_`pc'1 _e
    qui rename rpe_bytime_`pc'2 _lb
    qui rename rpe_bytime_`pc'3 _ub
	qui save rpe_bytime_`pc', replace
	
	qui frame change default
	qui frame drop rpe_bytime_`pc'
	qui est restore pr
}

frame create forsaving1	
frame change forsaving1	
		
	
/// Append files
use rpe_bytime_small_1, clear
append using rpe_bytime_middle_1
append using rpe_bytime_major_1
append using rpe_bytime_world_1
save rpe_bytime_all, replace



use rpe_bytime_all, clear


twoway (line _e pceyrs if pc == 2, color(gs5%90) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc == 4, color(gs13%90) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3, color(gs9%90*.9) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1, color(gs1%90*1.375) lpattern(solid) lwidth(vthick)), yline(0) ytitle("Relative % effect on Pr(MID initiation)", size(10.5pt)) ylabel(, tlcolor(black) labsize(10.5pt) nogextend) xtitle(" ") xlabel(, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none) titlegap(-2.1))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=1)) graphregion(margin(r=3 l=-4.75 t=.4 b=-8)) xsize(7) legend(off) name(pyworthog,replace) 

	

frame change default
frame drop forsaving1	
	

	
*Estimation of in-sample effects
***********************************************************
parallel initialize 6, force s("C:\Program Files\Stata17\StataBE-64.exe")

/// Program for estimation in-sample effect at +1 S.D.
capture program drop bytime2plus
program define bytime2plus
syntax varlist

/// Temporary sample
sample 20

// Sort by dyad sample size (to ensure computation of within effect on full 52 year sample)	
tempname dyobs		
qui bysort ddyadid: gen `dyobs' = _N
qui gsort - `dyobs' +ddyadid + year

// Tag 1st ob. (full sample dyad, that is, nonmissing 1949-2000)
gen obno1 = _n			

// Set status deficit at +1 S.D. for 1st ob. only 
*| Remaining obs fixed at 0
qui replace defz = 1 if obno1 == 1

// Macro for position of tagged ob when re-sorted by dyad-year
sort ddyadid year	
tempname neword
gen `neword' = _n
qui su `neword' if obno1 == 1, meanonly
local ob1pos = r(mean)

// Re-generate within status deficit components at + 1 S.D. (for tagged ob) 
sort ddyadid year
qui by ddyadid: center defz , prefix(W2_) mean(B2_)
           
/// Temp file to post estimation results via loop
			estimates use prs
			tempname plus
			sleep 10000
			postfile `plus' ddyadid year _prp pceyrs using "Status Conflict among Small States\Data Analysis\Datasets\Derived\rpe_bytime2plus_`varlist'", replace
			
			/// Loop over every ob in subpopulation sample
			local tobs = _N
			//local tobst = `tobs'/20
		    forvalues i = 1/`tobs' {
								
				/// Set restoration point
				preserve
				
				sort ddyadid year
				 
				 /// Loop over CINC(ln) splines
		        forvalues k = 0/7 {
					*| Reset CINC splines at ob"i" in-sample value
					*| Use both between AND within in-sample values of CINC
				    qui replace W_mcaplnk_`k' = W_mcaplnk_`k'[`i'] 
					qui replace B_mcaplnk_`k' = B_mcaplnk_`k'[`i'] 
					*| Update cross-level interaction for tagged ob using 
					*|ob "i" in-sample between CINC value at +1 S.D.
				    qui gen double W2_defXB_mcaplnk_`k' = W2_defz*B_mcaplnk_`k'
					qui by ddyadid: center W2_defXB_mcaplnk_`k', prefix(W2_) mean(B2_)
		            qui su W2_W2_defXB_mcaplnk_`k' if obno1 == 1, meanonly
		            qui replace W_W_defXB_mcaplnk_`k' = r(mean) 
		            qui su B2_W2_defXB_mcaplnk_`k' if obno1 == 1, meanonly
	                qui replace B_W_defXB_mcaplnk_`k' = r(mean) 
				
			    }
				

				/// Loop over peace years splines
		        forvalues k = 1/4 {
                    /// Reset at ob "i" in-sample valye
					qui replace pceyrsk_`k' = pceyrsk_`k'[`i'] 
					*| Update 3-way interaction at +1 S.D.
					forvalues k2 = 0/7 {
						qui replace W_W_defXB_mcaplnk_`k2'Xpyk_`k' = W_W_defXB_mcaplnk_`k2'*pceyrsk_`k'
						qui replace B_W_defXB_mcaplnk_`k2'Xpyk_`k' = B_W_defXB_mcaplnk_`k2'*pceyrsk_`k'
					}
				}
				
				/// Update status deficit within/between components 
				*| at +1 S.D.
	            qui replace W_defz = W2_defz[`ob1pos'] 
		        qui replace B_defz = B2_defz[`ob1pos'] 

	            /// Generate ob "i" in-sample prediction
			    qui predict double pr`i', pr
				*| Within deficit = +1; between deficit = 0
				*| Within CINCk = "i"; between CINCk = "i"
				*| Peace yearsk = "i"
				*| 3way interaction = def=1*cinck="i"*peaceyearsk="i"
				
				/// Post prediction and ob-i identifiers to tempfile 
				local ddyadid = ddyadid[`i']
				local year = year[`i']
				local _prp = pr`i'[`ob1pos']
				local pceyrs = pceyrs[`i']	
				sleep 500
				*| Force break in stata to give OS time to "catch up"
				post `plus' (`ddyadid') (`year') (`_prp') (`pceyrs')
			    restore
			}
			
			/// Save tempfile
			sleep 20000
			*| Force break in stata to give OS time to "catch up"
			postclose `plus'
		    end




*>~< Run the follopwing commands for each expected status class
local sample small_1 middle_1 major_1 world_1
local s 0
foreach pc in `sample' {
	
	estimates use prs

	/// Increment
	local ++ s

	/// New frame
	qui frame copy default rpe_bytime2_`pc'
	qui frame change rpe_bytime2_`pc'
	sort ddyadid year
		
	/// Re-generate (and reset) within/between components of status deficit to global mean
	qui replace defz = 0
	qui by ddyadid: center defz, prefix(Wm_) mean(Bm_)
	qui replace W_defz = Wm_defz
	qui replace B_defz = Bm_defz
	
	qui drop Wm_* Bm_*
	
	/// Change within status deficit value to 0/1 (for first observation only)
	*| First observation is always on full 50 2 year sample, meaning within-between deviation values are same throughout
    keep if `pc' == 1
	keep if pceyrs <= 50
	sort ddyadid year

	*>~< Loop for discrete change in status deficit 
	local f 0
	forvalues p = 0/1 {
		
        local ++ f
		
		
		*>~< Loop for effect at mean
		if `f' == 1 {
			
			/// Update within-deficitXbetween-CINC(ln) interaction 
			*| Status deficit = 0, so interaction = 0
		forvalues k = 0/7 {
			
			qui gen double W2_defXB_mcaplnk_`k' = W_defz*B_mcaplnk_`k'  
			
			qui by ddyadid: center W2_defXB_mcaplnk_`k', prefix(W2_) mean(B2_)
		   
		    qui replace W_W_defXB_mcaplnk_`k' =  W2_W2_defXB_mcaplnk_`k'
	        qui replace B_W_defXB_mcaplnk_`k' = B2_W2_defXB_mcaplnk_`k'
		
		
	        *| >~< Loop over peace years splines
		    forvalues k2 = 1/4 {
				qui replace W_W_defXB_mcaplnk_`k'Xpyk_`k2' = W_W_defXB_mcaplnk_`k'*pceyrsk_`k2'
				qui replace B_W_defXB_mcaplnk_`k'Xpyk_`k2' = B_W_defXB_mcaplnk_`k'*pceyrsk_`k2'
			}
		}
	   
		
	        /// Generate predictions for all in-sample values
		    frame copy rpe_bytime2_`pc' rpe_bytime2`f'_`pc'
		    frame change rpe_bytime2`f'_`pc'
            qui predictnl double _prm = predict(pr)
		    
			/// Save predictions (and observation identifiers)
			keep ddyadid year _prm pceyrs 
			save rpe_bytime2mean_`pc', replace
		    
			/// Reset frames
			qui frame change rpe_bytime2_`pc'
				
			qui drop W2_* B2_*
				
		    }
				 	
		*>~< Loop for effect at +1 S.D.
		else if `f' != 1 {
			
			/// Run estimation program within parallel 
			parallel, program(bytime2plus) : bytime2plus `pc'
			
		}
		
	}
	
	use rpe_bytime2mean_`pc', clear
	/// Add in-sample +1 S.D. predictions
	merge 1:1 ddyadid year using "Status Conflict among Small States\Data Analysis\Datasets\Derived\rpe_bytime2plus_`pc'", nogen
	/// Calculate discrete change from 0 to 1 S.D. for in-sample values
	gen _rpe = ((_prp/_prm)-1)*100
	gen pc = `s'
	drop if _rpe == .
	save rpe_bytime2_`pc', replace
	
	frame change default 
	frame drop rpe_bytime2_`pc'
}



/// Append
use rpe_bytime2_small_1, clear
append using rpe_bytime2_middle_1
append using rpe_bytime2_major_1
append using rpe_bytime2_world_1
save rpe_bytime2_all, replace



**** Binning of in-sample effects *********************** POSSIBLY WRONG CODE
* May not need to bin....decide
///use rpe_bytime2_all, clear
use rpe_bytime2_all, clear


///use pc_bytime_insam__all, clear

/// Generate base bin variables
gen _avrpe = .	
gen _avpy = .

/// Loop over expected status
forvalues j = 1/3 {
	
	noi di `j'
	
    *| Temp variable names
	tempname obs binno avpy avpyR binob1 binno2 avrpe
	
	*| Sort by peace years
	qui sort pceyrs
	
	*| Observation number by subpopulation
    qui generate `obs' = _n if pc == `j'
    qui su `obs'
    local subobs = r(N)

    *| Number of bins (proportional to subpopulation size)
    local nbins = floor(sqrt(`subobs'))
	noi local nbins: di %3.0f `nbins'
  
    *| Bins of (roughly) equal size 
    qui egen `binno' = cut(`obs') if pc == `j' , group(`nbins') icodes 

    *| Average peace years value by bin
    qui egen `avpy' = mean(pceyrs) if pc == `j', by(`binno')
	
	*| Round peace years
	qui gen `avpyR' = round(`avpy', 1) if `avpy' < .
	 
	
	/// Tag 1st ob in each bin
	//qui bysort `binno' : gen `binob1' = 1 if _n == 1
	
	//qui sort _rpe 

	/// Loop over peace years
	
	//forvalues i = 0/50 {
		
		//tempname binno2 avrpe
		
		*| No. bins for each value of peace years  (proportionate to density of peace years variable)
		//qui count if `avpyR' == `i' & `binob1' == 1
		//local nbins2 = r(N)
		
		*| Loop over peace years with multiple bins
		//if `nbins2' > 1 {
			
			// *| Bins of roughly equal size within each value of peace years 
			 ///qui egen `binno2' = cut(`obs') if pceyrs == `i' & pc == `j', group(`nbins2') icodes
			 	 
			 /// Average estimate by bin
			 qui egen `avrpe' = mean(_rpe) if `binno' < ., by(`binno')
			 
			 qui replace _avrpe = `avrpe' if `avrpe' < . 
			 qui replace _avpy = `avpyR' if `avrpe' < .
			
		}
		
	
		*| Loop over peace years  with single bin
		//else if `nbins2' == 1 {
			
			//qui su _rpe if pceyrs == `i' & pc == `j'
			
			//qui replace _avrpe = r(mean) if pceyrs == `i' & pc == `j' 
			
			//qui replace _avpy = `i' if pceyrs == `i' & pc == `j' 
		//}
		
		
		*| No bin
		//if `nbins2' == 0 {
			///di "Do nothing"
		//}//
	//}//
///}//

duplicates drop _avrpe _avpy, force
sort pc _avpy


save rpe_bytime2_all_bins, replace



*Plot
*****************************


use rpe_bytime_all, clear


twoway (line _e pceyrs if pc == 2, color(2) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc == 4, color(4) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3, color(3) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1, color(personal) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc ==  5, color(black) lpattern(solid) lwidth(vthick)) (scatter _e pceyrs if pc == 5, color(black) msize(medlarge) symbol(oh)), yline(0, lwidth(medthin))  ytitle("% effect on Pr(MID initiation)", size(10.5pt)) ylabel(, tlcolor(black) labsize(10.5pt) nogextend) xtitle(" ") xlabel(, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none) titlegap(-2.1)) plotregion(lcolor(black) margin(r=.375 b=1.5 l=1)) graphregion(margin(r=3 l=-4.75 t=.4 b=-8)) xsize(7) legend(label(4 "Small") label(1 "Middle") label(3 "Major") label(2 "World") label(5 "Mean") label(6 "In-sample bin") order(0 "{bf:Expected status}" 4 1 3 2 0 "{bf:Estimate}" 5 6) rows(1) pos(6) ring(0) size(9pt) region(margin(t=2 b=.5))) name(pynoorthog,replace) 



use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear




///histogram variables allows seperation by power class
twoway__histogram_gen pceyrs if pclass_1 == 1 & inrange(pceyrs , 0 ,55 ), density width(5)  start(0) gen(h1 x1)
twoway__histogram_gen pceyrs if pclass_1 == 2 & inrange(pceyrs , 0 ,55  ), density width(5) start(0) gen(h2 x2)
twoway__histogram_gen pceyrs if pclass_1 == 3 & inrange(pceyrs , 0 ,55 ), density width(5) start(0) gen(h3 x3)
twoway__histogram_gen pceyrs if pclass_1 == 4 & inrange(pceyrs , 0 ,55  ), density width(5) start(0) gen(h4 x4)

replace x1 = x1 -4
replace x2 = x2 - 3
replace x3 = x3 - 2
replace x4 = x4 - 1

twoway (bar h1 x1, barw(1) lwidth(vthin) fcolor(personal%95) lcolor(personal*1.3) lwidth(vthin))  (bar h2 x2, barw(1) lwidth(vthin) fcolor(2%95) lcolor(2*1.3) lwidth(vthin))  (bar h3 x3, barw(1) lwidth(vthin) fcolor(3%95) lcolor(3*1.3) lwidth(vthin)) (bar h4 x4, barw(1) lwidth(vthin) fcolor(4%95) lcolor(4*1.3) lwidth(vthin))  , ytitle("Density", size(10.5pt)) ylabel(0(.02).04, tlcolor(black) labsize(10.5pt) nogextend) xtitle("Year since MID", size(10.5pt)) xlabel(,labcolor(black) labsize(10.5pt) tlcolor(black) nogextend) xscale(alt lstyle(none) titlegap(-1))  yscale(reverse lstyle(none) titlegap(-1.6)) plotregion(lcolor(black)  margin(r=.375 t=-1.92 l=-8.25 b=-1)) graphregion(margin(r=3 l=-4.55 b=2.25 t=-2.75)) fysize(22.5) fxsize(200) legend(off)  name(pyhist, replace)



grc1leg2 pyhist pynoorthog, rows(2) xsize(9) ysize(6) imargin(l=-4 r=-2.28 t=-1.325 b=-1.325) graphregion(margin(r=1.5 l=3.85 t=-1)) iscale(.825) legendfrom(pynoorthog) note("{it:Note}. Program to generate in-sample binned estimates is highly computationally intensive. For now, I present estimates derived from population samples to speed up computation time.", size(7.5pt)) name(rpc_all, replace)



gr_edit .plotregion1.graph1.yaxis1.title.DragBy 0 -1.05



use rpe_bytime2_all_bins, clear
drop __*

addplot 2 :(scatter _avrpe _avpy if pc ==4, msize(vsmall) symbol(O) mlcolor(gs13) mfcolor(gs13%75)), legend(off) norescaling


addplot 2:(scatter _avrpe _avpy if pc ==3, msize(3.5pt) symbol(o) mfcolor(3) mlcolor(3*1.3) jitter(1)), legend(off) norescaling


addplot 2 :(scatter _avrpe _avpy if pc ==2 & _avrpe < 200, msize(3.5pt) symbol(o) mfcolor(2) mlcolor(2*1.3) jitter(1.5)), legend(off) norescaling


addplot 2 :(scatter _rpe pceyrs if pc ==1 & _avrpe < 200, msize(3.5pt) symbol(o) mfcolor(personal) mlcolor(personal*1.3)  jitter(1.5)), legend(off) norescaling

*alignment of y titles
* reduce top margin
*add note
*add mean and in-sample in legend
*done

* final plot, done

* quikcly amend g1 to get it as save dgraph and done.
*also make text larger. and and make graph longer to fit in more note at bottom.


colorpalette lightblue crimson, ipolate(4) globals



/// Plot scatter

/// Plot mean

/// Add density histogram


























*******************************************************************************************************


	
//replace pceyrs = pceyrs + 1 if pc == 2
//replace pceyrs = pceyrs + 2 if pc == 3
//replace pceyrs = pceyrs + 3 if pc == 4	




twoway (line _e pceyrs if pc == 2, color(gs4*.9) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc == 4, color(gs12) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3, color(gs8) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1, color(gs0) lpattern(solid) lwidth(vthick))   , legend(off)	 

 (rline _lb _ub pceyrs if pc == 2, color(gs4%85) lpattern(solid) lwidth(medthin)) (rline _lb _ub pceyrs if pc == 4,color(gs12%85) lpattern(solid) lwidth(medthin)) (rline _lb _ub pceyrs if pc == 3, color(gs8%85) lpattern(solid) lwidth(medthin))  (rline _lb _ub pceyrs if pc == 1, color(gs0%85*2.5) lpattern(solid) lwidth(medthin)) 

* add pece years density plot




















	
****************************************************************************************************************************************************

frames reset
frame change default
use acopyfornow, clear

status gains var simply 0 1, up to t+185, using t+6, (afternonn tomorrow) obviously will have little effect but w/e, simplifies variable creation and makes interaction effect even more meanignful model with var and interaction - full margins models by evening, bootstrap programs by night - get combine plot looking buff (try to 20 years first (all done) one day)

coefplot one full day

go back and specify interactions properly for within0between, tidy plots, code - WRITE.

diag x2 rpch wb haz bytime coefplot

















model next day, with var and interaction, with switching values - put in loop after logit for statusgains,  replace 0, replace 1, (and its interactions with stsdef and peaceyears) with all other vars switching in same way e.g. stsdef switching from -1 to 1 in both loops for nogains/gains, ony up to t+20 (LTEs of sorts). Could do separate matrices here...don't matter really, as long as matrix exists can do nlcom as long as from same model - nlcom matrices won't be erased after est restore, so it blessed

Ca bootstrap nogains vs gains in same program, by making double predictions on model(resetting values twice via clonevar as with above) and putting it all in one big matrix ( ngb ngn gb gn, especially since only going to t+20) - as long as same model its blessed. switching cna be doen various time on same model. easier to do all in one matrix for bootstrap.

* then simply plot, make it look good.

*coefplot, t0-t20, t21-t50. Absolute % change & relative, again bootstrapping can be done by taking predictions for all variables and put into matrix. As long as same model its blessed

	

	
	
	
	
	
	
	
	
	
	
* insert post-program to feed estimates to nlcom
	
	

* All that is left is to feed results to nlcom  via e post (30 mins), plot, put into loop by power group (45mins), plot briefl - clean room eat. create status gain var(2.5hr), run model with status gain var three-way interaction - defxpceyrs, then manually add extra i as defxpceyrsX status-gains with 1 as pr1 and 2 in pr2 (leaving only defxpceyrsxstatusgain in model, not defxpceyrs) - something like that, predicitons should be adjusted by inclusion of double val...and then maybe bootstrapped se, (dont think want to center defxoceyrs, but could try?) and then comparison plot. Coefplot for say 6-7 variables, within effects

*maybe absolute % change for few variables for comparison, and done.
	  
	  

	/// No standardization
	
	*rest of variables set to within 0.05sd of global or subpopulation mean - specifically, between capabilities is set at the subpopulation-mean; within capabilities to within 0.05sd of the global mean; between status deficit to within 0.05sd of the global mean; and the between component of the within-status deficit by between-capabilities interaction to within 0.05sd of the sub-population mean - while switching within status deficit, and its correpsoding within interaction components, from -1sd to +1sd, giving the difference between downward and upward deviation from the mean. 
	
	*Can draw comparison between status groups because only change is based on thing thast distinguishes groups - capabilities
	while switching status deficit and
	
	
	while switching within status deficit, and its corresponding interaction values interactions, from -1 to 1....
	
*Moderating effect of pceyrs of status deficit exists independent of capabilities, just as moderating effect of capabilities of status deficit exists indepndent of peace years. Hence, two separate interactions.

* Moderating effect of status gains on status deficit will vary across peace years. The effect of status gains 5 five years after conflcit will be significantly greater versus 50 years after conflict, by which point huge changes can happen which will drive behaviour rather a previous and most likely moderate decrease/increase in status. Hence, three-way interaction between status deficit, peace years and status gains. Expect predictions to be very similar at 15+ years after conflict for no gains versus gains.
*So,  w_wdefxpeaceyears b_wdefxpceyrs X staus gains(1, 2)








 Hence, three way interaction between status deficit, peace years and status gains
	
	
	
	
	
	
	

	
	predict pr3
	  
	  
	  
	 
	
	 
	 
	 noi replace w_defXb_mcap = W2_comstsdefpw_1_lg1_z1*B2_mcap_1_lg1_ln_z1
	 
	 










