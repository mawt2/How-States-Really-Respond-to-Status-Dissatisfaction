/// So simple interaction between status gains and status deficit up to 20 years most recently conflict
///Beyond this point, and in line with theoretical expectations, the moderating effect of the positional ramifications
///of past conflict is impossible to disentangle from the development of unobserved, heterogeneuous
///influences and leaders' neglect or disregard of past experiences.


frames reset
set varabbrev off
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear


merge 1:1 ddyadid year using "Status Conflict among Small States\Data Analysis\Datasets\Derived\01-Data Management\01-Status Deficit & MID\d-Militarized Interstate Disputes\scss-0101d-embmid.dta", nogen



/// Generate status gains variables
qui bysort ccode1 year: egen tag = max(midint==1)

qui bysort ddyadid (year): gen attgblsts_1_ld5 = attgblsts_1_lg1[_n+6] if tag == 1 

qui gen gblstschange_5y = (attgblsts_1_ld5  - attgblsts_1_lg1)*-1 if tag == 1

qui bysort ccode1 (year): carryforward gblstschange_5y, gen(gblstschange_5yf)

qui gen stsgain = 1 if gblstschange_5yf > 0 & gblstschange_5yf != .

qui replace stsgain = 0 if gblstschange_5yf <= 0 & gblstschange_5yf != .

replace stsgain = 0 if stsgain == .


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

/// Defixit X gains
gen W_defXstg  = W_defz*stsgain

keep if `in'

/// Estimate model with interaction for within-deficitXstatus-gains
logit midint W_defz B_defz W_mcaplnk_0 W_mcaplnk_1 W_mcaplnk_2 W_mcaplnk_3 W_mcaplnk_4 W_mcaplnk_5 W_mcaplnk_6 W_mcaplnk_7 B_mcaplnk_0 B_mcaplnk_1 B_mcaplnk_2 B_mcaplnk_3 B_mcaplnk_4 B_mcaplnk_5 B_mcaplnk_6 B_mcaplnk_7 pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4 stsgain W_defXstg `3way', cluster(ddyadid)
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
	
	est restore pr
	
	*| Increment
	local ++ s

	*| New frame
	qui frame copy default rpe_bytimeg_`pc'
	qui frame change rpe_bytimeg_`pc'
	
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
	
	
	/// Change status gains var to 0/1
	local f -1
	forvalues i = 0/1 {
		local ++ f
	    noi replace stsgain = `i'
	   
	    *| Change within status deficit value to 0/1 (for first observation only)
	    *... First observation is always US-CAN, which means that variable values are updated on full 50 year sample
        qui tempvar obno1
	    qui sort ddyadid year
	    qui gen `obno1' = _n 
	
	    **>~< Loop for discrete change in status deficit 
	    local ff -1
	    forvalues p = 0/1 {
		
            local ++ ff
						 	
		    **>~< Reset status deficit to +1 for discrete change (otherwise simply update interactions)
		    if 	`ff' == 0 {
				qui replace defz = `p'
			}
			
			
			else if `ff' == 1 {
			
			    ***|
			    qui replace defz = `p' if `obno1' == 1
			
			}
			
			
	       
			***| Re-generate within/between status deficit components at + 1.s.d 
	        qui by ddyadid: center defz , prefix(W2_) mean(B2_)
			  							
		    ***| Update core within/between components (within = +1S.D. devation, between = mean) 
			qui replace W_defz = W2_defz[1]
		    qui replace B_defz = B2_defz[1]
		    qui su W_defz
		    di "`r(mean)' is within deficit deviation for def= `p'"
		    qui su B_defz
            di "`r(mean)' is between deficit mean"
		
	    **>~< Loop to update within-deficitXbetween-CINC(ln) interaction (using non-updated W2_defz to preserve variation)
		forvalues k = 0/7 {
			
			***| For mean prediction, simply update interaction using mean-updated within component
			**| (do not need to preserve variation in within component since mean represents zero deviation - W_defz)
			
			if `ff' == 0 {
				qui gen double W2_defXB_mcaplnk_`k' = W_defz*B_mcaplnk_`k'
			}
			
			
			***| For +1 S.D. prediction, must use the re-generated within-deficit component (W2_defz), since computation of cross-level interaction needs varaiation in within component
			else {
				
	            qui gen double W2_defXB_mcaplnk_`k' = W2_defz*B_mcaplnk_`k'
			}
			
			
			qui by ddyadid: center W2_defXB_mcaplnk_`k', prefix(W2_) mean(B2_)
		    qui su W2_W2_defXB_mcaplnk_`k' if `obno1' == 1, meanonly
		    qui replace W_W_defXB_mcaplnk_`k' = r(mean)
		    qui su B2_W2_defXB_mcaplnk_`k' if `obno1' == 1, meanonly
	        qui replace B_W_defXB_mcaplnk_`k' = r(mean)
			
			
			forvalues k2 = 1/4 {
				
				qui replace W_W_defXB_mcaplnk_`k'Xpyk_`k2' = W_W_defXB_mcaplnk_`k'*pceyrsk_`k2'
		        qui replace B_W_defXB_mcaplnk_`k'Xpyk_`k2' = B_W_defXB_mcaplnk_`k'*pceyrsk_`k2'
			}
		}
		   
			
			
           // Generate interaction for within deficit X peace years (still try centered version, but centered peace years doesn't make much   sense....)
           replace W_defXstg = W_defz*stsgain
		
	        /// Generate predictions with partial derivatives
		    if `f' == 0 {
			   
			    if `ff' == 0 {
                    qui predictnl double prmng = predict(pr), g(dmng_)
		        }
		
		        else {
		            qui predictnl double prpng = predict(pr), g(dpng_)
		        }
			}
			
			else if `f' == 1 {
			    
				if `ff' == 0 {
					su W_defz
					su B_defz
                    qui predictnl double prmg = predict(pr), g(dmg_)
		        }
		
		        else {
		            qui predictnl double prpg = predict(pr), g(dpg_)
		        }
			}
			    
		    qui drop W2* B2*
	    }
    }
	


	/// Generate partial derivatives if missing
	*| Program checks if derivative variable exists, and generates derivatives as zero if missing
	local levs1 ng g
	local levs2 m p
	
	foreach l in `levs1' {
	
	    foreach ll in `levs2' {
	
	        forvalues i = 1/`paramno' {
            
			    capture confirm var d`ll'`l'_`i'
            
			    if c(rc) == 111 { 
                
				    qui gen d`ll'`l'_`i' = 0
				}
				
				else {
					continue
				}
			}
		}
	} 
								 	
	/// Drop redundant observations/variables
	qui sort pceyrs
	qui drop if pceyrs > 50
	qui duplicates drop pceyrs, force
	qui keep _est_pr pr* dm* dp*
	
	qui est restore pr
		
	/// Generate estimates matrix
	local colno_b = _N*4
	noi di "`colno_b'"
	mat b = J(1,`colno_b',.)
	local k 0
	local levs1 ng g
	foreach l in `levs1' {
	       
		local ++ k
		local kk 0
	    
	    local levs2 m p
	    foreach ll in `levs2' {
		    
			local ++ kk
		    
		    forvalues j =1/51 {
			    
				if `k' == 1 {
				    
					if `kk' == 1 {
						
						mat b[1,`j'] = pr`ll'`l'[`j']
		               
			        }  
					
					else {
					    
					    local jp1 = `j'+51
			            mat b[1,`jp1'] = pr`ll'`l'[`j']
					}
				}
				
				else if `k' == 2 {
				     
					if `kk' == 1 {
						
						local jp2 = `j'+102
					    mat b[1,`jp2'] = pr`ll'`l'[`j']
		                
			        }  
					
					else {
					    
						local jp3 = `j'+153
			            mat b[1,`jp3'] = pr`ll'`l'[`j']
					}
				}
			}
		}
	}
				  
	
	/// Generate Jacobian matrix
	
	local rowno_j = _N*4
	local paramno = e(k)
	
	mat J = J(`rowno_j',`paramno',.)
	
	local cnames_j: colnames e(V)
	
	mat colnames J = `cnames_j'
	
	local k 0
	local levs1 ng g
	foreach l in `levs1' {
	    	    
		local ++ k
		local kk 0
		
		local levs2 m p
	    foreach ll in `levs2' {
		    
			local ++ kk
			
		    forvalues i = 1/`paramno' {
	
	            forvalues j =1/51 {
				    
					if `k' == 1 {
					    
						if `kk' == 1 {
							 mat J[`j',`i'] = d`ll'`l'_`i'[`j']
							 	 
						}
						
						else {
							local jp1 = `j'+51
							 mat J[`jp1',`i'] = d`ll'`l'_`i'[`j']
						}
					}
					
					else if `k' == 2  {
					    
						if `kk' == 1 {
							
						    local jp2 = `j'+102
							mat J[`jp2',`i'] = d`ll'`l'_`i'[`j']
							 	 
						}
						
						else {
						    
						    local jp3 = `j'+153
							mat J[`jp3',`i'] = d`ll'`l'_`i'[`j']
						}
					}
				}
			}
		}
	}
	
	
					    
					
	/// Generate variance-covariance matrix
	mat V = J*e(V)*J'
	
	/// Macro for results matrix row/column names
	local levs ng g
	foreach l in `levs' {
	    	
	    local names_m`l' ""
        forvalues i = 0/50 {
            local names_m`l' "`names_m`l'' _atm#`l'py`i'"
        }
	
	    local names_p`l' ""
        forvalues i=0/50 {
            local names_p`l' "`names_p`l'' _atp#`l'py`i'"
        }
	}
	local names_all ""
    local names_all "`names_mng' `names_png' `names_mg' `names_pg'"
	
    mat colnames b = `names_all'
    mat colnames V = `names_all'
    mat rownames V = `names_all'


    /// Post results matrixes to e (for feeding to nlcom)
    epost_bv b V
    
	
    /// Matrix to store relative % change estimates
	mat rpe_btg_`pc' = J(102,3,.)
	
	/// Estimate relative % change over peace years by nogains/gains *try difference in change, then compare with bootstrapped se (220 reps test) - both by tomorrow night
	forvalues i = 0/50 {
	    
		local ip1 = `i'+1
		
	    nlcom (rpe:((_b[_atp#ngpy`i']/_b[_atm#ngpy`i'])-1)*100)
	
		mat rpe_btg_`pc'[`ip1', 1] = r(b)[1,1]
	    mat rpe_btg_`pc'[`ip1', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
		mat rpe_btg_`pc'[`ip1', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
		
	}
	
	forvalues i = 0/50 {
	    
		local ip2 = `i'+52
		
		nlcom (rpe:((_b[_atp#gpy`i']/_b[_atm#gpy`i'])-1)*100) 
	
		mat rpe_btg_`pc'[`ip2', 1] = r(b)[1,1]
		mat rpe_btg_`pc'[`ip2', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
		mat rpe_btg_`pc'[`ip2', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
	}

	
	/// Convert matrix to data file
	qui xsvmat rpe_btg_`pc', saving(rpe_bytimeg_`pc', replace)
	qui use rpe_bytimeg_`pc', clear
	qui gen pc = `s'
	tempvar obno
	qui gen stg = 1 if _n > 51
	qui replace stg = 0 if stg == .
	qui gen pceyrs = _n-1 if stg == 0
	replace pceyrs = _n-52 if stg == 1
	qui rename rpe_btg_`pc'1 _e
    qui rename rpe_btg_`pc'2 _lb
    qui rename rpe_btg_`pc'3 _ub
	qui save rpe_bytimeg_`pc', replace
	
	qui frame change default
	qui frame drop rpe_bytimeg_`pc'
	qui est restore pr
	
}


**** Set stata to run across 6 cores
parallel initialize 6, force s("C:\Program Files\Stata17\StataBE-64.exe") // Set Stata to run across 6 cores
*** Program for multi-core computation of in-sample predictions
capture program drop bytimeg2plus
program define bytimeg2plus
    syntax varlist
    *| Sort by dyad sample size (to ensure computation of within effect on full 52 year sample)	
    tempname dyobs		
    bysort ddyadid: gen `dyobs' = _N
    gsort - `dyobs' +ddyadid + year
    *| Tag 1st ob. (always full sample dyad)
    gen obno1 = _n			
    *| Set status deficit at +1 S.D. for 1st ob. only 
    qui replace defz = 1 if obno1 == 1
    *| Macro for 1st ob. position when re-sorted by dyad-year
    sort ddyadid year	
    tempname neworder
    gen `neworder' = _n
    su `neworder' if obno1 == 1, meanonly
    local ob1pos = r(mean)
    *| Re-generate within status deficit components at + 1 S.D. (for 1st ob)
    by ddyadid: center defz , prefix(W2_) mean(B2_) 
    *| Temp file to post estimation results 
    estimates use prs
    tempname plus
    postfile `plus' ddyadid year _prp pceyrs using rpe_bytimeg2_plus_`varlist', replace
    *| Loop over each ob. 
    local tobs = _N
    forvalues i = 1/`tobs'{
        preserve
    ################# INSERT ESTIMATION SCRIPT FROM Figure 1.4.3 ######################		
        *| Generate predictions
	qui predict double pr`i', pr
	*| Post prediction and ob. identifiers to tempfile
        local ddyadid = ddyadid[`i']
	local year = year[`i']
        local _prp = pr`i'[`ob1pos']
        local pceyrs = pceyrs[`i']	
	post `plus' (`ddyadid') (`year') (`_prp') (`pceyrs') 
	restore
    }
    *| Save tempfile
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
	qui frame copy default rpe_bytimeg2_`pc'
	qui frame change rpe_bytimeg2_`pc'
		
	/// Re-generate (and reset) within/between components of status deficit to global mean
	qui replace defz = 0
	qui by ddyadid: center defz, prefix(Wm_) mean(Bm_)
	qui replace W_defz = Wm_defz
	qui replace B_defz = Bm_defz
	
	qui drop Wm_* Bm_*
	

	/// Change within status deficit value to 0/1 (for first observation only)
	*| First observation is always on full 50 2 year sample, meaning within-between deviation values are same throughout
    keep if `pc' == 1
	keep if pceyrs <= 20
	sort ddyadid year
	
	/// Change status gains var to 0/1
	local g -1
	forvalues i = 0/1 {
		
		local ++ g
		
		frame copy rpe_bytimeg2_`pc' rpe_bytimeg2`g'_`pc'
		frame change rpe_bytimeg2`g'_`pc'
				

	    noi replace stsgain = `i'

	    *>~< Loop for discrete change in status deficit 
	    local f 0
	    forvalues p = 0/1 {
		
            local ++ f
		
		
		    *>~< Loop for effect at mean
		    if `f' == 1 {
			
		        /// Update within-deficitXbetween-CINC(ln) interaction (using update since variation not important w=0)
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
						
			    /// Gains interaction
			    replace W_defXstg = W_defz*stsgain
		
	            /// Generate predictions with partial derivatives
		        frame copy rpe_bytimeg2`g'_`pc' rpe_bytimeg2`g'mean_`pc'
		        frame change rpe_bytimeg2`g'mean_`pc'
                qui predictnl double _prm = predict(pr)
		    
			    /// Save predictions
			    keep ddyadid year _prm pceyrs
				gen stg = `g'
			    save rpe_bytimeg2_`g'mean_`pc', replace
		    
			    /// Reset frames
			    qui frame change rpe_bytimeg2`g'_`pc'
				
				drop W2_* B2_*
				
			}
				
				
		    
				 	
		    *>~< Loop for effect at +1 S.D.
		    else if `f' != 1 {
						
			    /// Run estimation program within parallel 
			    parallel, program(bytimeg2plus) : bytimeg2plus `pc'
				
				use rpe_bytimeg2_plus_`pc', clear
				
				gen stg = `g' 
				
				save rpe_bytimeg2_`g'plus_`pc'
				
		    }
		
		
		}
		
		
		use rpe_bytimeg2_`g'mean_`pc', clear
	    merge 1:1 ddyadid year using rpe_bytimeg2_`g'plus_`pc'
	    gen _rpe = ((_prp/_prm)-1)*100
	    gen pc = `s'
	    drop if _rpe == .
	    save rpe_bytimeg2_`g'_`pc', replace
		
		frame change rpe_bytimeg2_`pc'
	}
	 
	/// Clear frames 
	frame change default
	frame drop rpe_bytimeg2_`pc'
	forvalues g = 0/1 {
	    frame drop rpe_bytimeg2`g'_`pc'
		frame drop rpe_bytimeg2`g'mean_`pc'
	}
	
}



/// Append
forvalues g = 0/1 {
    use rpe_bytimeg2_`g'_small_1, clear
    append using rpe_bytimeg2_`g'_middle_1
    append using rpe_bytimeg2_`g'_major_1
    append using rpe_bytimeg2_`g'_world_1
    save rpe_bytimeg2_`g'_all, replace
}



**** Binning of in-sample effects ***********************
forvalues g =0/1 {
///use rpe_bytime2_all, clear
use rpe_bytimeg2_`g'_all, clear

///use pc_bytime_insam__all, clear

/// Generate base bin variables
gen _avrpe = .	
gen _avpy = .

/// Loop over expected status
forvalues j = 1/4 {
	
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
	qui local nbins: di %3.0f `nbins'

    *| Bins of (roughly) equal size 
    qui egen `binno' = cut(`obs') if pc == `j' , group(`nbins') icodes 

    *| Average peace years value by bin
    qui egen `avpy' = mean(pceyrs) if pc == `j', by(`binno')
	
	*| Round peace years
	qui gen `avpyR' = round(`avpy', 1) if `avpy' < .
	
	/// Tag 1st ob in each bin
	qui bysort `binno' : gen `binob1' = 1 if _n == 1
	
	qui sort _rpe 

	/// Loop over peace years
	forvalues i = 0/50 {
		
		tempname binno2 avrpe
		
		*| No. bins for each value of peace years  (proportionate to density of peace years variable)
		qui count if `avpyR' == `i' & `binob1' == 1
		local nbins2 = r(N)
		
		*| Loop over peace years with multiple bins
		if `nbins2' > 1 {
			
			 *| Bins of roughly equal size from within values of peace years 
			 qui egen `binno2' = cut(`obs') if pceyrs == `i' & pc == `j', group(`nbins2') icodes
			 	 
			 /// Average estimate by bin
			 qui egen `avrpe' = mean(_rpe) if `binno2' < ., by(`binno2')
			 
			 qui replace _avrpe = `avrpe' if `avrpe' < . 
			 qui replace _avpy = `i' if `avrpe' < . 
		}
		
	
		*| Loop over peace years  with single bin
		else if `nbins2' == 1 {
			
			qui su _rpe if pceyrs == `i' & pc == `j'
			
			qui replace _avrpe = r(mean) if pceyrs == `i' & pc == `j' 
			
			qui replace _avpy = `i' if pceyrs == `i' & pc == `j' 
		}
		
		
		*| No bin
		if `nbins2' == 0 {
			di "Do nothing"
		}
	}
}

duplicates drop _avrpe _avpy, force
sort pc _avpy
drop __*

save rpe_bytimeg2_`g'_all_bins, replace
}


************ Plot
	
use rpe_bytimeg_small_1, clear

append using rpe_bytimeg_middle_1
append using rpe_bytimeg_major_1
append using rpe_bytimeg_world_1
save rpe_bytimeg_all, replace	
	
	
twoway (line _e pceyrs if pc == 2 & stg == 0, color(2) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc == 4 & stg == 0, color(4) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3 & stg == 0, color(3) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1 & stg == 0, color(personal) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc ==  5, color(black) lpattern(solid) lwidth(vthick)) (scatter _e pceyrs if pc == 5, color(black) msize(medlarge) symbol(oh)), yline(0) ytitle("% effect on Pr(MID initiation)", size(7.5pt)) ylabel(-100(50)200, tlcolor(black) labsize(7.5pt) nogextend) xtitle("Years since MID", size(7.5pt)) xlabel(, tlcolor(black) labsize(7.5pt) nogextend) xscale(lstyle(none) titlegap(-.75) alt) yscale(lstyle(none) titlegap(-2)) subtitle("0/{&minus} change 5yr after MID", box bexpand fcolor(none) size(7.5pt) pos(6) alignment(bottom)) legend(label(1 "Middle") label(2 "World") label(3 "Major") label(4 "Small") label(5 "Mean") label(6 "In-sample") order(0 "{bf:Expected status}" 4 1 3 2  0 "{bf:Estimator}" 5 6) rows(1) pos(6) ring(0) size(5.5pt) region(margin(t=.5 b=-.5))) plotregion(lcolor(black) margin(r=3 b=1.5 l=2.5)) graphregion(margin(r=4 l=-6.75 t=.4 b=2.5)) fxsize(201) name(nogains,replace) 



twoway (line _e pceyrs if pc == 2 & stg == 1, color(2) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc == 4 & stg == 1, color(4) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3 & stg == 1, color(3) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1 & stg == 1, color(personal) lpattern(solid) lwidth(vthick)), yline(0) ytitle(" ") ylabel(-100(50)200, noticks nolabel nogextend) xtitle("Years since MID", size(7.5pt)) xlabel(, tlcolor(black) labsize(7.5pt) nogextend) xscale(lstyle(none) titlegap(-.75) alt) yscale(lstyle(none) titlegap(-1.75)) subtitle("+ change 5yr after MID", box bexpand fcolor(none) size(7.5pt) pos(6) alignment(bottom))  plotregion(lcolor(black) margin(r=2.5 b=1.7 l=3)) graphregion(margin(r=1 l=-4.75 t=.4 b=2.5)) fxsize(199) legend(off) name(gains,replace) 



grc1leg2 nogains gains, xsize(8) ysize(3) imargin(r=-3.8 l=-3.8) graphregion(margin(l=4.5 r=4.5 b=0 t=0)) note("{it:Note}. Program to generate in-sample binned estimates is highly computationally intensive. For now, I present estimates mean estimates only.", size(5pt)) name(rpc_all, replace)




use rpe_bytimeg2_0_all_bins, clear
drop __*

addplot 1 :(scatter _avrpe _avpy if pc ==4, msize(medsmall) symbol(o) mlcolor(gs13) mfcolor(gs13%75) jitter(5))

addplot 1 :(scatter _avrpe _avpy if pc ==3, msize(medsmall) symbol(o) mlcolor(gs9*.9) mfcolor(gs9*.9%75) jitter(5))

addplot 1 :(scatter _avrpe _avpy if pc ==2, msize(medsmall) symbol(o) mlcolor(gs5) mfcolor(gs5%75) jitter(5))

addplot 1 :(scatter _avrpe _avpy if pc ==1, msize(medsmall) symbol(o) mlcolor(gs0) mfcolor(gs1*1.375%75) jitter(5))


use rpe_bytimeg2_1_all_bins, clear
drop __*
addplot 2 :(scatter _avrpe _avpy if pc ==4, msize(medsmall) symbol(o) mlcolor(gs13) mfcolor(gs13%75) jitter(5))

addplot 2 :(scatter _avrpe _avpy if pc ==3, msize(medsmall) symbol(o) mlcolor(gs9*.9) mfcolor(gs9*.9%75) jitter(5))

addplot 2 :(scatter _avrpe _avpy if pc ==2, msize(medsmall) symbol(o) mlcolor(gs5) mfcolor(gs5%75) jitter(5))

addplot 2 :(scatter _avrpe _avpy if pc ==1, msize(medsmall) symbol(o) mlcolor(gs0) mfcolor(gs1*1.375%75) jitter(5))














twoway (line _e pceyrs if pc == 2, color(gs5%90) lpattern(solid) lwidth(vthick)) (line _e pceyrs if pc == 4, color(gs13%90) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3, color(gs9%90*.9) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1, color(gs1%90*1.375) lpattern(solid) lwidth(vthick)),  ytitle("Relative % effect on Pr(MID initiation)", size(10.5pt)) ylabel(, tlcolor(black) labsize(10.5pt) nogextend) xtitle(" ") xlabel(, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none) titlegap(-2.1))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=1)) graphregion(margin(r=3 l=-4.75 t=.4 b=-8)) xsize(7) legend(off) name(pynoorthog,replace) 






















	

	

 (line _e pceyrs if pc == 4 & gains = 0, color(gs13%90) lpattern(solid) lwidth(vthick))  (line _e pceyrs if pc == 3 & gains = 0, color(gs9%90*.9) lpattern(solid) lwidth(vthick))   (line _e pceyrs if pc == 1 & gains = 0, color(gs1%90*1.375) lpattern(solid) lwidth(vthick)),  ytitle("Relative % effect on Pr(MID initiation)", size(10.5pt)) ylabel(, tlcolor(black) labsize(10.5pt) nogextend) xtitle(" ") xlabel(, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none) titlegap(-2.1))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=1)) graphregion(margin(r=3 l=-4.75 t=.4 b=-8)) xsize(7) legend(off) name(pynoorthog,replace) 

	
	
	
replace pceyrs = pceyrs + 1 if pc == 2
replace pceyrs = pceyrs + 2 if pc == 3
replace pceyrs = pceyrs + 3 if pc == 4	



twoway  (rarea _lb _ub pceyrs if pc == 4 & gains == 0, color(gs13%95)) (rarea _lb _ub pceyrs if pc == 3 & gains == 0,color(gs9%95*1.1)) (rarea _lb _ub pceyrs if pc == 2 & gains == 0, color(gs5%95*1.2)) (rarea _lb _ub pceyrs if pc == 1 & gains == 0,color(gs1%95*2.5)) (line _e pceyrs if pc == 2 & gains == 0, color(gs6%95*1.2) lpattern(solid) lwidth(thick))  (line _e pceyrs if pc == 1 & gains == 0, color(gs2%95*2.5) lpattern(solid) lwidth(thick))    (line _e pceyrs if pc == 3 & gains == 0, color(gs10%95*1.1) lpattern(solid) lwidth(thick)) (line _e pceyrs if pc == 4 & gains == 0, color(gs14%95) lpattern(solid) lwidth(thick)), legend(off) ylabel(-100(50)200)  name(nogains, replace)  


twoway  (rarea _lb _ub pceyrs if pc == 4 & gains == 1, color(gs13%95)) (rarea _lb _ub pceyrs if pc == 3 & gains == 1 ,color(gs9%95*1.1)) (rarea _lb _ub pceyrs if pc == 2 & gains == 1, color(gs5%95*1.2)) (rarea _lb _ub pceyrs if pc == 1 & gains == 1,color(gs1%95*2.5)) (line _e pceyrs if pc == 2 & gains == 1, color(gs6%95*1.2) lpattern(solid) lwidth(thick))  (line _e pceyrs if pc == 1 & gains == 1, color(gs2%95*2.5) lpattern(solid) lwidth(thick))    (line _e pceyrs if pc == 3 & gains == 1, color(gs10%95*1.1) lpattern(solid) lwidth(thick)) (line _e pceyrs if pc == 4 & gains == 1, color(gs14%95) lpattern(solid) lwidth(thick)), legend(off) ylabel(-100(50)200)  name(gains, replace)  

twoway (line _e pceyrs if pc == 1 & gains == 1, color(gs2%95*2.5) lpattern(solid) lwidth(thick)) (line _e pceyrs if pc == 2 & gains == 1, color(gs6%95*1.2) lpattern(solid) lwidth(thick))  (line _e pceyrs if pc == 3 & gains == 1, color(gs10%95*1.1) lpattern(solid) lwidth(thick)) (line _e pceyrs if pc == 4 & gains == 1, color(gs14%95) lpattern(solid) lwidth(thick)), ylabel(-50(50)100) legend(off) name(gains, replace)  



* try exonetiated nlcom....if no, then
* bootstrap program, as is, just take standard error from nlcom, foreach model (gains vs nogains) as scalar, if ci still huge, then do "Difference in relative % change in Pr"

*done 

*coefplot, compare absolute effect at t1-20 vs absolute effect at t21-t50, finished. (one/two days)


coconfirm var dbng_2



