frames reset
set varabbrev off

use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear

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


/// Generate peace years splines (4 knots) (non orthog = tiny change, orthog works better here. )
qui sum pceyrs
local min = r(min)
splinegen pceyrs `min' 10 25 50 100 if `in', basis(pceyrsk) degree(3) orthog


/// Generate within-between components of status deficit/CINC(ln) smoothed
qui sort ddyadid year
foreach v of varlist defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 {
    qui by ddyadid: center `v' if `in', prefix(W_) mean(B_) 
}


/// Generate within-deficitXCINC(ln) smoothed interaction
forvalues k = 0/7 {
	gen R_defXR_mcaplnk_`k' = defz*mcaplnk_`k'
	gen W_defXR_mcaplnk_`k' = W_defz*mcaplnk_`k'
	gen B_defXR_mcaplnk_`k' = B_defz*mcaplnk_`k'
}

			
*| Random effects GSEM
clonevar midint1 = midint
clonevar midint2 = midint

gsem(midint1 <- defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 c.defz#c.mcaplnk_0 c.defz#c.mcaplnk_1 c.defz#c.mcaplnk_2 c.defz#c.mcaplnk_3 c.defz#c.mcaplnk_4 c.defz#c.mcaplnk_5 c.defz#c.mcaplnk_6 c.defz#c.mcaplnk_7, logit) (midint2 <- defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 c.defz#c.mcaplnk_0 c.defz#c.mcaplnk_1 c.defz#c.mcaplnk_2 c.defz#c.mcaplnk_3 c.defz#c.mcaplnk_4 c.defz#c.mcaplnk_5 c.defz#c.mcaplnk_6 c.defz#c.mcaplnk_7 pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4,logit), vce(cluster ddyadid)
est store gsem1

*| REWB GSEM
clonevar midint3 = midint
clonevar midint4 = midint

gsem(midint3 <- W_defz B_defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 W_defXR_mcaplnk_0 W_defXR_mcaplnk_1 W_defXR_mcaplnk_2 W_defXR_mcaplnk_3 W_defXR_mcaplnk_4 W_defXR_mcaplnk_5 W_defXR_mcaplnk_6 W_defXR_mcaplnk_7 B_defXR_mcaplnk_0 B_defXR_mcaplnk_1 B_defXR_mcaplnk_2 B_defXR_mcaplnk_3 B_defXR_mcaplnk_4 B_defXR_mcaplnk_5 B_defXR_mcaplnk_6 B_defXR_mcaplnk_7, logit) (midint4 <- W_defz B_defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 W_defXR_mcaplnk_0 W_defXR_mcaplnk_1 W_defXR_mcaplnk_2 W_defXR_mcaplnk_3 W_defXR_mcaplnk_4 W_defXR_mcaplnk_5 W_defXR_mcaplnk_6 W_defXR_mcaplnk_7 B_defXR_mcaplnk_0 B_defXR_mcaplnk_1 B_defXR_mcaplnk_2 B_defXR_mcaplnk_3 B_defXR_mcaplnk_4 B_defXR_mcaplnk_5 B_defXR_mcaplnk_6 B_defXR_mcaplnk_7 pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4, logit), vce(cluster ddyadid) nocapslatent
est store gsem2
local paramno = e(k)


/// Program to post results matrixes to e (in order to feed to nlcom for calculation of relative % change)
capture program drop epost_bv
program define epost_bv, eclass
args b V
ereturn post `b' `V'
end


*<~> Loop over expected status 
local j 0
foreach pc of varlist small_1 middle_1 major_1 world_1 {
    local ++ j
	
    *>~< Loop over models
	local l 0
	forvalues i = 1/2 {	
	    local ++ l
	
		frame copy default wrb`i'_`pc'
		frame change wrb`i'_`pc'
		
		/// Tempfile for saving predictions
		tempfile wrb`i'_`pc'
		
		// Macros for the (approximate) subpopulation mean of CINC(ln) smoothed 
		qui tempvar avmcaplnk_0
		qui tempvar absmcaplnk_0
		qui egen avmcaplnk_0 = mean(mcaplnk_0) if `pc' == 1
		qui gen absmcaplnk_0 = abs(avmcaplnk_0 - mcaplnk_0)
		qui sort absmcaplnk_0
		forvalues k = 0/7 {
			qui local k`k'av = mcaplnk_`k'[1]	
		}
		
		sort ddyadid year
		 
		*<~> Loop for random effects estimates
		if `l' == 1 {
		 	
		    /// Predictions at CINC mean (and post to e)
		    est restore gsem1
            margins, subpop(if `pc' == 1) vce(unconditional) predict(outcome(midint1) fixed) predict(outcome(midint2) fixed) at(defz =(-2(.5)2) mcaplnk_0 = `k0av' mcaplnk_1 = `k1av' mcaplnk_2 = `k2av' mcaplnk_3 = `k3av' mcaplnk_4 = `k4av' mcaplnk_5 = `k5av' mcaplnk_6 = `k6av' mcaplnk_7 = `k7av') post coeflegend
			 
			//// Relative % change
			nlcom (rpc1_1: ((_b[1bn._predict#1bn._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_2: ((_b[1bn._predict#2._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_3: ((_b[1bn._predict#3._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_4: ((_b[1bn._predict#4._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_5: ((_b[1bn._predict#5._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_6: ((_b[1bn._predict#6._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_7: ((_b[1bn._predict#7._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_8: ((_b[1bn._predict#8._at]/_b[1bn._predict#5._at])-1)*100) (rpc1_9: ((_b[1bn._predict#9._at]/_b[1bn._predict#5._at])-1)*100) (rpc2_1: ((_b[2._predict#1bn._at]/_b[2._predict#5._at])-1)*100) (rpc2_2: ((_b[2._predict#2._at]/_b[2._predict#5._at])-1)*100) (rpc2_3: ((_b[2._predict#3._at]/_b[2._predict#5._at])-1)*100) (rpc2_4: ((_b[2._predict#4._at]/_b[2._predict#5._at])-1)*100) (rpc2_5: ((_b[2._predict#5._at]/_b[2._predict#5._at])-1)*100) (rpc2_6: ((_b[2._predict#6._at]/_b[2._predict#5._at])-1)*100) (rpc2_7: ((_b[2._predict#7._at]/_b[2._predict#5._at])-1)*100) (rpc2_8: ((_b[2._predict#8._at]/_b[2._predict#5._at])-1)*100) (rpc2_9: ((_b[2._predict#9._at]/_b[2._predict#5._at])-1)*100), post coeflegend
			 
			
			/// Matrix to store estimates
			mat rpcd_wrb`i'_`pc' = J(9,3,.)
			 
			*>~< Calculate difference in % change via loop o 
			forvalues e = 1/9 {
			 	
			    nlcom (rpcd: (_b[rpc2_`e'] - _b[rpc1_`e']))
				
				mat rpcd_wrb`i'_`pc'[`e', 1] = r(b)[1,1]
	            mat rpcd_wrb`i'_`pc'[`e', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
			    mat rpcd_wrb`i'_`pc'[`e', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
			}
			
			/// Convert matrix to dataset and save
			xsvmat rpcd_wrb1_`pc', saving(rpcd_wrb1_`pc', replace)
	        use rpcd_wrb`i'_`pc', clear
	        rename rpcd_wrb`i'_`pc'1 _e
	        rename rpcd_wrb`i'_`pc'2 _lb
	        rename rpcd_wrb`i'_`pc'3 _ub
	        gen _pc = `j'
			gen _m = `i'
			gen _etype = 1
	        egen _at = fill(-2(.5)2)
			save rpcd_wrb`i'_`pc', replace
			
		}
			 
			
		/// Loop for REWB estimates
		else if `l' == 2 {
	
		    *>~< Loop over CINC(ln) smoothed
	        forvalues k = 0/7 {
		        *| Fix at subpop mean
		        qui replace mcaplnk_`k' = `k`k'av'
			}
		     
				
		    /// Re-set status deficit to global mean (that is, 0)
	        qui su defz, meanonly
	        qui replace defz = r(mean)
		
	        /// Update within and between components of status deficit at mean
		    *| Within = 0 and between = subpop. mean
		    qui by ddyadid: center defz, prefix(Wm_) mean(Bm_)
		    qui replace W_defz = Wm_defz
		    qui replace B_defz = Bm_defz
			
			
	        /// Update interactions at means via loop over CINC(ln) smoothed
		    forvalues k = 0/7 {
		        qui replace W_defXR_mcaplnk_`k' = W_defz*mcaplnk_`k'
			    qui replace B_defXR_mcaplnk_`k' = B_defz*mcaplnk_`k'
		    }
				
		    *>~< Loop for within/between predictions
		    local levs a b
	        local l2 0 
		    foreach e in `levs' {
			
	            local ++ l2
				
		        frame copy wrb`i'_`pc' wrb`i'`e'_`pc'
			    frame change wrb`i'`e'_`pc'
			
			    // Create new frame for storing predictions/derivatives
			    frame create wrb`i'`e'p_`pc'
			    frame change wrb`i'`e'p_`pc'
		        set obs 1
		        gen mtag = 1
			    frame change wrb`i'`e'_`pc'
				
		  	    /// Macros for looping over 1st versus 2nd equation
			    local half = `paramno'/2
				local half1 = `half'-2
				local half2 = `half1' + 1
				di `half1'
				di `half2'
	              
		        *>~< Loop over fixed values of status deficit
		        local f 0
		        forvalues p = -2(.5)2 {
		            local ++ f
				  
		            /// Set restoration point for loop over fixed values of status deficit
			        preserve
				  
			        *>~< Loop for within-effect predictions
			        if `l2' == 1 {
				    
					    /// Tag first observation (for within deviation from mean)
				        qui sort ddyadid year
	                    qui gen obtag = _n 
	                    su ddyadid if obtag == 1
				  	
				        /// For first observation only, switch status deficit across fixed values (so that first observation represents deviation from mean(0), and between is at (approx.) mean)
		                qui replace defz = `p' if obtag == 1
				
			            /// Re-generate within/between components at S.D. increments of status deficit
	                    qui by ddyadid: center defz, prefix(Wf_) mean(Bf_)
			  							
				        /// Update core within/between components 
			  	        qui replace W_defz = Wf_defz[1]
		                qui replace B_defz = Bf_defz[1]
				        qui su W_defz
				        di "`r(mean)' is within deficit deviation for def= `p'"
				        qui su B_defz
					    di "`r(mean)' is between deficit mean"
					
					    /// Update interactions
					    forvalues k = 0/7 {
			                replace W_defXR_mcaplnk_`k' = W_defz*mcaplnk_`k'
			                replace B_defXR_mcaplnk_`k' = B_defz*mcaplnk_`k'
		                }
					  
					    /// Restrict sample by expected status (check that margins give some results and just use that for random)
			            *| A necessary step when averaging predictions across peace years(but unecessary when taking prediction at each peace year)      
						keep if `pc' == 1
						
				        /// Generate within effect predictions with partial derivatives
			            qui est restore gsem`i'
						*| 1st equation (without peace years)
						predictnl double p1_`f' = exp(xb(midint3)), g(d1_`f'_)
			            su p1_`f'
						*| 2nd equation (with peace years)
						predictnl double p2_`f' = exp(xb(midint4)), g(d2_`f'_)
						su p2_`f'
				   
				        /// Generate partial derivatives if missing
	                    *| Program checks if derivative variable exists, and generates derivatives as zero if missing
						
				
			            *| Derivates for 1st equation (without peace years)
			            forvalues k = 1/`half1' {
            
			                capture confirm var d1_`f'_`k'
            
			                if c(rc) == 111 { 
                
				                qui gen d1_`f'_`k' = 0
				            }
				
				            else {
				                continue
				            }
			            }
						
						*| Derivatives for 2nd equations (with peace yeaars)
						forvalues k = `half2'/`paramno' {
							
							capture confirm var d2_`f'_`k'
            
			                if c(rc) == 111 { 
                
				                qui gen d2_`f'_`k' = 0
				            }
				
				            else {
				                continue
				            }
			            }
							
				        /// Mean prediction/partial derivative across peace years
			            *| 1st equation (without peace years)
						egen p1_`f'm = mean(p1_`f')
			            forvalues k = 1/`half1' {
			                egen d1_`f'm_`k' = mean(d1_`f'_`k')
					    } 
						*| 2nd equation (with peace years)
						egen p2_`f'm = mean(p2_`f')
			            forvalues k = `half2'/`paramno' {
			                egen d2_`f'm_`k' = mean(d2_`f'_`k')
					    } 
					  
					  
					    /// Variable for frame merge (to store predictions)
					    keep in 1
				        gen mtag = 1
				        
				        /// Store predictions/derivatives in separate frame
			   	        frame change wrb`i'`e'p_`pc'
						if `f' == 1 {
						    frlink 1:1 mtag, frame(wrb`i'`e'_`pc')
						}
						
				        frget p1_`f'm-d1_`f'm_`half1', from(wrb`i'`e'_`pc')
						frget p2_`f'm-d2_`f'm_`paramno', from(wrb`i'`e'_`pc')
						
					    frame change wrb`i'`e'_`pc'
						
			            restore
				    }
					
					else if `l2' == 2 {
				  	
				        /// Switch status deficit in .5 S.D. increments
		                qui replace defz = `p' 
				     
				        /// Re-generate within/between components of status deficit at S.D. increments
	                    qui by ddyadid: center defz, prefix(Wf_) mean(Bf_)
				 
				        /// Update core within/between components 
			  	        qui replace W_defz = Wf_defz
		                qui replace B_defz = Bf_defz
					    qui su W_defz
					    di "`r(mean)' is within deficit mean"
				        qui su B_defz
					    di "`r(mean)' is between deficit deviation for def = `p'"
					
					    /// Update interactions
					    forvalues k = 0/7 {
			                replace W_defXR_mcaplnk_`k' = W_defz*mcaplnk_`k'
			                replace B_defXR_mcaplnk_`k' = B_defz*mcaplnk_`k'
		                }
				    
				        /// Restrict sample by expected status (check that margins give some results and just use that for random)
			            *| A necessary step when averaging predictions across peace years(but unecessary when taking prediction at each peace year)
			            keep if `pc' == 1
				
						/// Generate between effect predictions with partial derivatives
			            qui est restore gsem`i'
						*| 1st equation (without peace years)
						predictnl double p1_`f' = exp(xb(midint3)), g(d1_`f'_)
			            su p1_`f'
						*| 2nd equation (with peace years)
						predictnl double p2_`f' = exp(xb(midint4)), g(d2_`f'_)
						su p2_`f'
				   
				        /// Generate partial derivatives if missing
	                    *| Program checks if derivative variable exists, and generates derivatives as zero if missing
						
			            *| Derivates for 1st equation (without peace years)
			            forvalues k = 1/`half1' {
            
			                capture confirm var d1_`f'_`k'
            
			                if c(rc) == 111 { 
                
				                qui gen d1_`f'_`k' = 0
				            }
				
				            else {
				                continue
				            }
			            }
						
						*| Derivatives for 2nd equations (with peace yeaars)
						forvalues k = `half2'/`paramno' {
							
							capture confirm var d2_`f'_`k'
            
			                if c(rc) == 111 { 
                
				                qui gen d2_`f'_`k' = 0
				            }
				
				            else {
				                continue
				            }
			            }
							
				        /// Mean prediction/partial derivative across peace years
			            *| 1st equation (without peace years)
						egen p1_`f'm = mean(p1_`f')
			            forvalues k = 1/`half1' {
			                egen d1_`f'm_`k' = mean(d1_`f'_`k')
					    } 
						*| 2nd equation (with peace years)
						egen p2_`f'm = mean(p2_`f')
			            forvalues k = `half2'/`paramno' {
			                egen d2_`f'm_`k' = mean(d2_`f'_`k')
					    } 
					  
					    /// Variable for frame merge (to store predictions)
					    keep in 1
				        gen mtag = 1
				        
				        /// Store predictions/derivatives in separate frame
			   	        frame change wrb`i'`e'p_`pc'
						if `f' == 1 {
						    frlink 1:1 mtag, frame(wrb`i'`e'_`pc')
						}
				        frget p1_`f'm-d1_`f'm_`half1', from(wrb`i'`e'_`pc')
						frget p2_`f'm-d2_`f'm_`paramno', from(wrb`i'`e'_`pc')
						
					    frame change wrb`i'`e'_`pc'
						
			            restore
				    }
				}
					
					
				/// Loop over frames (2a = within, 2b=between)
			    frame change wrb`i'`e'p_`pc'
			    est restore gsem`i' 
			  
			    /// Generate estimates matrix (p1_1-9m and p2)
	            mat b = J(1,18,.)
			  
			    forvalues p = 1/9 {
					
					local pplus = `p'+9
			  	
			        mat b[1,`p'] = p1_`p'm[1]
					
					mat b[1,`pplus'] = p2_`p'm[1]
			    }
					
			    /// Generate Jacobian matrix
				local paramno = e(k)
				local half = `paramno'/2
				local half1 = `half'-2
				local half2 = `half1' + 1
				di `half1'
				di `half2'
				
			    mat J = J(18,`paramno',.)
			  
			    forvalues p = 1/9 {
					
					local pplus = `p'+9
			  
			        forvalues k = 1/`half1'{
						
			            mat J[`p',`k'] = d1_`p'm_`k'[1]
				    }
					
					forvalues k = `half2'/`paramno' {
						
						mat J[`pplus',`k'] = d2_`p'm_`k'[1]
					}
				}
                
				*| Set missing rows/colums to 0 (exactly half are missing because matrix is split between gsem equations)
			    forvalues k = 1/`=rowsof(J)' {
                    forvalues k2 = 1/`=colsof(J)' {
                        if missing(J[`k', `k2']) {
                            matrix J[`k', `k2'] = 0
						}
					}
				}
 		
			    /// Generate variance-covariance matrix
	            mat V = J*e(V)*J'
				
				mat list V
				  	
			    /// Macro for results matrix row/column names
	            local names " "
				forvalues m = 1/2 {
                    forvalues p = 1/9 {
                        local names "`names' m`m'#`p'._at"
					}
				}
				
				di "`names'"
                
			    mat colnames b = `names'
                mat colnames V = `names'
                mat rownames V = `names'
	            

	
	            /// Post results matrixes to e (for feeding to nlcom)
                epost_bv b V
				
				
			    /// Relative % change
				nlcom (rpc1_1: ((_b[1._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_2: ((_b[2._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_3: ((_b[3._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_4: ((_b[4._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_5: ((_b[5._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_6: ((_b[6._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_7: ((_b[7._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_8: ((_b[8._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc1_9: ((_b[9._at#c.m1]/_b[5._at#c.m1])-1)*100) (rpc2_1: ((_b[1._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_2: ((_b[2._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_3: ((_b[3._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_4: ((_b[4._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_5: ((_b[5._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_6: ((_b[6._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_7: ((_b[7._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_8: ((_b[8._at#c.m2]/_b[5._at#c.m2])-1)*100) (rpc2_9: ((_b[9._at#c.m2]/_b[5._at#c.m2])-1)*100), post
				 
				 
			    /// Matrix to store estimates
			    mat rpcd_wrb`i'`e'_`pc' = J(9,3,.)
			 
			    *>~< Calculate difference in % change via loop o 
			    forvalues k = 1/9 {
			 	
			         nlcom (rpcd: (_b[rpc2_`k'] - _b[rpc1_`k']))
				
				    mat rpcd_wrb`i'`e'_`pc'[`k', 1] = r(b)[1,1]
	                mat rpcd_wrb`i'`e'_`pc'[`k', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
			        mat rpcd_wrb`i'`e'_`pc'[`k', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
				}
				
				/// Convert matrix to dataset and save
				xsvmat rpcd_wrb`i'`e'_`pc', saving(rpcd_wrb`i'`e'_`pc', replace)
	            use rpcd_wrb`i'`e'_`pc', clear
	            rename rpcd_wrb`i'`e'_`pc'1 _e
	            rename rpcd_wrb`i'`e'_`pc'2 _lb
	            rename rpcd_wrb`i'`e'_`pc'3 _ub
	            gen _pc = `j'
			    gen _m = `i'
			    gen _etype = `l2'
	            egen _at = fill(-2(.5)2)
			    save rpcd_wrb`i'`e'_`pc', replace
				
				/// Loop back to between estimates
			    frame change wrb`i'_`pc'
		    }
		 
		}
		 
		frame change default
	}
}
				
	
/// Append random and REWB models, and save
local sample small_1 middle_1 major_1 world_1
foreach pc in `sample' {
    use rpcd_wrb1_`pc', clear
    append using rpcd_wrb2a_`pc'
    append using rpcd_wrb2b_`pc'
	save rpcd_`pc', replace
}

/// Append across level fo expected status, and save
use rpcd_small_1, clear
append using rpcd_middle_1
append using rpcd_major_1
append using rpcd_world_1
save rpcd_all, replace
	
	
	
**********************************************************************************
replace _at = _at - .125 if _m == 2 & _etype == 1
replace _at = _at + .125 if _m == 2 & _etype == 2



**************** Small state plot

*****************************************************************
twoway (line _e _at if _m == 1 & _etype ==  1 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  1 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  2 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(gs0)) /* connecting lines


*/ (scatter _e _at if _m == 1 & _etype ==  1 & _pc == 1, symbol(O) mfcolor(white) mlcolor(black) msize(7.25pt) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc == 1,  msymbol(S) mfcolor(white) mlcolor(gs0) msize(7.25pt) mlwidth(vthin))  (scatter _e _at if _m == 2 & _etype ==  2 & _pc == 1, msymbol(T) mfcolor(white) mlcolor(gs0)  msize(7.25pt) mlwidth(vthin)) /* marker estimates

*/ (connected _e _at if _etype == 3, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(O) mfcolor(white) mlcolor(black) msize(7.25pt) mlwidth(vthin)) (connected _e _at if _etype == 3, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(S) mfcolor(white) mlcolor(gs0) msize(7.25pt) mlwidth(vthin)) (connected _e _at if _etype == 3, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(T) mfcolor(white) mlcolor(gs0) msize(7.25pt) mlwidth(vthin)), /* invisible plot for legend

*/ yline(0, lwidth(vthin)) xtitle("S.D. units of status deficit", size(8pt)) xlabel(-2(1)2, tlcolor(black) labsize(8pt) nogextend) ytitle("Difference in % effect on Pr(MID initiation", size(8pt)) ylabel(-50(25)50, tlcolor(black) labsize(8pt) nogextend) xscale(lstyle(none) titlegap(-1) alt) yscale(lstyle(none) titlegap(-2)) plotregion(lcolor(black) margin(l=-.5 r=-.5)) subtitle("Small", box bexpand pos(6) fcolor(none) size(8pt)) /* graph options


*/  legend(label(8 "Within") label(7 "Random") label(9 "Between") order(0 "{bf:Estimator}" 8 7 9) rows(1) pos(6) ring(0) size(7.5pt) region(margin(t=1.5 b=.5))) /* 


*/ graphregion(margin(r=-1 l=-5 t=-5 b=2)) fysize(54.5) name(wrbd_small, replace)



******************* Middle power plot


twoway (line _e _at if _m == 1 & _etype ==  1 & _pc ==2, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  1 & _pc ==2, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  2 & _pc ==2, lwidth(vthin) lpattern(solid) lcolor(gs0)) /* connecting lines


*/ (scatter _e _at if _m == 1 & _etype ==  1 & _pc ==2, symbol(O) mfcolor(white) mlcolor(black) msize(7.25pt) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc ==2,  msymbol(S) mfcolor(white) mlcolor(gs0)  msize(7.25pt) mlwidth(vthin))  (scatter _e _at if _m == 2 & _etype ==  2 & _pc ==2, msymbol(T) mfcolor(white) mlcolor(gs0)  msize(7.25pt) mlwidth(vthin)), /* marker estimates

graph options */  yline(0, lwidth(vthin)) xtitle("S.D. units of status deficit", size(8pt)) xlabel(-2(1)2, tlcolor(black) labsize(8pt) nogextend) ytitle("Difference in % change in Pr(MID initiation)", orientation(rvertical) size(8pt)) ylabel(-50(25)25, labsize(8pt) tlcolor(black) nogextend) xscale(lstyle(none) titlegap(-1) alt) yscale(lstyle(none) titlegap(-2) alt) plotregion(lcolor(black) margin(l=-.5 r=-.5)) subtitle("Middle", box bexpand pos(6) fcolor(none) size(8pt))  graphregion(margin(r=-5 l=-1 t=-5 b=2)) fysize(54.5) legend(off) name(wrbd_middle, replace)


************* Major power


twoway (line _e _at if _m == 1 & _etype ==  1 & _pc ==3, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  1 & _pc ==3, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  2 & _pc ==3, lwidth(vthin) lpattern(solid) lcolor(gs0)) /* connecting lines


*/ (scatter _e _at if _m == 1 & _etype ==  1 & _pc ==3, symbol(O) mfcolor(white) mlcolor(black) msize(7.25pt) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc ==3,  msymbol(S) mfcolor(white) mlcolor(gs0) msize(7.25pt) mlwidth(vthin))  (scatter _e _at if _m == 2 & _etype ==  2 & _pc ==3, msymbol(T) mfcolor(white) mlcolor(gs0) msize(7.25pt) mlwidth(vthin)), /* marker estimates

graph options */ yline(0,lwidth(vthin)) xtitle("") xlabel(, noticks nolabel nogextend) ytitle("Difference in % effect on Pr(MID initiation", size(8pt)) ylabel(-100(50) 100, tlcolor(black) labsize(8pt) nogextend) xscale(lstyle(none) titlegap(-1)) yscale(lstyle(none) titlegap(-2)) plotregion(lcolor(black) margin(l=-.5 r=-.5)) subtitle("Major", box bexpand pos(6) fcolor(none) size(8pt)) fysize(45.5) legend(off)  graphregion(margin(r=-1 l=-6.1 t=2 b=1)) name(wrbd_major, replace)



********* World power


twoway (line _e _at if _m == 1 & _etype ==  1 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  1 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(gs0))  (line _e _at if _m == 2 & _etype ==  2 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(gs0)) /* connecting lines



*/ (scatter _e _at if _m == 1 & _etype ==  1 & _pc == 4, symbol(O) mfcolor(white) mlcolor(black) msize(7.25pt) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc == 4,  msymbol(S) mfcolor(white) mlcolor(gs0) msize(7.25pt) mlwidth(vthin))  (scatter _e _at if _m == 2 & _etype ==  2 & _pc == 4, msymbol(T) mfcolor(white) mlcolor(gs0)  msize(7.25pt) mlwidth(vthin)), /* marker estimates

graph options */  yline(0,lwidth(vthin)) xtitle("") xlabel(, noticks nolabel nogextend) ytitle("Difference in % change in Pr(MID initiation", size(8pt) orientation(rvertical)) ylabel(, tlcolor(black) labsize(8pt) nogextend) yscale(lstyle(none) titlegap(-2) alt) xscale(lstyle(none) titlegap(-1)) plotregion(lcolor(black) margin(l=-.5 r=-.5)) subtitle("World", box bexpand pos(6) fcolor(none) size(8pt)) fysize(45.5) graphregion(margin(r=-6.1 l=-1 t=2 b=1)) legend(off) name(wrbd_world, replace)

grc1leg2 wrbd_small wrbd_middle wrbd_major wrbd_world, iscale(.91) ysize(6) xsize(9) imargin(r=-2.55 l=-2.55 t=-2.05 b=-2.05) graphregion(margin(r=3.85 l=3.85 b=0)) note("{it:Note}. Plot of the difference in the within, random and between effects of status deficit on the probability of initiating an MID for models which exclude versus include peace years smoothed. Estimates are the" "difference in discrete percentage effect from the mean, fixing CINC(ln) smoothed at the expected status mean while switching status deficit 0 to 1 (and allowing peace years smoothed to vary). Random effect" "estimates - represented by circular markers - are drawn from standard RE models with status deficit, CINC(ln) smoothed and an interaction term between these effects. Estimates of the within and between" "effects - represented by square and triangular markers respectively - are drawn from REWB models with separate within and between estimates for status deficit and CINC(ln) smoothed - plus a cross-level" "interaction between the within-effect of status deficit and the between-effect of CINC(ln) smoothed.", size(7pt)) 


gr_edit .plotregion1.graph1.yaxis1.title.text = {}
gr_edit .plotregion1.graph1.yaxis1.title.text.Arrpush `" "'
// title edits

gr_edit .plotregion1.graph3.yaxis1.title.DragBy 23.5297794175093 -.1176488970875465
// title reposition

gr_edit .plotregion1.graph2.yaxis1.title.text = {}
gr_edit .plotregion1.graph2.yaxis1.title.text.Arrpush `" "'
// title edits

gr_edit .plotregion1.graph4.yaxis1.title.DragBy 22.82388603498401 -.2352977941751346
// title reposition


gr_edit .plotregion1.graph4.yaxis1.title.DragBy 0 .2352977941750835
// title reposition


gr_edit .note.DragBy 0 -4.342790860802037


