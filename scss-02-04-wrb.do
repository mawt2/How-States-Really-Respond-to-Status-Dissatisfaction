frames reset
set varabbrev off

use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear

mark nonmiss 
markout nonmiss mcap_1_lg1 comstsdefpw_1_lg1
local in nonmiss == 1

egen mcapz = std(mcap_1_lg1) if `in' 
egen defz = std(comstsdefpw_1_lg1) if `in'

gen mcap_1_lg1_ln = ln(mcap_1_lg1) if `in'
egen mcaplnz = std(mcap_1_lg1_ln) if `in'


/// Generate CINC splines
splinegen mcap_1_lg1_ln if `in', basis(mcaplnk) degree(3) df(8) orthog


/// Generate peace years splines
qui sum pceyrs
local min = r(min)
splinegen pceyrs `min' 10 25 50 100 if `in', basis(pceyrsk) degree(3) orthog


/// Generate within-between components of status deficit/CINC(ln) smoothed
qui sort ddyadid year
foreach v of varlist defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 {
    qui by ddyadid: center `v' if `in', prefix(W_) mean(B_) 
}

/// Generate cross-level interactions between within-status deficit and between-CINC
foreach v of varlist B_mcaplnk_0 B_mcaplnk_1 B_mcaplnk_2 B_mcaplnk_3 B_mcaplnk_4 B_mcaplnk_5 B_mcaplnk_6 B_mcaplnk_7 {
	qui gen W_defX`v' = W_defz*`v' if `in'
	qui by ddyadid: center W_defX`v' if `in', prefix(W_) mean(B_)
}

/// Generate between/within-level interactions between status deficit and CINC
foreach v of varlist mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 {
	qui gen defX`v' = defz*`v' if `in'
	qui by ddyadid: center defX`v' if `in', prefix(W_) mean(B_)
}

forvalues k = 0/7 {
	gen R_defXR_mcaplnk_`k' = defz*mcaplnk_`k'
	gen W_defXR_mcaplnk_`k' = W_defz*mcaplnk_`k'
	gen B_defXR_mcaplnk_`k' = B_defz*mcaplnk_`k'
}
			
*| Random	
logit midint defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 c.defz#c.mcaplnk_0 c.defz#c.mcaplnk_1 c.defz#c.mcaplnk_2 c.defz#c.mcaplnk_3 c.defz#c.mcaplnk_4 c.defz#c.mcaplnk_5 c.defz#c.mcaplnk_6 c.defz#c.mcaplnk_7 , cluster(ddyadid)
est store pr1

*| REWB 
logit midint W_defz B_defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 W_defXR_mcaplnk_0 W_defXR_mcaplnk_1 W_defXR_mcaplnk_2 W_defXR_mcaplnk_3 W_defXR_mcaplnk_4 W_defXR_mcaplnk_5 W_defXR_mcaplnk_6 W_defXR_mcaplnk_7 B_defXR_mcaplnk_0 B_defXR_mcaplnk_1 B_defXR_mcaplnk_2 B_defXR_mcaplnk_3 B_defXR_mcaplnk_4 B_defXR_mcaplnk_5 B_defXR_mcaplnk_6 B_defXR_mcaplnk_7  , cluster(ddyadid) 
local paramno2 = e(k)
est store pr2
*| Wald test
test _b[W_defz] = _b[B_defz]
scalar wald1 = r(p)

*| Random with peace years smoothed
logit midint defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 c.defz#c.mcaplnk_0 c.defz#c.mcaplnk_1 c.defz#c.mcaplnk_2 c.defz#c.mcaplnk_3 c.defz#c.mcaplnk_4 c.defz#c.mcaplnk_5 c.defz#c.mcaplnk_6 c.defz#c.mcaplnk_7 pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4, cluster(ddyadid) 
est store pr3


*| REWB with peace years smoothed 
logit midint W_defz B_defz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 W_defXR_mcaplnk_0 W_defXR_mcaplnk_1 W_defXR_mcaplnk_2 W_defXR_mcaplnk_3 W_defXR_mcaplnk_4 W_defXR_mcaplnk_5 W_defXR_mcaplnk_6 W_defXR_mcaplnk_7 B_defXR_mcaplnk_0 B_defXR_mcaplnk_1 B_defXR_mcaplnk_2 B_defXR_mcaplnk_3 B_defXR_mcaplnk_4 B_defXR_mcaplnk_5 B_defXR_mcaplnk_6 B_defXR_mcaplnk_7 pceyrsk_1 pceyrsk_2 pceyrsk_3 pceyrsk_4, cluster(ddyadid)
local paramno4 = e(k)
est store pr4
*| Wald test
test _b[W_defz] = _b[B_defz]
scalar wald2 = r(p)

/// Program to post results matrixes to e (in order to feed to nlcom for calculation of relative % change)
capture program drop epost_bv
program define epost_bv, eclass
args b V
ereturn post `b' `V'
end

keep if `in'

*>~< Loop over models
local l 0
forvalues i = 1/4 {	
	local ++ l
	
	frame copy default wrb`i'
	frame change wrb`i'
		
	/// Tempfile for saving predictions
	tempfile wrb`i'
	
	*<~> Loop for random effects
   	if `l' == 1 | `l' == 3 {
		
		// Matrix to store % effect (discrete change) estimates
	    mat rpc_wrb`i' = J(13,3,.)
		
	    /// Loop over CINC
		local j 0
		forvalues k = -2(.5)2.5 {
			
		    local ++ j
			
		    // Macros for the (approximate) CINC value across intervals of CINC 
			tempvar diff_`j'
		    qui gen `diff_`j'' = abs(mcaplnk_0 - `k')
		    qui sort `diff_`j''
		    forvalues k2 = 0/7 {
			    qui local k`k2'_`j' = mcaplnk_`k2'[1]	
		    }
			
		    sort ddyadid year
			
			/// Predictions
		    est restore pr`i'
			margins, at(defz =(0 1) mcaplnk_0 = `k0_`j'' mcaplnk_1 = `k1_`j'' mcaplnk_2 = `k2_`j'' mcaplnk_3 = `k3_`j'' mcaplnk_4 = `k4_`j''  mcaplnk_5 = `k5_`j'' mcaplnk_6 = `k6_`j'' mcaplnk_7 = `k7_`j'') post coeflegend
			 
			 
			/// Relative % effect (discrete change from 0 to 1 S.D.) 
			nlcom (rpc: ((_b[2._at]/ _b[1bn._at])-1)*100)
			
			mat rpc_wrb`i'[`j', 1] = r(b)[1,1]
	        mat rpc_wrb`i'[`j', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
			mat rpc_wrb`i'[`j', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
			
		}
		
		/// Convert matrix to dataset and save
		xsvmat rpc_wrb`i', saving(rpc_wrb`i'_new, replace)
	    use rpc_wrb`i'_new, clear
		rename rpc_wrb`i'1 _e
		rename rpc_wrb`i'2 _lb
		rename rpc_wrb`i'3 _ub
	    gen _m = `i'
	    gen _etype = 1
		egen _at = fill(-2(.5)2.5)
		save rpc_wrb`i'_new, replace
	}
		 
    else if `l' == 2 | `l' == 4 {
		
	    *>~< Loop for within/between predictions
		local levs a b
	    local l2 0 
		foreach e in `levs' {
	        local ++ l2
			
			// Create new frame for storing predictions/derivatives
			frame create wrb`i'`e'p
			frame change wrb`i'`e'p
		    set obs 1
		    gen mtag = 1
			frame change wrb`i'
			
			/// Set status deficit at mean (remains at mean for between estimation since the same frame is used as a base)
			if `l2' == 1  {
				
			    /// Re-set status deficit to global mean (that is, 0)
	            qui replace defz = 0
		
	             /// Update within and between components of status deficit at mean
		         *| Within = 0 and between = 0
		         qui by ddyadid: center defz, prefix(Wm_) mean(Bm_)
		         qui replace W_defz = Wm_defz
		         qui replace B_defz = Bm_defz
				
			} 
			 
		     /// Loop over CINC
		     local j 0
		     forvalues k = -2(.5)2.5 {
			
		         local ++ j
				 
				 frame copy wrb`i' wrb`i'`e'_`j'
			     frame change wrb`i'`e'_`j'
				 	
		         // Set CINC 
			     tempvar diff_`j'
		         qui gen `diff_`j'' = abs(mcaplnk_0 - `k')
		         qui sort `diff_`j''
		         forvalues k2 = 0/7 {
			         qui local k`k2'_`j' = mcaplnk_`k2'[1]
					 replace mcaplnk_`k2' = `k`k2'_`j''
		         }
				
		         sort ddyadid year
				
				 
				 *>~< Loop for within-effect predictions
			     if `l2' == 1 {
		 
		             *>~< Discrete change in status deficit
		             local f 0
		             forvalues p = 0/1 {
		                 local ++ f
						 
						 /// Set restoration point
						 preserve
						 
						 /// Reset status deficit for discrete change (otherwise simply update CINC interaction with status deficit at mean)
						 if `f' != 1 {
						 
					         /// Tag first observation 
				             qui sort ddyadid year
	                         qui gen obtag = _n 
	                         su ddyadid if obtag == 1
				  	
				             /// Set status deficit at +1 S.D. (1st ob only for within effect)
		                     qui replace defz = `p' if obtag == 1
				
			                 /// Re-generate within/between status deficit components at + 1.s.d 
	                         qui by ddyadid: center defz, prefix(Wf_) mean(Bf_)
			  							
				             /// Update core within/between components (within = +1S.D., between = approx mean) 
			  	             qui replace W_defz = Wf_defz[1]
		                     qui replace B_defz = Bf_defz[1]
				             qui su W_defz
				             di "`r(mean)' is within deficit deviation for def= `p'"
				             qui su B_defz
					         di "`r(mean)' is between deficit mean"
						 }
						 
					     /// Update interactions
					     forvalues k2 = 0/7 {
			                 replace W_defXR_mcaplnk_`k2' = W_defz*mcaplnk_`k2'
			                 replace B_defXR_mcaplnk_`k2' = B_defz*mcaplnk_`k2'
		                 }
					  
				         /// Generate within effect predictions with partial derivatives
			             est restore pr`i'
	                     qui predictnl double p`j'_`p' = predict(pr), g(d`j'_`p'_) 
			             qui su p`j'_`p'
						 di "`r(mean)' is prediction at source"
				   
				         /// Generate partial derivatives if missing
	                     *| Program checks if derivative variable exists, and generates derivatives as zero if missing
			   
			             forvalues d = 1/`paramno`i'' {
            
			                 capture confirm var d`j'_`p'_`d'
            
			                 if c(rc) == 111 { 
                
				                 qui gen d`j'_`p'_`d' = 0
				             }
				
				             else {
				                 continue
				             }
			             }
				  
				         *| Mean prediction/partial derivative across peace years
			             egen p`j'_`p'm = mean(p`j'_`p')
			             forvalues d = 1/`paramno`i'' {
			                 egen d`j'_`p'm_`d' = mean(d`j'_`p'_`d')
					     } 
					  
					     /// Variable for frame merge (to store predictions)
					     keep in 1
				         gen mtag = 1
						 
						 frames
				        
				         /// Store predictions/derivatives in separate frame (one for each CINC value)
			   	         frame change wrb`i'`e'p
						 if `f' == 1 {
						     frlink 1:1 mtag, frame(wrb`i'`e'_`j')
						 }
				         
						 frget p`j'_`p'm-d`j'_`p'm_`paramno`i'', from(wrb`i'`e'_`j')
						 su p`j'_`p'm
						 di "`r(mean)' is prediction in 2nd frame"
					     frame change wrb`i'`e'_`j'
						 
						 restore
					 }
				 }
			
				 *>~< Loop for between-effect predictions
				 else if `l2' == 2 {
					 	
				     *>~< Discrete change for status deficit
		             local f 0
		             forvalues p = 0/1 {
		                 local ++ f
				  
		                 /// Set restoration point 
			             preserve
						 
						 /// Reset status deficit for discrete change (otherwise simply update CINC interaction with status deficit at mean)
						 if `f' != 1 {
				  	
				             /// Set status deficit at +1S.D. (for all obs for between effect)
		                     qui replace defz = `p' 
				     
				             /// Re-generate within/between components of status deficit
	                         qui by ddyadid: center defz, prefix(Wf_) mean(Bf_)
				 
				             /// Update core within/between components 
			  	             qui replace W_defz = Wf_defz
		                     qui replace B_defz = Bf_defz
					         qui su W_defz
					         di "`r(mean)' is within deficit mean"
				             qui su B_defz
					         di "`r(mean)' is between deficit deviation for def = `p'"
						 }
					
					     /// Update interactions
					     forvalues k2 = 0/7 {
			                 replace W_defXR_mcaplnk_`k2' = W_defz*mcaplnk_`k2'
			                 replace B_defXR_mcaplnk_`k2' = B_defz*mcaplnk_`k2'
		                 }
				    
				         /// Generate between effect predictions with partial derivatives
			             qui est restore pr`i'
	                     qui predictnl double p`j'_`p' = predict(pr), g(d`j'_`p'_) 
			             su p`j'_`p'
				   
				         /// Generate partial derivatives if missing
	                     *| Program checks if derivative variable exists, and generates derivatives as zero if missing
			   
			             forvalues d = 1/`paramno`i'' {
            
			                 capture confirm var d`j'_`p'_`d'
            
			                 if c(rc) == 111 { 
                
				                 qui gen d`j'_`p'_`d' = 0
				             }
				
				             else {
				                 continue
				             }
			             }
				  
				         *| Mean prediction/partial derivative across peace years
			             egen p`j'_`p'm = mean(p`j'_`p')
			             forvalues d = 1/`paramno`i'' {
			                 egen d`j'_`p'm_`d' = mean(d`j'_`p'_`d')
					     }  
					  
					     /// Variable for frame merge (to store predictions)
					     keep in 1
				         gen mtag = 1
				        
				         /// Store predictions/derivatives in separate frame
			   	         frame change wrb`i'`e'p
						 if `f' == 1 {
						     frlink 1:1 mtag, frame(wrb`i'`e'_`j')
						 }
				         frget p`j'_`p'm-d`j'_`p'm_`paramno`i'', from(wrb`i'`e'_`j')
					     frame change wrb`i'`e'_`j'
						
			             restore
					 }
				 }
				 
			     frame change wrb`i'
				 frame drop wrb`i'`e'_`j'
				 frames list
			 }
			 
				       
			 /// Change to frame (wrb2a_pc wrb2b_pc wrb4a_pc wrb4b_pc) and create estimates/jacobian matrix
			 frame change wrb`i'`e'p
			 est restore pr`i'
			  
			 /// Generate estimates matrix
	         mat b = J(1,20,.)
			  
			 forvalues p = 1/10 {
					
				local pplus = `p'+10
			            
				mat b[1,`p'] = p`p'_0m[1]
						
				mat b[1,`pplus'] = p`p'_1m[1]	
			 }
			  
			 mat list b
			 
			
			 /// Generate Jacobian matrix
			 mat J = J(20,`paramno`i'',.)
			  
			 forvalues p = 1/10 {
					
			     local pplus = `p'+10
					
			     forvalues d = 1/`paramno`i'' {
				  	
			         mat J[`p',`d'] = d`p'_0m_`d'[1]
				     mat J[`pplus',`d'] = d`p'_1m_`d'[1]
				 }
			 }
			    
			 /// Generate variance-covariance matrix
	         mat V = J*e(V)*J'
				  	
			 /// Macro for results matrix row/column names
	         local names " "
             forvalues p = 1/10 {
                 local names "`names' at0#at`p'"
             }   
			 
			 forvalues p = 1/10 {
			 local names "`names' at1#at`p'"
			 }
			 
			 di "`names'"
			   
			 mat colnames b = `names'
             mat colnames V = `names'
             mat rownames V = `names'
	
	
	         /// Post results matrixes to e (for feeding to nlcom)
             epost_bv b V
			 
		
			 /// Matrix to store estimates
			 mat rpc_wrb`i'`e'= J(10,3,.)
			 mat apc_wrb`i'`e'= J(10,3,.)
			 
			  local j 0
			 
			 /// Relative % effect (discrete change from 0 to 1 S.D.) 
			 forvalues p =1/10 {
			 	
				local ++ j
			 	
			     nlcom (rpc: ((_b[c.at1#c.at`p']/ _b[c.at0#c.at`p'])-1)*100)
			
			     mat rpc_wrb`i'`e'[`j', 1] = r(b)[1,1]
	             mat rpc_wrb`i'`e'[`j', 2] = r(b)[1,1] - invnorm(.975) * sqrt(r(V)[1,1])
			     mat rpc_wrb`i'`e'[`j', 3] = r(b)[1,1] + invnorm(.975) * sqrt(r(V)[1,1])
				
			 }
			 /// Convert matrix to dataset and save
			 xsvmat rpc_wrb`i'`e', saving(rpc_wrb`i'`e'_new, replace)
			 use rpc_wrb`i'`e'_new, clear
			 rename rpc_wrb`i'`e'1 _e
		     rename rpc_wrb`i'`e'2 _lb
		     rename rpc_wrb`i'`e'3 _ub
			 gen _m = `i'
			 gen _etype = `l2'
			 egen _at = fill(-2(.5)2.5)
		     save rpc_wrb`i'`e'_new, replace
		
			 frame change wrb`i'
			 
		}
		
	}

	frame change default
	
}  










********************************************

		
use rpc_wrb1_new, clear
append using rpc_wrb2a_new	
append using rpc_wrb2b_new	
append using rpc_wrb3_new	
append using rpc_wrb4a_new	
append using rpc_wrb4b_new	
save rpc_wrball_new, replace

drop if _e == .
drop if _at == 2.5

gen _pc = .
replace _pc = 1 if _at < 0.5
replace _pc = 2 if _at <= 1 & _pc == .
replace _pc = 3 if _at <= 1.5 & _pc == .
replace _pc = 4 if _pc == .



gen change = 1 if _pc!=_pc[_n+1] & _etype==_etype[_n+1] & _m == _m[_n+1]


expand 2 if change == 1, gen(tag)
sort _m _etype _at tag
replace _pc = _pc[_n+1] if tag == 1

replace _at = _at - .115 if (_m == 2 | _m == 4) & _etype == 1
replace _at = _at + .115 if (_m == 2 | _m == 4) & _etype == 2



twoway (line _e _at if _m == 1 & _etype ==  1 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 1 & _etype ==  1 & _pc == 2, lwidth(vthin) lpattern(solid) lcolor(black))(line _e _at if _m == 1 & _etype ==  1 & _pc == 3, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 1 & _etype ==  1 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(black)) /*

*/ (line _e _at if _m == 2 & _etype ==  1 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 2 & _etype ==  1 & _pc == 2, lwidth(vthin) lpattern(solid) lcolor(black))(line _e _at if _m == 2 & _etype ==  1 & _pc == 3, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 2 & _etype ==  1 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(black)) /*

*/ (line _e _at if _m == 2 & _etype ==  2 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 2 & _etype ==  2 & _pc == 2, lwidth(vthin) lpattern(solid) lcolor(black))(line _e _at if _m == 2 & _etype ==  2 & _pc == 3, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 2 & _etype ==  2 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(black)) /* 

*/ (scatter _e _at if _m == 1 & _etype ==  1 & _pc == 1 & tag != 1, symbol(O) mfcolor(personal) mlcolor(black) msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 1 & _etype ==  1 & _pc == 2 & tag != 1, symbol(O) mfcolor(2) mlcolor(black) msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 1 & _etype ==  1 & _pc == 3 & tag != 1, symbol(O) mfcolor(3) mlcolor(black) msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 1 & _etype ==  1 & _pc == 4 & tag != 1, symbol(O) mfcolor(4) mlcolor(black) msize(3.45) mlwidth(vthin)) /*

*/ (scatter _e _at if _m == 2 & _etype ==  1 & _pc == 1 & tag != 1,symbol(S) mfcolor(personal) mlcolor(black) msize(3.1) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc == 2 & tag != 1,symbol(S) mfcolor(2) mlcolor(black) msize(3.1) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc == 3 & tag != 1,symbol(S) mfcolor(3) mlcolor(black) msize(3.1) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  1 & _pc == 4 & tag != 1,symbol(S) mfcolor(4) mlcolor(black) msize(3.1) mlwidth(vthin)) /*

*/ (scatter _e _at if _m == 2 & _etype ==  2 & _pc == 1 & tag != 1, symbol(T) mfcolor(personal) mlcolor(black) msize(3.75) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  2 & _pc == 2 & tag != 1, symbol(T) mfcolor(2) mlcolor(black) msize(3.75) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  2 & _pc == 3 & tag != 1, symbol(T) mfcolor(3) mlcolor(black) msize(3.75) mlwidth(vthin)) (scatter _e _at if _m == 2 & _etype ==  2 & _pc == 4 & tag != 1, symbol(T)mfcolor(4) mlcolor(black) msize(3.75) mlwidth(vthin)) /*

*/ (connected _e _at if tag == 2, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(O) mfcolor(personal) mlcolor(black) msize(medlarge) mlwidth(vthin)) (connected _e _at if tag == 2, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(O) mfcolor(2) mlcolor(black) msize(medlarge) mlwidth(vthin)) (connected _e _at if tag == 2, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(O) mfcolor(3) mlcolor(black) msize(medlarge) mlwidth(vthin)) (connected _e _at if tag == 2, lwidth(vthin) lpattern(solid) lcolor(gs0) symbol(O) mfcolor(4) mlcolor(black) msize(medlarge) mlwidth(vthin)) /*

*/ (scatter _e _at if tag == 2, symbol(Oh) msize(medlarge) color(black) mlwidth(thin)) (scatter _e _at if tag == 2, symbol(Sh) msize(medlarge) color(black) mlwidth(thin)) (scatter _e _at if tag == 2, symbol(Th) msize(medlarge) color(black) mlwidth(thin)), /*

*/ yline(0) xtitle("S.D. units of CINC(ln) smoothed", size(7.5pt)) xlabel(-2(1)2, tlcolor(black) labsize(7.5pt) nogextend) ytitle("% effect on Pr(MID initiation)", size(7.5pt)) ylabel(-100(50) 100, tlcolor(black) labsize(7.5pt) nogextend) xscale(lstyle(none) titlegap(-1) alt) yscale(lstyle(none) titlegap(-3.5)) plotregion(lcolor(black) margin(t=2.925 r=2.8 l=2.25 t=3.25))  subtitle("Without peace years smoothed", box bexpand fcolor(none) size(7.5pt) pos(6) alignment(bottom)) /*

*/  legend(label(25 "Small") label(26 "Middle") label(27 "Major") label(28 "World") label(30 "Within") label(29 "Random") label(31 "Between") order(0 "{bf:Expected status}" 25 26 27 28 0 "{bf:Estimator}" 30 29 31) rows(1) pos(6) ring(0) size(5.5pt) region(margin(t=2 b=.5))) /*


*/ graphregion(margin(t=1.25 b=-4 r=2.25 l=-5.5)) name(not, replace)





*** Connected plot
twoway (line _e _at if _m == 3 & _etype ==  1 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 3 & _etype ==  1 & _pc == 2, lwidth(vthin) /*
*/ lpattern(solid) lcolor(black))(line _e _at if _m == 3 & _etype ==  1 & _pc == 3, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 3 & _etype ==  1 /* 
*/ & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 4 & _etype ==  1 & _pc == 1, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if /*
*/ _m == 4 & _etype ==  1 & _pc == 2, lwidth(vthin) lpattern(solid) lcolor(black))(line _e _at if _m == 4 & _etype ==  1 & _pc == 3, lwidth(vthin) lpattern(solid) /*
*/ lcolor(black)) (line _e _at if _m == 4 & _etype ==  1 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 4 & _etype ==  2 & _pc == 1, /*
*/ lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 4 & _etype ==  2 & _pc == 2, lwidth(vthin) lpattern(solid) lcolor(black))(line _e _at if _m == 4 & _etype/*
*/ ==  2 & _pc == 3, lwidth(vthin) lpattern(solid) lcolor(black)) (line _e _at if _m == 4 & _etype ==  2 & _pc == 4, lwidth(vthin) lpattern(solid) lcolor(black)) /*

*/ (scatter _e _at if _m == 3 & _etype ==  1 & _pc == 1 & tag != 1, symbol(O) mfcolor(personal) mlcolor(black) msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 3 & /*
*/ _etype ==  1 & _pc == 2 & tag != 1, symbol(O) mfcolor(2) mlcolor(black) msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 3 & _etype ==  1 & _pc == 3 & tag != 1, /* 
*/ symbol(O) mfcolor(3) mlcolor(black) msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 3 & _etype ==  1 & _pc == 4 & tag != 1, symbol(O) mfcolor(4) mlcolor(black) /*
*/ msize(3.45) mlwidth(vthin)) (scatter _e _at if _m == 4 & _etype ==  1 & _pc == 1 & tag != 1,symbol(S) mfcolor(personal) mlcolor(black) msize(3.1) mlwidth(vthin)) /*
*/ (scatter _e _at if _m == 4 & _etype ==  1 & _pc == 2 & tag != 1,symbol(S) mfcolor(2) mlcolor(black) msize(3.1) mlwidth(vthin)) (scatter _e _at if _m == 4 & _etype ==  1 /*
*/ & _pc == 3 & tag != 1,symbol(S) mfcolor(3) mlcolor(black) msize(3.1) mlwidth(vthin)) (scatter _e _at if _m == 4 & _etype ==  1 & _pc == 4 & tag != 1,symbol(S) mfcolor(4) /*
*/ mlcolor(black) msize(3.1) mlwidth(vthin)) (scatter _e _at if _m == 4 & _etype ==  2 & _pc == 1 & tag != 1, symbol(T) mfcolor(personal) mlcolor(black) msize(3.75) /*
*/ mlwidth(vthin)) (scatter _e _at if _m == 4 & _etype ==  2 & _pc == 2 & tag != 1, symbol(T) mfcolor(2) mlcolor(black) msize(3.75) mlwidth(vthin)) (scatter _e _at if /*
*/ _m == 4 & _etype ==  2 & _pc == 3 & tag != 1, symbol(T) mfcolor(3) mlcolor(black) msize(3.75) mlwidth(vthin)) (scatter _e _at if _m == 4 & _etype ==  2 & _pc == 4 & /*
*/ tag != 1, symbol(T) mfcolor(4) mlcolor(black) msize(3.75) mlwidth(vthin)), legend(off)  /*

*/ subtitle("With peace years smoothed", box bexpand fcolor(none) pos(6) size(7.5pt) alignment(bottom))  yline(0) xtitle("S.D. units of CINC(ln) smoothed ", size(7.5pt))  /*
*/xlabel(-2(1)2, tlcolor(black) labsize(7.5pt) nogextend) ytitle(" ") ylabel(-100(50) 100, nolabel noticks nogextend) xscale(lstyle(none) titlegap(-1) alt) /*
yscale(lstyle(none)) plotregion(lcolor(black) margin(t=4.625 r=2.8)) graphregion(margin(t=1.15 b=-4 l=-6.5 r=-2)) name(t, replace)

*** Combine graphs
grc1leg2 not t, rows(1) ysize(3) xsize(7.5) imargin(l=-4 r=-2.25) graphregion(margin(l=3.5 b=0 t=-2.5)) legendfrom(not) 
*| Add Wald chi-squared statistics
local wald1: di %4.3f scalar(wald1)
local wald2: di %4.3f scalar(wald2)
addplot 1:(scatter _e _at if tag == 10, text(-83.5 -1 "{stSerif:Wald {it:{&chi}}{super:2}({it:{&beta}}1{it:{&chi}{subscript:it}} = {it:{&beta}}1{it:{&chi}} /*
*/ `=ustrunescape("\u0305")'{it:{sub:i}}) = `wald1'}", size(medlarge))), norescaling legend(off) /*
*/ addplot 2:(scatter _e _at if tag == 10, text(-85 -1 "{stSerif:Wald {it:{&chi}}{super:2}({it:{&beta}}1{it:{&chi}{subscript:it}} = {it:{&beta}}1{it:{&chi}} /*
*/ `=ustrunescape("\u0305")'{it:{sub:i}}) = `wald2'}", size(medlarge))), norescaling legend(off)




note("{it:Note}. Plot of the within, random and between effects of status deficit on the probability of initiating an MID for models which exclude versus include a smoothed function of peace years. Estimates represent discrete percentage effect from the mean," "fixing CINC(ln) smoothed at incremental values of .5 S.D. while switching status deficit from 0 to 1 (and allowing peace years smoothed to vary). Random effect estimates - represented by circular markers - are drawn from standard RE models with status" "deficit, CINC(ln) smoothed and an interaction term between these effects. Estimates of the within and between effects - represented by square and triangular markers respectively - are drawn from REWB models with separate within and between" "estimates for status deficit and CINC(ln) smoothed - plus a cross-level interaction between the within-effect of status deficit and the between-effect of CINC(ln) smoothed. Estimates from models which include peace years smoothed - an orthogonalised" "cubic spline of 4 knots - are plotted in the right-side panel. Both RE and REWB models estimate the random effect of peace years smoothed. {it:p}-values from a Wald {it:{&chi}}{super:2} test of equality of REWB status deficit coefficients are reported in each panel.", size(4.75pt))  name(gc, replace)

/// Reposition note
gr_edit .note.DragBy 0 -3.47
