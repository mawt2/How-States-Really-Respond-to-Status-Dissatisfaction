clear all
macro drop _all
frames reset
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear


mark nonmiss 
markout nonmiss mcap_1_lg1 comstsdefpw_1_lg1
local in nonmiss == 1

egen mcapz = std(mcap_1_lg1) if `in'
egen comz = std(comstsdefpw_1_lg1) if `in'

gen mcap_1_lg1_ln = ln(mcap_1_lg1) if `in'

splinegen mcap_1_lg1_ln if `in', basis(mcaplnk) degree(3) df(8) orthog
forvalues k = 0/7 {
	label var mcaplnk_`k' "           k`k'"
	gen comzXmcaplnk_`k' = comz*mcaplnk_`k'
	label var comzXmcaplnk_`k'   "           k`k'"
}


/// Estimate models
*| WIth CINC
logit midint comz mcapz, cluster(ddyadid)
qui est store pr1

*| With CINC and interaction
logit midint comz mcapz c.comz#c.mcapz, cluster(ddyadid)
est store pr2
*| Manual interaction model allowing for smoother customization in esttab
gen comzXmcapz = comz*mcapz
logit midint comz mcapz comzXmcapz, cluster(ddyadid)
qui est store pr2b

*| With CINC(ln) smoothed
logit midint comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7, cluster(ddyadid)
qui est store pr3

*| With CINC(ln) smoothed and interaction
logit midint comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 c.comz#c.mcaplnk_0 c.comz#c.mcaplnk_1 c.comz#c.mcaplnk_2 c.comz#c.mcaplnk_3 c.comz#c.mcaplnk_4 c.comz#c.mcaplnk_5 c.comz#c.mcaplnk_6 c.comz#c.mcaplnk_7  , cluster(ddyadid) 
qui est store pr4
*| Manual interaction model allowing for smoother customization in esttab
logit midint comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 comzXmcaplnk_0 comzXmcaplnk_1 comzXmcaplnk_2 comzXmcaplnk_3 comzXmcaplnk_4 comzXmcaplnk_5 comzXmcaplnk_6 comzXmcaplnk_7, cluster(ddyadid)
est store pr4b


label var comz "Status deficit"
label var mcapz "CINC"
label var comzXmcapz "Status deficit X CINC"



//// Coefficient table 
*| Full LaTeX code available in replication file
esttab pr1 pr2b pr3 pr4b using ex2.tex, label se star(* 0.05 ** 0.01) scalars("r2_p Psuedo R2" ) mgroups("With CINC" "With CINC(ln) smoothed", pattern(1 0 1 0) prefix(\multicolumn{@span}{c}{) suffix(})   span erepeat(\cmidrule(lr){@span})) alignment(D{.}{.}{-1}) page(dcolumn) refcat(mcaplnk_0 "CINC(ln) smoothed" comzXmcaplnk_0 "Status deficit X CINC(ln) smoothed", nolabel) order(comz mcapz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 comzXmcapz comzXmcaplnk_0 comzXmcaplnk_1 comzXmcaplnk_2 comzXmcaplnk_3 comzXmcaplnk_4 comzXmcaplnk_5 comzXmcaplnk_6 comzXmcaplnk_7 ) eqlabel(none) nomtitle booktabs replace





frame copy default rpc
frame change rpc

*** Loop over expected status (sub-populations)
local j 0 
qui foreach pc of varlist small_1 middle_1 major_1 world_1 {
     frame copy rpc rpchange_`pc'   // New frame
     frame change rpchange_`pc'
     local ++ j *| // Set increment by sub-pop
     local j2 0
     *| Loop over models
     forvalues i = 1/4 {
         local ++ j2   // Set increment by model
	 est restore pr`i'   // Restore model estimates
	 mat rpc`i'_`pc' = J(21,3,.)   // Matrix to store results
         *| Predictions for non-spline models
	 if `j2' == 1 | `j2' == 2 {
             margins, subpop(if `pc' == 1) vce(unconditional) at(comz=(-2 (.2) 2)) atmeans post  
	     est store mar`i'_`pc'
	 }
	 *| Predictions for spline models
	 else {
	     tempvar avmcaplnk_0
             tempvar absmcaplnk_0
	     egen `avmcaplnk_0' = mean(mcaplnk_0) if `pc' == 1   // Sub-population mean of CINC/CINC(ln)
	     gen `absmcaplnk_0' = abs(`avmcaplnk_0' - mcaplnk_0)   // Temp var to hold sub-pop mean
	     sort `absmcaplnk_0'
	     forvalues k = 0/7 {
		 local k`k'av = mcaplnk_`k'[1]   // Macro for knot value at sub-pop mean
	     }
	     margins, subpop(if `pc' == 1) vce(unconditional) at(comz=(-2 (.2) 2) mcaplnk_0 = `k0av' mcaplnk_1 = `k1av'/*
	     */ mcaplnk_2 = `k2av' mcaplnk_3 = `k3av' mcaplnk_4 = `k4av' mcaplnk_5 = `k5av' mcaplnk_6 = `k6av' mcaplnk_7 = `k7av') post
             qui est store mar`i'_`pc'
	 }
	 *| Loop over margins estimates in .2 sd increnents of status deficit
         forvalues k = 1/21 {
             if `k' != 11 {
                 if `k' == 1 {   // 1st est. (due to different var suffix)
	             nlcom (rpc`k':(_b[`k'bn._at]/_b[11._at]-1)*100), post   // Calculate % change
		     qui mat rpc`i'_`pc'[`k', 1] = e(b)   // Store est.
	             qui mat rpc`i'_`pc'[`k', 2] = e(b) - invnorm(.975) * _se[rpc`k']   // Store lbCI
	             qui mat rpc`i'_`pc'[`k', 3] = e(b) + invnorm(.975) * _se[rpc`k']   // Store ubCI
		     qui est restore mar`i'_`pc'
	         }
	         else {   // Remaining est. (except where status deficit = 0)
	             nlcom (rpc`k':(_b[`k'._at]/_b[11._at]-1)*100), post   // Calculate % change
		     qui mat rpc`i'_`pc'[`k', 1] = e(b)   // Store est.
		     qui mat rpc`i'_`pc'[`k', 2] = e(b) - invnorm(.975) * _se[rpc`k']   // Store lbCI
		     qui mat rpc`i'_`pc'[`k', 3] = e(b) + invnorm(.975) * _se[rpc`k']   // Store ubCI	
		     qui est restore mar`i'_`pc'
		 }
	     }
             else if `k' == 11 {   // Mean est = 0 (since % change calculated from 0)
		 qui mat rpc`i'_`pc'[`k', 1] = 0
		 qui mat rpc`i'_`pc'[`k', 2] = 0
		 qui mat rpc`i'_`pc'[`k', 3] = 0
	     }
         }
	 
         
	    
         *** Save results matrix as dataset	
         qui preserve
         qui xsvmat rpc`i'_`pc', saving(rpcbase`i'_`pc', replace)
	 qui use rpcbase`i'_`pc', clear
	 qui egen _at = fill(-2 (.2) 2)
	 qui gen pc = `j'
	 qui gen e_type = `i'
	 qui rename rpc`i'_`pc'1 rpc_e
	 qui rename rpc`i'_`pc'2 rpc_lb
	 qui rename rpc`i'_`pc'3 rpc_ub
	 qui save rpcbase`i'_`pc', replace
	 qui restore
     } 
	
     /// Append results from each model
     qui use rpcbase1_`pc', clear
     qui append using rpcbase2_`pc'
     qui append using rpcbase3_`pc'
     qui append using rpcbase4_`pc'
     qui save rpcbase_`pc', replace
	
    qui frame change rpc
}





frame change rpc


use rpcbase_small_1, clear
append using rpcbase_middle_1
append using rpcbase_major_1
append using rpcbase_world_1
save rpcbase_all, replace



use rpcbase_all, clear


/// Increment 
replace _at = _at + .2 if pc == 4
replace _at = _at + .1 if pc == 3
replace _at = _at - 0 if pc == 2
replace _at = _at - .1 if pc == 1


/// & rpc_ub <=200 & rpc_lb > -100
* With CINC
twoway (rarea rpc_lb rpc_ub _at if pc == 4 & e_type == 1, fcolor(4%95)  lcolor(4*1.3))  (rarea rpc_lb rpc_ub _at if pc == 3 & e_type == 1, fcolor(3%95)  lcolor(3*1.3))  (rarea rpc_lb rpc_ub _at if pc == 2 & e_type == 1, fcolor(2%95)  lcolor(2*1.3)) (rarea rpc_lb rpc_ub _at if pc == 1 & e_type == 1, fcolor(personal%95)  lcolor(personal*1.3))  (line rpc_e _at if pc == 4 & e_type == 1, lcolor(4*1.3) lpattern(solid) lwidth(0.85))  (line rpc_e _at if pc == 3 & e_type == 1, lcolor(3*1.3) lpattern(solid) lwidth(0.85)) (line rpc_e _at if pc == 2 & e_type == 1, lcolor(2*1.3) lpattern(solid) lwidth(0.85))  (line rpc_e _at if pc == 1 & e_type == 1, lcolor(personal*1.3) lpattern(solid) lwidth(0.85)) ,  legend(off) yline(0) ysize(5.5) xsize(11) xtitle(" ") xlabel(, noticks nolabel nogextend) ytitle("% change in Pr(MID initiation)", size(10.5pt)) ylabel(-50(25)100, tlcolor(black) labsize(10.5pt) nogextend) yscale(range(-55 105) lstyle(none) titlegap(-1)) xscale(lstyle(none)) subtitle("With CINC",  pos(12) box bexpand fcolor(none) lwidth(thin) size(10.5pt) alignment(middle) ) plotregion(lcolor(black) margin(r=.75 b=-.25 l=.5 t=-.25)) graphregion(margin(r=2.25 t=.4 b=-8 l=-5.5)) fxsize(101) name(rpc_cinc, replace) 

* With CINC and interaction
twoway (rarea rpc_lb rpc_ub _at if pc == 3 & e_type == 2 & rpc_ub <=150, fcolor(3%95)  lcolor(3*1.3))  (rarea rpc_lb rpc_ub _at if pc == 2 & e_type == 2 & rpc_ub <=150, fcolor(2%95)  lcolor(2*1.3)) (rarea rpc_lb rpc_ub _at if pc == 1 & e_type == 2 & rpc_ub <=150, fcolor(personal%95) lcolor(personal*1.3))  (line rpc_e _at if pc == 3 & e_type == 2 & rpc_ub <=150, lcolor(3*1.3) lpattern(solid) lwidth(0.85)) (line rpc_e _at if pc == 2 & e_type == 2 & rpc_ub <=150, lcolor(2*1.3) lpattern(solid) lwidth(0.85))   (line rpc_e _at if pc == 1 & e_type == 2 & rpc_ub <=150, lcolor(personal*1.3) lpattern(solid) lwidth(0.85)),  legend(off) yline(0) ysize(5.5) xsize(11) xtitle("") xlabel(, noticks nolabel nogextend) ytitle("% change in Pr(MID intiation)", size(10.5pt)) ylabel(-50(50)150, tlcolor(black) labsize(10.5pt) nogextend) subtitle("With CINC + interaction",box bexpand pos(12) fcolor(none) lwidth(thin) size(10.5pt) alignment(middle)) yscale(range(-55 150)lstyle(none) titlegap(-1)) xscale(lstyle(none)) plotregion(lcolor(black) margin(r=.75 b=3 l=.5 t=3.5)) graphregion(margin(r=2.25 t=-3.25 b=1.75 l=-5.5)) fxsize(101)   name(rpc_cinci, replace) 


* With CINC(ln) smoothed
twoway (rarea rpc_lb rpc_ub _at if pc == 4 & e_type == 3 , fcolor(4%95)  lcolor(4*1.3))  (rarea rpc_lb rpc_ub _at if pc == 3 & e_type == 3, fcolor(3%95)  lcolor(3*1.3))  (rarea rpc_lb rpc_ub _at if pc == 2 & e_type == 3,fcolor(2%95)  lcolor(2*1.3)) (rarea rpc_lb rpc_ub _at if pc == 1 & e_type == 3, fcolor(personal%95)  lcolor(personal*1.3))  (line rpc_e _at if pc == 4 & e_type == 3, lcolor(4*1.3) lpattern(solid) lwidth(0.85))  (line rpc_e _at if pc == 3 & e_type == 3, lcolor(3*1.3) lpattern(solid) lwidth(0.85)) (line rpc_e _at if pc == 2 & e_type == 3, lcolor(2*1.3) lpattern(solid) lwidth(0.85))  (line rpc_e _at if pc == 1 & e_type == 3, lcolor(personal*1.3) lpattern(solid) lwidth(0.85)) ,  yline(0) ysize(5.5) xsize(11) xtitle(" ") xlabel(, noticks nolabel nogextend) ytitle(" ") ylabel(-50(25)100, noticks nolabel nogextend) yscale( range(-55 105) lstyle(none)) xscale(lstyle(none)) subtitle("With CINC(ln) smoothed",  pos(12) box bexpand fcolor(none) lwidth(thin) size(10.5pt) alignment(middle))  plotregion(lcolor(black) margin(r=.5 b=-.25 l=.5 t=-0.25)) graphregion(margin(r=-2 t=.4 b=-8 l=-6.5)) fxsize(99) legend(label(4 "Small") label(3 "Middle") label(2 "Major") label(1 "World") order(0 "{bf:Expected status}" 4 3 2 1 0) rows(1) pos(6) ring(0) size(10.5pt) region(margin(t=-.5 b=.5)))  name(rpc_cincln, replace) 
 


* With CINC(ln) smoothed and interaction
twoway (rarea rpc_lb rpc_ub _at if pc == 3 & e_type == 4, fcolor(3%95)  lcolor(3*1.3)) (rarea rpc_lb rpc_ub _at if pc == 2 & e_type == 4, fcolor(2%95)  lcolor(2*1.3)) (rarea rpc_lb rpc_ub _at if pc == 1 & e_type == 4, fcolor(personal%95)  lcolor(personal*1.3))  (line rpc_e _at if pc == 3 & e_type == 4, lcolor(3*1.3) lpattern(solid) lwidth(0.85)) (line rpc_e _at if pc == 2 & e_type == 4, lcolor(2*1.3) lpattern(solid) lwidth(0.85))  (line rpc_e _at if pc == 1 & e_type == 4, lcolor(personal*1.3) lpattern(solid) lwidth(0.85)) ,  ysize(5.5) xsize(11) xtitle("") xlabel(, noticks nolabel nogextend) ytitle(" ") ylabel(-50(50)150, noticks nolabel nogextend) yscale(range(-55 150) lstyle(none)) xscale(lstyle(none)) subtitle("With CINC(ln) smoothed + interaction",  pos(12) box bexpand fcolor(none) lwidth(thin) size(10.5pt) alignment(middle))  plotregion(lcolor(black) margin(r=.5 b=2.25 l=.5 t=3.175)) graphregion(margin(r=-2 t=-3.25 b=1.75 l=-6.5)) fxsize(99) legend(off)  name(rpc_cinclni, replace) 

addplot: (pci 0 -2.1 -0 2.1, lwidth(medthin) lpattern(shortdash) lcolor(black) norescaling legend(off))


frame change default


///histogram variables allows seperation by power class
twoway__histogram_gen comz if pclass_1 == 1 & inrange(comz ,-2 ,2.25 ), density width(.5)  start(-2) gen(h1 x1)
twoway__histogram_gen comz if pclass_1 == 2 & inrange(comz ,-2 ,2.25 ), density width(.5) start(-2) gen(h2 x2)
twoway__histogram_gen comz if pclass_1 == 3 & inrange(comz ,-2 ,2.25 ), density width(.5) start(-2) gen(h3 x3)
twoway__histogram_gen comz if pclass_1 == 4 & inrange(comz ,-2 ,2.25 ), density width(.5) start(-2) gen(h4 x4)

replace x1 = x1 -.4
replace x2 = x2 - .3
replace x3 = x3 - .2
replace x4 = x4 - .1

twoway (bar h1 x1, barw(.1) fcolor(personal%95) lcolor(personal*1.3)  lwidth(vthin))  (bar h2 x2, barw(.1) fcolor(2%95) lcolor(2*1.3)  lwidth(vthin))  (bar h3 x3, barw(.1) fcolor(3%95) lcolor(3*1.3)  lwidth(vthin)) (bar h4 x4, barw(.1) fcolor(4%95) lcolor(4*1.3)  lwidth(vthin)), ytitle("Density", size(10.5pt)) ylabel(0(.5)1.5, tlcolor(black) labsize(10.5pt) nogextend) xtitle("S.D. units of status deficit", size(10.5pt)) xlabel(,labcolor(black) labsize(10.5pt) tlcolor(black) nogextend) xscale(alt lstyle(none) titlegap(-1))  yscale(reverse lstyle(none) titlegap(-1.6)) plotregion(lcolor(black)  margin(r=.375 t=-2 l=-7 b=-1)) graphregion(margin(r=2.25 l=-4.35 b=2.25 t=-2.5)) fysize(22.5) fxsize(100) legend(off)  name(defhist1, replace)

twoway (bar h1 x1, barw(.1) fcolor(personal%95) lcolor(personal*1.3)  lwidth(vthin))  (bar h2 x2, barw(.1) fcolor(2%95) lcolor(2*1.3)  lwidth(vthin))  (bar h3 x3, barw(.1) fcolor(3%95) lcolor(3*1.3)  lwidth(vthin)) (bar h4 x4, barw(.1) fcolor(4%95) lcolor(4*1.3)  lwidth(vthin))  , ytitle(" ") ylabel(0(.5)1.5, noticks nolabel nogextend) xtitle("S.D. units of status deficit", size(10.5pt)) xlabel(,labcolor(black) labsize(10.5pt) tlcolor(black) nogextend) xscale(alt lstyle(none) titlegap(-1))  yscale(reverse lstyle(none) titlegap(-1.6)) plotregion(lcolor(black) margin(r=.375 t=-2 l=-7 b=-1)) graphregion(margin(r=1.5 l=-4.525 b=2.25 t=-2.5)) fysize(22.5) fxsize(99)  legend(off) name(defhist2, replace)


grc1leg2 defhist1 defhist2 rpc_cinc rpc_cincln rpc_cinci rpc_cinclni, rows(3) xsize(12) ysize(8) imargin(l=-4 r=-2.28 t=-1.35 b=-1.35) graphregion(margin(r=1.5 l=3.85 t=-1 b=-2)) iscale(.825) legendfrom(rpc_cincln) labsize(10pt) note("{it:Note}. Plot of the substantive effect of status deficit on the probability of initiating an MID for models with CINC versus CINC(ln) smoothed. Estimates represent relative risk (RR) from the mean, fixing" "CINC/CINC(ln) smoothed at the expected status mean while switching status deficit from -2 to 2 in increments of .2 S.D.. Estimates for models with CINC and CINC(ln) smoothed are plotted in the left and right" "panels respectively, while corresponding middle/bottom panels plot estimates from models which omit/include an interaction term between status deficit and CINC/CINC(ln) smoothed. Both top panels plot" "histograms of status deficit, based on probability density estimates within levels of expected status. Mean RR estimates are represented by solid lines and bounded by confidence interval areas at the 95%" "level. Estimates gradate in color across levels of expected status. A 'zoomed-out' inset plot of RR estimates for world powers is included in both bottom panels so as to prevent visual compression.", just(left) span size(9.5pt)) name(rpc_all, replace)

/// Reposition axis title + note
gr_edit .plotregion1.graph1.yaxis1.title.DragBy 0 -1.05

gr_edit .note.DragBy -.705893382525275 -4.353009192239214
gr_edit .note.DragBy .2352977941750914 0



frame change rpc


/// Add inset plot axes 
addplot 5:  (scatteri 65 -1.9 165 -1.9 165 0 65 0 65 -1.9 , recast(line) lpattern(solid) lcolor(black) norescaling) 
gr_edit .plotregion1.graph5.plotregion1.plot7.style.editstyle line(width(thin)) editcopy

addplot 6:  (scatteri 66.5 -1.9 163 -1.9 163 -.15 66.5 -.15 66.5 -1.9 , recast(line) lpattern(solid) lcolor(black) norescaling) 

/// Plot inset markers (non-initiation dyads) on hidden 2nd Y/X-axes
addplot 5: (rarea rpc_lb rpc_ub _at if pc == 4 & e_type == 2 , fcolor(4%95)  lcolor(4*1.3) xaxis(2) yaxis(2) ylabel(-1500 800, axis(2))  xlabel(-2.5 7, axis(2)) xscale(axis(2) off) yscale(axis(2) off)) (line rpc_e _at if pc == 4 & e_type == 2, lcolor(4*1.3) lpattern(solid) lwidth(0.85) xaxis(2) yaxis(2) ylabel(-1500 800, axis(2)) norescaling legend(off)) 

addplot 6: (rarea rpc_lb rpc_ub _at if pc == 4 & e_type == 4 , fcolor(4%95)  lcolor(4*1.3) xaxis(3) yaxis(2) ylabel(-1500 800, axis(2))  xlabel(-2.5 7.75, axis(3)) xscale(axis(3) off) yscale(axis(2) off)) (line rpc_e _at if pc == 4 & e_type == 4, lcolor(4*1.3) lpattern(solid) lwidth(0.85) xaxis(3) yaxis(2) ylabel(-1500 800, axis(2)) xlabel(-2.5 7.75, axis(3)) xscale(axis(3) off) norescaling legend(off)) 


/// Add inset plot axis labels
addplot 5: (pci 750 -2.23 750 -2.32, xaxis(2) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(750 -2.5 "750", size(9pt) xaxis(2) yaxis(2) just(left))) (pci 500 -2.23 500 -2.32, xaxis(2) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(500 -2.5 "500", size(9pt) xaxis(2) yaxis(2) just(left))) (pci 250 -2.23 250 -2.32, xaxis(2) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(250 -2.5 "250", size(9pt) xaxis(2) yaxis(2) just(left))) (pci 0 -2.23 0 -2.32, xaxis(2) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(0 -2.5 "0", size(9pt) xaxis(2) yaxis(2) just(left))) (pci 0 -2.15  0 2.25, xaxis(2) yaxis(2) lwidth(0.1) lpattern(shortdash) lcolor(black) norescaling)

addplot 6: (pci 750 -2.2 750 -2.28875, xaxis(3) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(750 -2.475 "750", size(9pt) xaxis(3) yaxis(2) just(left))) (pci 500 -2.2 500 -2.28875, xaxis(3) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(500 -2.475 "500", size(9pt) xaxis(3) yaxis(2) just(left))) (pci 250 -2.2 250 -2.28875, xaxis(3) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(250 -2.475 "250", size(9pt) xaxis(3) yaxis(2) just(left))) (pci 0 -2.2 0 -2.28875, xaxis(3) yaxis(2) lpattern(solid) lwidth(thin) lcolor(black) text(0 -2.475 "0", size(9pt) xaxis(3) yaxis(2) just(left))) (pci 0 -2.15  0 2.25, xaxis(3) yaxis(2) lwidth(0.1) lpattern(shortdash) lcolor(black) norescaling legend(off))










 



