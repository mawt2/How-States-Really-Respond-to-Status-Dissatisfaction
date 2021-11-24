clear all
macro drop _all
frames reset
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear


mark nonmiss 
markout nonmiss mcap_1_lg1 comstsdefpw_1_lg1
local in nonmiss == 1



**************************************** Transformations ***********************************************
egen mcapz = std(mcap_1_lg1) if `in'
egen comz = std(comstsdefpw_1_lg1) if `in'

gen mcap_1_lg1_ln = ln(mcap_1_lg1) if nonmiss == 1

/// Generate splines
splinegen mcap_1_lg1_ln if `in', basis(mcaplnk) degree(3) df(8) orthog

************************************** Residual differences ************************************

/// Estimate model with raw CINC
logit midint comz mcapz

/// Predictions 
predict pr_1, pr

/// Estimate model with CINC(ln) smoothed
logit midint comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7 

/// Predictions 
predict pr_2, pr
 
/// Manually calculate deviance residual 
*| Manual method calculates residual by observation, whereas the residual function of - predict - calculates residuals by covariate pattern
*| By-observation residuals thus clearly distinguish MID dyads from non-MID dyads, as seen in figure 2 plots.
gen double dres_1 =sign(`e(depvar)' - pr_1)*sqrt(abs(2*(`e(depvar)'*ln(pr_1)+(1-`e(depvar)')*ln(1-pr_1))))
gen double dres_2 =sign(`e(depvar)' - pr_2)*sqrt(abs(2*(`e(depvar)'*ln(pr_2)+(1-`e(depvar)')*ln(1-pr_2))))

/// Residual difference
gen dresdif = dres_2-dres_1

/// Different group for MID dyads
recode pclass_1 (1=5) (2=6) (3=7) (4=8) if midint == 1

/// Further separation for example dyads
replace pclass_1 = 9 if ddyadid == 110115 & year == 1977
///replace pclass_1 = 10 if ddyadid == 345310 & year == 1991
///replace pclass_1 = 10 if ddyadid == 771750 & year == 1976
/// this is the one
replace pclass_1 = 10 if ddyadid == 345310 & year == 1991
replace pclass_1 = 11 if ddyadid == 260365 & year == 1980
replace pclass_1 = 12 if ddyadid == 2710 & year == 1957

/// Marker labels for example dyads
gen str exmlab = " "
replace exmlab = "GUY (1977)" if pclass_1 == 9
replace exmlab = "YUG (1991)" if pclass_1 == 10
replace exmlab = "GFR (1980)" if pclass_1 == 11
replace exmlab = "USA (1957)" if pclass_1 == 12
//gen exmlab = "{it:" + exmlab + "}"


/// SLight adjustment of marker position
replace dresdif = dresdif - .0125 if pclass_1 == 10
replace dresdif = dresdif + .02125 if pclass_1 == 11
replace dresdif = dresdif + .005 if pclass_1 == 12

/// Plot
sepscatter dresdif mcaplnk_0 if mcaplnk_0 > -3 & pclass_1 < 9, separate(pclass_1) msymbol(Oh Oh Oh Oh X X X X) msize(medsmall medsmall medsmall medsmall medsmall medsmall medsmall medsmall) mcolor(gs1%5 gs5%5*1.2 gs9%5*1.1 gs13%5 gs1%92 gs5%92*1.2 gs9%92*1.1 gs13%92) jitter(.1 .1 .1 .1 1 1 1 1) xtitle("CINC(ln) smoothed", size(14pt)) xlabel(-3(1)3, tlcolor(black) labsize(14pt) nogextend) ytitle("Difference in deviance residual", size(14pt)) ylabel(, tlcolor(black) labsize(14pt) nogextend) plotregion(lcolor(black)) xscale(alt)  yscale(lstyle(none)) yscale(titlegap(-1)) xscale(lstyle(none) titlegap(-1)) plotregion(lcolor(black) margin(r=2.5 t=-1 l=2.5)) graphregion(margin(r=-1 l=-5.45 t=-4 b=5.75)) legend(off) name(resdif,replace) 

/// Add example dyads to plot
addplot: (scatter dresdif mcaplnk_0 if pclass_1 == 9, msymbol(X) msize(vlarge) mcolor(gs1) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabpos(9) mlabgap(.3)) (scatter dresdif mcaplnk_0 if pclass_1 == 10, msymbol(X) msize(vlarge) mcolor(gs5*1.2) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabpos(3) mlabgap(.3)) (scatter dresdif mcaplnk_0 if pclass_1 == 11, msymbol(X) msize(vlarge) mcolor(gs9*1.1) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabsize(14pt) mlabpos(3) mlabgap(.3)) (scatter dresdif mcaplnk_0 if pclass_1 == 12, msymbol(X) msize(vlarge) mcolor(gs13*1.05) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabpos(9) mlabgap(.3) norescaling legend(off))


************************** Change in influence *******************************

/// Estimate model with raw CINC
logit midint comz mcapz

/// Standard error of coefficients for standardized DFEBTAs (DEFBETASs)
local defse = _se[comz]
local mcapse = _se[mcapz]

/// DFEBTA measure of influence
ldfbeta comz mcapz

/// Standardized DFBETAs
gen DFZdef1 = DFcomz/`defse'
gen DFZmcap1 = DFmcapz/`mcapse'
drop DFcomz DFmcapz

/// Estimate model with CINC(ln) smoothed
logit midint comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7
local sedef = _se[comz]
forvalues i = 0/7 {
	local se`i' = _se[mcaplnk_`i']
}

/// DFEBTA measure of influence
ldfbeta comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7

/// Standardized DFBETAs
gen DFZdef2 = DFcomz/`sedef'

forvalues i = 0/7 {
	tempname DFZ`i'
	
	if `i' != 0 {
	    gen `DFZ`i'' = DF`i'/`se`i''
	}
	else {
		gen `DFZ`i'' = DFmcapln/`se`i''
	}
}

/// DFEBTA mean across CINC(ln) splines
gen DFZmcap2 = (`DFZ0'+ `DFZ1' + `DFZ2' + `DFZ3' + `DFZ4' + `DFZ5' + `DFZ6' + `DFZ7' ) / 8

/// Infuence differences
gen DFZdif_def = DFZdef2 - DFZdef1
gen DFZdif_mcap = DFZmcap2 - DFZmcap1

/// Slight adjustment of markers position
replace DFZdif_def = DFZdif_def - .002 if pclass_1 == 9
replace DFZdif_mcap = DFZdif_mcap + .005 if pclass_1 == 10
replace DFZdif_mcap = DFZdif_mcap - .00475 if pclass_1 == 11
replace DFZdif_def = DFZdif_def - .0025 if pclass_1 == 11
replace DFZdif_mcap = DFZdif_mcap - .001 if pclass_1 == 12



/// Plot
twoway (scatter DFZdif_def DFZdif_mcap if pclass_1 == 1, msymbol(Oh) msize(medsmall) mcolor(gs1%5) jitter(.1)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 2, msymbol(Oh) msize(medsmall) mcolor(gs5%5*1.2) jitter(.1)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 3, msymbol(Oh) msize(medsmall) mcolor(gs9%5*1.1) jitter(.1)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 4, msymbol(Oh) msize(medsmall) mcolor(gs13%5)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 5, msymbol(X) msize(medsmall) mcolor(gs1%95)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 6, msymbol(X) msize(medsmall) mcolor(gs5%95*1.2) jitter(1))  (scatter DFZdif_def DFZdif_mcap if pclass_1 == 7, msymbol(X) msize(medsmall) mcolor(gs9%95*1.1) jitter(1)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 8, msymbol(X) msize(medsmall) mcolor(gs13%95*1.05)  jitter(1)) /* Main plot

*/  (scatter DFZdif_def DFZdif_mcap if pclass_1 == 9, msymbol(X) msize(vlarge) mcolor(gs1) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabpos(9) mlabgap(.3)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 10, msymbol(X) msize(vlarge) mcolor(gs5*1.2) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabpos(3) mlabgap(.3)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 11, msymbol(X) msize(vlarge) mcolor(gs9*1.1) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabsize(14pt) mlabpos(9) mlabgap(.3)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 12, msymbol(X) msize(vlarge) mcolor(gs13*1.05) mlwidth(thick) mlab(exmlab) mlabsize(14pt) mlabpos(9) mlabgap(.3)) /* Add example dyads to plot

*/ (scatter DFZdif_def DFZdif_mcap if nonmiss == 0, msymbol(X) msize(medlarge) mcolor(gs1) mlwidth(thick)) (scatter DFZdif_def DFZdif_mcap if nonmiss == 0, msymbol(X) msize(medlarge) mcolor(gs5*1.2) mlwidth(thick)) (scatter DFZdif_def DFZdif_mcap if nonmiss == 0, msymbol(X) msize(medlarge) mcolor(gs9*1.1) mlwidth(thick)) (scatter DFZdif_def DFZdif_mcap if nonmiss == 0, msymbol(X) msize(medlarge) mcolor(gs13) mlwidth(thick)) (scatter DFZdif_def DFZdif_mcap if nonmiss == 0, msymbol(oh) msize(medlarge) mcolor(gs1))  (scatter DFZdif_def DFZdif_mcap if nonmiss == 0, msymbol(X) msize(medlarge) mcolor(gs1) mlwidth(medium)) /*  Invisible plot to control size of legend markers 

*/ ,xline(0) yline(0) xtitle("Change in influence on capabilities coefficient", size(14pt)) xlabel(-.15(.05).05, tlcolor(black) labsize(14pt) nogextend) ytitle("Change in influence on status deficit coefficient", size(14pt)) ylabel(, tlcolor(black) labsize(14pt) nogextend) yscale(lstyle(none) titlegap(-2)) xscale(lstyle(none) titlegap(-.1)) plotregion(lcolor(black) margin(r=2.5 t=-1 l=2.5)) graphregion(margin(r=-1 l=-5.6 t=1.25 b=-2)) legend(label(13 "Small") label(14 "Middle") label(15 "Major") label(16 "World") label( 17 "No MID initiation") label(18 "MID initiation") order(0 "{bf:Expected status}" 13 14 15 16 0 "{bf:Dyad}" 17 18) rows(1) pos(6) ring(0) size(12.5pt) region(margin(t=-.5 b=.5))) name(influd, replace) // Plot options



************************ Combined residual/influence difference plot ************************

/// Combine graphs, with legend and note
grc1leg2 resdif influd, rows(2) ysize(10) xsize(14) imargin(b=-3.555 t=-3.555) legendfrom(influd) iscale(.825) note("{it:Note}. Plot of the difference in {it:a}) residuals and {it:b}) influence for models of status deficit plus CINC versus CINC(ln) smoothed. Upper panel plots the difference in deviance residual against" "standardized values of CINC(ln) smoothed. Residual differences, and corresponding change in predicted probability, are reported for four example dyads - one for each expected status rank." "Lower panel plots change in positive influence on coefficients for status deficit and capabilities from models with CINC versus CINC(ln) smoothed. Estimates are the difference in standardized" "DFEBTA(S). More specifically, the difference in {it:a}) the DFBETAS of status deficit and {it:b}) the DFBETAS of CINC and the cross-spline mean of the DFBETAS of CINC(ln) smoothed. Both upper and" "lower panels plot markers for each observation in the multivariate sample. Markers gradate in color across levels of expected status, with crosses and circles representing the initiation and non-""initation of an MID respectively. A 'zoomed-in' inset plot of DFBETAS estimates for non-initiating dyads is included in the lower panel as an additional visual aid.", size(11.75pt) just(left) span) graphregion(margin(b=0 l=-1.75 r=-3.25)) name(combine2, replace) 

/// Move note to far left
gr_edit .note.DragBy 0 -.94118235297

/// Macros for example residual differences text
gen prdif = pr_2 - pr_1
local j 0
forvalues i = 9/12 {
	local ++ j
	su prdif if pclass_1 == `i', meanonly
	if r(mean) < 0 {
	    local prdif`j' = r(mean)*-1
	}
	else {
		local prdif`j' = r(mean)
	}
	local prdif`j': di %5.4f `prdif`j''
	
	su dresdif if pclass_1 == `i', meanonly
	if r(mean) < 0 {
		local resdif`j' = r(mean)*-1
	}
	else {
		local resdif`j' = r(mean)
	}
		
	local resdif`j': di %5.4f `resdif`j''
}

/// Add residual differences text to plot
addplot 1 :(, text(.5 1 "{bf:Residual differences by change in Pr(MID initiation)}", size(14pt) just(left))) (, text(.3975 .975 "GUY vs. SUR (1977): {&Delta} Pr = {&minus}`prdif1' {&rArr} {&Delta} {it:d} = `resdif1'" "YUG vs. HUN (1991): {&Delta} Pr = `prdif2' {&rArr} {&Delta} {it:d} = {&minus}`resdif2'" "GFR vs. RUS (1980): {&Delta} Pr = `prdif3' {&rArr} {&Delta} {it:d} = {&minus}`resdif3'" "USA vs. CHN (1957): {&Delta} Pr = {&minus}`prdif4' {&rArr} {&Delta} {it:d} = `resdif4'", size(14pt) just(left)) norescaling legend(off)) 

/// SLight adjustment to marker position
replace DFZdif_mcap = DFZdif_mcap - .00012 if pclass_1 == 3
replace DFZdif_mcap = DFZdif_mcap - .00006 if pclass_1 == 2

/// Add inset plot axes and connecting lines
addplot 2: (scatteri -.002 -.002 .002 -.002 .002 .00625 -.002 .00625 -.002 -.002, recast(line) lpattern(solid) lcolor(black)) /* Superimposed inset box
*/ (pcarrowi -.002 .00625 -.021 -.09, recast(line) lpattern(solid) lwidth(vthin) lcolor(black)) /* Connecting lines 
*/ (pci .002 -.002 -.0055 -.1385, lpattern(solid) lwidth(vthin) lcolor(black)) (scatteri -.021 -.09 -.021 -.1385 -.0055 -.1385 -.0055 -.09 -.021 -.09 , recast(line)
lpattern(solid) lcolor(black)) /* Inset plot along hidden 2nd X-axis
*/ (pci -.021 -.133 -.0055 -.133, lpattern(shortdash) lcolor(black) lwidth(thin)) (pci -.013 -.1385 -.013 -.09, lpattern(shortdash) lcolor(black) lwidth(thin) norescaling
legend(off)) /* Axis guide lines */

/// Width of axis guide lines
gr_edit .plotregion1.graph2.plotregion1.plot19.style.editstyle line(width(vthin)) editcopy 
gr_edit .plotregion1.graph2.plotregion1.plot22.style.editstyle line(width(vthin)) editcopy 

/// Plot inset markers on 2nd X-axis
addplot 2: (scatter DFZdif_def DFZdif_mcap if pclass_1 == 1 & DFZdif_mcap <=.002, msymbol(oh) msize(small) mcolor(gs1%2) xaxis(2) yaxis(2) ylabel(-.0001 .000975, axis(2)) /*
*/ xlabel(-.0007 .00725, axis(2)) yscale(axis(2) off) mlwidth(thin) jitter(.1)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 2 & DFZdif_mcap <=.002, msymbol(oh) msize(small) /*
*/ mcolor(gs5%25*1.2) xaxis(2) yaxis(2) ylabel(-.0001 .000975, axis(2)) xlabel(-.0007 .00725, axis(2)) yscale(axis(2) off) mlwidth(thin) jitter(.1)) /* 
*/ (scatter DFZdif_def DFZdif_mcap if pclass_1 == 3 & DFZdif_mcap <=.002, msymbol(oh) msize(small) mcolor(gs9%25*1.1) xaxis(2) yaxis(2) ylabel(-.0001 .000975, axis(2)) /* 
*/ xlabel(-.0007 .00725, axis(2)) yscale(axis(2) off) mlwidth(thin) jitter(.1)) (scatter DFZdif_def DFZdif_mcap if pclass_1 == 4 & DFZdif_mcap <=.002, msymbol(oh) msize(small) /* 
*/ mcolor(gs13%50) xaxis(2) yaxis(2) ylabel(-.0001 .000975, axis(2)) xlabel(-.0007 .00725, axis(2)) yscale(axis(2) off) xscale(axis(2)off) mlwidth(thin) norescaling legend(off))


