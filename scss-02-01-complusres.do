clear all
macro drop _all
frames reset
use "Status Conflict among Small States\Data Analysis\Datasets\Derived\02-Processing\01-Dyadic\a-Base\scss-0201a-base.dta", clear


mark nonmiss 
markout nonmiss mcap_1_lg1 comstsdefpw_1_lg1
local in nonmiss == 1



**************************************** Linear ***********************************************
egen mcapz = std(mcap_1_lg1) if `in'
egen comz = std(comstsdefpw_1_lg1) if `in'


clonevar clone_def = comz

/// Estimate linear model
logit midint comz mcapz

/// Macros for goodness of fit stats
*| Coef
local coef: di%5.3f e(b)[1,1]
local coefn ""
local coefn `coef'
*|Psuedo R squared 
local r2 : di %5.3f `e(r2_p)'
local r2n""
local r2n `r2'
*|AIC
 estat ic
local aic: di%8.0f r(S)[1,5]
local aicn " "
local aicn `aic'
*| AUC
qui lroc, nograph
local auc :di%8.3f r(area)
local aucn ""
local aucn `auc'

/// Deviance residuals for full model
predict dres_a, dev

/// Partial prediction with status deficit at mean
qui su comz, meanonly
replace comz = r(mean)
predict pr_a, xb

/// Component-plus-residual
qui gen pres_a = pr_a + dres_a

/// Loop over levels of expected status (subpopulations)
local j 0
local sample small_1 middle_1 major_1 world_1
foreach pc in `sample' {
	local ++ j
	*| Temp variable names
	tempname obs`j' binno`j' 
	sort mcap_1_lg1
	*| Observation number by subpopulation
    generate `obs`j'' = _n if pr_a < . & `pc' == 1
    su `obs`j''
    local subobs = r(N)
    *| Number of bins (proportional to subpopulation size)
    local nbins`j' = floor(sqrt(`subobs'))
    noi di `nbins`j''
	local nbins`j': di %3.0f `nbins`j''
    *| Bins of (roughly) equal size 
    egen `binno`j'' = cut(`obs`j'') if pr_a < . & `pc' == 1 , group(`nbins`j'') icodes 
    *| Average CINC value by bin
    egen av_a`j' = mean(mcap_1_lg1), by(`binno`j'')
    *| Average prediction by bin 
    egen avcpr_a`j' = mean(pres_a) if `pc' == 1, by(`binno`j'')
    *| Tag binned observations
    egen tag_a`j' = tag(`binno`j'')
}


/// Plot
graph twoway (scatter avcpr_a4 av_a4 if tag_a4 == 1 & avcpr_a4 <= 0 , msize(small) msymbol(o) mcolor(gs13%75) jitter(3.5)) (scatter avcpr_a3 av_a3 if tag_a3 == 1, msize(small) msymbol(o) mcolor(gs9%75*1.1) jitter(3.5)) (scatter avcpr_a2 av_a2 if tag_a2 == 1, msize(small) msymbol(o) mcolor(gs5%75*1.2) jitter(3.5)) (scatter avcpr_a1 av_a1 if tag_a1 == 1, msize(small) msymbol(o) mcolor(gs2%75) jitter(3.5)) (lfit pres_a mcap_1_lg1, lpattern(dash) lwidth(medthick) color(plg2) range(. .)) (lowess pres_a mcap_1_lg1, bwidth(.05) lpattern(solid) lwidth(medthick) color(pll2)), ytitle("Partial prediction + residual", size(10.5pt)) ylabel(-10(2)0, tlcolor(black) labsize(10.5pt) nogextend) xtitle(" ") xlabel(0(.05).3, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none) titlegap(-2.1)) subtitle("Linear", box bexpand fcolor(none) size(10.5pt))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=1)) graphregion(margin(r=3 l=-4.75 t=.4 b=-8)) legend(off)  text(-7.55 .265 "{bf:Status deficit coef. = `coefn'}" -8.6 .2495 "AIC = `aicn'" "AUC = `aucn'" "Psuedo R{superscript:2} = `r2n'", just(left) size(10.5pt)) name(linear,replace)




********************************************************************************************************************************************



**************************************** Log ***********************************************



gen mcap_1_lg1_ln = ln(mcap_1_lg1) if nonmiss == 1
egen mcaplnz = std(mcap_1_lg1_ln) if nonmiss == 1

replace comz = clone_def

/// Estimate bivariate linear model
logit midint comz mcaplnz

/// Macros for goodness of fit stats
*| Coef
local coef: di%5.3f e(b)[1,1]
local coefn ""
local coefn `coef'
*|Psuedo R squared 
local r2 : di %5.3f `e(r2_p)'
local r2n""
local r2n `r2'
*|AIC
 estat ic
local aic: di%8.0f r(S)[1,5]
local aicn " "
local aicn `aic'
*| AUC
qui lroc, nograph
local auc :di%8.3f r(area)
local aucn ""
local aucn `auc'


/// Deviance residuals for full model
predict dres_b, dev

/// Partial prediction with status deficit at mean
su comz
replace comz = r(mean)
predict pr_b, xb

/// Component-plus-residual
qui gen pres_b = pr_b + dres_b

/// Loop over expected status (subpopulation)
local j 0
local sample small_1 middle_1 major_1 world_1
foreach pc in `sample' {
	local ++ j
	
	*| Temp variable names
	tempname obs`j' binno`j' 
	sort mcap_1_lg1
	
	*| Observation number by subpopulation
    generate `obs`j'' = _n if pr_b < . & `pc' == 1
    su `obs`j''
    local subobs = r(N)

    *| Number of bins (proportional to subpopulation size)
    local nbins`j' = floor(sqrt(`subobs'))
    noi di `nbins`j''
	local nbins`j': di %3.0f `nbins`j''

    *| Bins of (roughly) equal size 
    egen `binno`j'' = cut(`obs`j'') if pr_b < . & `pc' == 1 , group(`nbins`j'') icodes 

    *| Average CINC value by bin
    egen av_b`j' = mean(mcap_1_lg1_ln), by(`binno`j'')

    *| Average prediction by bin 
    egen avcpr_b`j' = mean(pres_b) if `pc' == 1, by(`binno`j'')

    *| Tag binned observations
    egen tag_b`j' = tag(`binno`j'')

}



graph twoway (scatter avcpr_b4 av_b4 if tag_b4 == 1 & avcpr_b4 <= 0 , msize(small) msymbol(o) mcolor(gs13%75) jitter(3.5)) (scatter avcpr_b3 av_b3 if tag_b3 == 1, msize(small) msymbol(o) mcolor(gs9%75*1.1) jitter(3.5)) (scatter avcpr_b2 av_b2 if tag_b2 == 1, msize(small) msymbol(o) mcolor(gs5%75*1.2) jitter(3.5)) (scatter avcpr_b1 av_b1 if tag_b1 == 1, msize(small) msymbol(o) mcolor(gs2%75) jitter(3.5)) (lfit pres_b mcap_1_lg1_ln, lpattern(dash) lwidth(medthick) color(plg2) range(. .)) (lowess pres_b mcap_1_lg1_ln, bwidth(.05) lpattern(solid) lwidth(medthick) color(pll2)), ytitle(" ") ylabel(-10(2)0, noticks nolabel nogextend) xtitle(" ") xlabel(-15(2.5)0 ,noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none)) subtitle("Log", box bexpand fcolor(none) size(10.5pt))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=-1.5)) graphregion(margin(r=-2.75 l=-7 t=.4 b=-8)) legend(off) text(-.595 -11.94 "{bf:Status deficit coef. = `coefn'}" -1.675 -12.635 "AIC = `aicn'" "AUC = `aucn'" "Psuedo R{superscript:2} = `r2n'", just(left) size(10.5pt)) name(log,replace)



***************************************************** Linear splines ***********************************************


replace comz = clone_def


/// Generate splines
splinegen mcap_1_lg1 if `in', basis(mcapk) degree(3) df(8) orthog

///mvrs logit midint comstsdefpw_1_lg1 mcap_1_lg1 ,  degree(3) df(comstsdefpw_1_lg1:1) knots(`k1' `k2' `k3' `k4') orthog

logit midint comz mcapk_0 mcapk_1 mcapk_2 mcapk_3 mcapk_4 mcapk_5 mcapk_6 mcapk_7

/// Macros for goodness of fit stats
*| Coef
local coef: di%5.3f e(b)[1,1]
local coefn ""
local coefn `coef'
*|Psuedo R squared 
local r2 : di %5.3f `e(r2_p)'
local r2n""
local r2n `r2'
*|AIC
 estat ic
local aic: di%8.0f r(S)[1,5]
local aicn " "
local aicn `aic'
*| AUC
qui lroc, nograph
local auc :di%8.3f r(area)
local aucn ""
local aucn `auc'

/// Deviance residual
predict dres_c, dev

/// Partial predictions with status deficit at mean
su comz
replace comz = r(mean)
predict pr_c, xb

/// Component-plus-residual
gen pres_c = pr_c + dres_c

/// Loop over expected status (subpopulation)
local j 0
local sample small_1 middle_1 major_1 world_1
foreach pc in `sample' {
	local ++ j
	
	*| Temp variable names
	tempname obs`j' binno`j' 
	sort mcap_1_lg1
	
	*| Observation number by subpopulation
    generate `obs`j'' = _n if pr_c < . & `pc' == 1
    su `obs`j''
    local subobs = r(N)

    *| Number of bins (proportional to subpopulation size)
    local nbins`j' = floor(sqrt(`subobs'))
    noi di `nbins`j''
	local nbins`j': di %3.0f `nbins`j''

    *| Bins of (roughly) equal size 
    egen `binno`j'' = cut(`obs`j'') if pr_c < . & `pc' == 1 , group(`nbins`j'') icodes 

    *| Average CINC value by bin
    egen av_c`j' = mean(mcap_1_lg1), by(`binno`j'')

    *| Average prediction by bin 
    egen avcpr_c`j' = mean(pres_c) if `pc' == 1, by(`binno`j'')

    *| Tag binned observations
    egen tag_c`j' = tag(`binno`j'')

}

rcspline pres_c mcap_1_lg1, gen(fitlink) nknots(7)


graph twoway (scatter avcpr_c4 av_c4 if tag_c4 == 1 & avcpr_c4 <= 0 , msize(small) msymbol(o) mcolor(gs13%75) jitter(3.5)) (scatter avcpr_c3 av_c3 if tag_c3 == 1, msize(small) msymbol(o) mcolor(gs9%75*1.1) jitter(3.5)) (scatter avcpr_c2 av_c2 if tag_c2 == 1, msize(small) msymbol(o) mcolor(gs5%75*1.2) jitter(3.5)) (scatter avcpr_c1 av_c1 if tag_c1 == 1, msize(small) msymbol(o) mcolor(gs2%75) jitter(3.5)) (line fitlink mcap_1_lg1, lwidth(medthick) lpattern(dash) color(plg2)) (lowess pres_c mcap_1_lg1, bwidth(.05) lpattern(solid) lwidth(medthick) color(pll2)), ytitle("Partial prediction + residual", size(10.5pt)) ylabel(-10(2)0, tlcolor(black) labsize(10.5pt) nogextend) xtitle(" ") xlabel(0(.05).3, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none) titlegap(-2.1)) subtitle("Linear smoothed (orthogonal cubic spline)", box bexpand fcolor(none) size(10.5pt))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=1)) graphregion(margin(r=3 l=-4.75 t=.4 b=-8)) legend(off) text(-7.55 .265  "{bf:Status deficit coef. = `coefn'}" -8.6 .2495 "AIC = `aicn'" "AUC = `aucn'" "Psuedo R{superscript:2} = `r2n'", just(left) size(10.5pt)) name(lineark,replace)


************************************************ Log cubic spline *********************************


replace comz = clone_def


/// Generate splines
splinegen mcap_1_lg1_ln if `in', basis(mcaplnk) degree(3) df(8) orthog

///mvrs logit midint comstsdefpw_1_lg1 mcap_1_lg1 ,  degree(3) df(comstsdefpw_1_lg1:1) knots(`k1' `k2' `k3' `k4') orthog

///splinegen mcap_1_lg1_ln , basis(mcaplnk) degree(3) df(6) orthog

logit midint comz mcaplnk_0 mcaplnk_1 mcaplnk_2 mcaplnk_3 mcaplnk_4 mcaplnk_5 mcaplnk_6 mcaplnk_7
est store pr4 


/// Macros for goodness of fit stats
*| Coef
local coef: di%5.3f e(b)[1,1]
local coefn ""
local coefn `coef'
*|Psuedo R squared 
local r2 : di %5.3f `e(r2_p)'
local r2n""
local r2n `r2'
*|AIC
 estat ic
local aic: di%8.0f r(S)[1,5]
local aicn " "
local aicn `aic'
*| AUC
qui lroc, nograph
local auc :di%8.3f r(area)
local aucn ""
local aucn `auc'

/// Deviance residual
predict dres_d, dev

/// Partial prediction with status deficit at mean
qui su comz
qui replace comz = r(mean)
qui predict pr_d, xb

/// Component-plus-residual
qui gen pres_d = pr_d + dres_d

/// Loop over levels of expected status (subpopulations)
local j 0
local sample small_1 middle_1 major_1 world_1
qui foreach pc in `sample' {
	local ++ j
	
	*| Temp variable names
	tempname obs`j' binno`j' 
	sort mcap_1_lg1_ln
	
	*| Observation number by subpopulation
    gen `obs`j'' = _n if pr_d < . & `pc' == 1
    su `obs`j'', meanonly
    local subobs = r(N)

    *| Number of bins (proportional to subpopulation size)
    local nbins`j' = floor(sqrt(`subobs'))
	local nbins`j': di %3.0f `nbins`j''

    *| Bins of (roughly) equal size 
    egen `binno`j'' = cut(`obs`j'') if pr_d < . & `pc' == 1 , group(`nbins`j'') icodes 

    *| Average CINC value by bin
    egen av_d`j' = mean(mcap_1_lg1_ln), by(`binno`j'')

    *| Average prediction by bin 
    egen avcpr_d`j' = mean(pres_d) if `pc' == 1, by(`binno`j'')

    *| Tag binned observations
    egen tag_d`j' = tag(`binno`j'')

}

/// Spline regression to generate fitted spline
qui rcspline pres_d mcap_1_lg1_ln, gen(fitlogk) nknots(7)

/// Component-plus-residual bins plot 
est restore pr4
graph twoway (scatter avcpr_d4 av_d4 if tag_d4 == 1 & avcpr_d4 <= 0 , msize(small) msymbol(o) mcolor(gs13%75) jitter(3.5)) (scatter avcpr_d3 av_d3 if tag_d3 == 1, msize(small) msymbol(o) mcolor(gs9%75*1.1) jitter(3.5)) (scatter avcpr_d2 av_d2 if tag_d2 == 1, msize(small) msymbol(o) mcolor(gs5%75*1.2) jitter(3.5)) (scatter avcpr_d1 av_d1 if tag_d1 == 1, msize(small) msymbol(o) mcolor(gs2%75) jitter(3.5)) (line fitlogk mcap_1_lg1_ln, lwidth(medthick) lpattern(dash) color(plg2)) (lowess pres_d mcap_1_lg1_ln, bwidth(.05) lpattern(solid) lwidth(medthick) color(pll2)), ytitle(" ") ylabel(-10(2)0, noticks nolabel nogextend) xtitle(" ") xlabel(-15(2.5)0, noticks nolabel nogextend) xscale(lstyle(none)) yscale(lstyle(none)) subtitle("Log smoothed (orthogonal cubic spline)", box bexpand fcolor(none) size(10.5pt))  plotregion(lcolor(black) margin(r=.375 b=1.5 l=-1.5)) graphregion(margin(r=-2.75 l=-7 t=.4 b=-8)) legend(label(1 "World") label(2 "Major") label(3 "Middle") label(4 "Small") label(5 "Fitted") label(6 "Local") order(0 "{bf:Expected status}" 4 3 2 1 0 "{bf:Estimator}" 5 6) rows(1) pos(6) size(10.5pt) bmargin(vsmall)) text(-.595 -11.94 "{bf:Status deficit coef. = `coefn'}" -1.675 -12.635 "AIC = `aicn'" "AUC = `aucn'" "Psuedo R{superscript:2} = `r2n'", just(left) size(10.5pt)) name(logk,replace)


///histogram variables allows seperation by power class
twoway__histogram_gen mcap_1_lg1 if pclass_1 == 1, frequ width(.01) gen(h1 x1)
twoway__histogram_gen mcap_1_lg1 if pclass_1 == 2, frequ width(.01) gen(h2 x2)
twoway__histogram_gen mcap_1_lg1 if pclass_1 == 3, frequ width(.01) gen(h3 x3)
twoway__histogram_gen mcap_1_lg1 if pclass_1 == 4, frequ width(.01) gen(h4 x4)
twoway__histogram_gen mcap_1_lg1_ln if pclass_1 == 1, frequ width(.5) gen(h1ln x1ln)
twoway__histogram_gen mcap_1_lg1_ln if pclass_1 == 2, frequ width(.5) gen(h2ln x2ln)
twoway__histogram_gen mcap_1_lg1_ln if pclass_1 == 3, frequ width(.5) gen(h3ln x3ln)
twoway__histogram_gen mcap_1_lg1_ln if pclass_1 == 4, frequ width(.5) gen(h4ln x4ln)

/// Conver to percent 
* Have to convert from frequency
count if mcap_1_lg1 != . 
local total = r(N)
forvalues i = 1/4 {
	gen h`i'p = (h`i'/`total')*100
	gen h`i'lnp = (h`i'ln/`total')*100
}


twoway (bar h1p x1, barw(.01) lwidth(vthin) color(gs2%92) fintensity(97.5)) (bar h2p x2, barw(.01) lwidth(vthin) color(gs5%92*1.2) fintensity(97.5)) (bar h3p x3, barw(.01) lwidth(vthin) color(gs9%92*1.1) fintensity(97.5)) (bar h4p x4, barw(.01) lwidth(vthin) color(gs13%92) fintensity(97.5)), ytitle("% of obs.", size(10.5pt)) ylabel(0(25)75, tlcolor(black) labsize(10.5pt) nogextend) xtitle("CINC", size(10.5pt)) xlabel(,labcolor(black) labsize(10.5pt) tlcolor(black) nogextend) xscale(alt lstyle(none) titlegap(-2.1))  yscale(reverse lstyle(none) titlegap(-1.6)) plotregion(lcolor(black) margin(r=.375 t=-1.95 l=.35 b=-1.5)) graphregion(margin(r=3 l=-4.75 b=2.25 t=-2.5)) fysize(18) fxsize(100.5) legend(off) name(cinchist, replace)


twoway (bar h1lnp x1ln, barw(.5)  lwidth(vthin)  color(gs2%92) fintensity(97.5)) (bar h2lnp x2ln, barw(.5) lwidth(vthin) color(gs5%92*1.2) fintensity(97.5)) (bar h3lnp x3ln, barw(.5) lwidth(vthin) color(gs9%92*1.1) fintensity(97.5)) (bar h4lnp x4ln, barw(.5) lwidth(vthin) color(gs13%92) fintensity(97.5)), ytitle(" ") ylabel(0(25)75, noticks nolabel nogextend) xtitle("CINC(ln)", size(10.5pt)) xlabel(,labcolor(black) labsize(10.5pt) tlcolor(black) nogextend) xscale(alt lstyle(none) titlegap(-2.1)) yscale(reverse lstyle(none)) plotregion(lcolor(black) margin(r=.375 t=-1.95 l=-1.5 b=-1.5)) graphregion(margin(r=-2.75 l=-7 b=2.25 t=-2.5)) fysize(18) fxsize(99.5) legend(off)  name(cinclnhist, replace)


grc1leg2 cinchist cinclnhist linear log lineark logk , rows(3) xsize(15) ysize(12) imargin(l=-3.5 r=-3 t=-1.55 b=-1.375)  legendfrom(logk) pos(6) ring(1) ltsize(10.5pt)  graphregion(margin(b=0 t=-2.5 l=2.5 r=3.25)) note("{it:Note}. Component-plus-residual (CPR) plot of the estimated effects of linear and cubic spline functions of CINC and CINC(ln) on the probability of initiating an MID. Histograms of CINC in its raw" "form and the natural log of CINC are plotted in the top-left and top-right panels respectively, while CPR estimates of CINC/CINC(ln) as {it:a}) a linear function and {it:b}) an orthogonalised cubic spline" "of 7 percentalised knots are plotted in the corresponding middle and bottom panels. Estimates are the sum of the partial linear prediction and deviance residual of a given observation, fixing" "status deficit at the full population mean while allowing CINC/CINC(ln) to vary. Estimates are averaged within (roughly) equally sized bins of CINC/CINC(ln). Each binned estimate is represented" "by a circular marker, gradating in color across levels of expected status from small states (dark) to world powers (light). Dashed green lines represent best fit from OLS regression of CPR" "estimates on CINC/CINC(ln). Solid purple lines represent locally smoothed fit from LOWESS regression using a bandwidth of .05. The coefficient for status deficit from each model is reported in" "bold, with goodness-of-fit statistics below.", size(10pt) just(left) span) iscale(.875) name(compres,replace)



gr_edit .note.DragBy -.4705955883501851 -3.176520221363754
// note reposition

gr_edit .legend.plotregion1.key[1].view.style.editstyle marker(size(medium)) editcopy
// view size

gr_edit .legend.plotregion1.key[2].view.style.editstyle marker(size(medium)) editcopy
// view size

gr_edit .legend.plotregion1.key[4].view.style.editstyle marker(size(medium)) editcopy
// view size

gr_edit .legend.plotregion1.key[3].view.style.editstyle marker(size(medium)) editcopy
// view size



end
***********************
`"{it:Note}. Component-plus-residual (CPR) plot of the estimated effects of linear and cubic spline functions of CINC and CINC(ln) on the probability of initiating an MID. Histograms of CINC"' `"in its raw form and the natural log of CINC are plotted in the top-left and top-right panels respectively, while CPR estimates of CINC/CINC(ln) as {it:a}) a linear function and {it:b}) an"' "orthogonalised cubic spline of 7 percentalised knots are plotted in the corresponding middle and bottom panels. Estimates are the sum of the partial linear prediction and deviance" `"residual of a given observation, fixing status deficit at the full population mean while allowing CINC/CINC(ln) to vary. Estimates are averaged within (roughly) equally sized bins of CINC/"' `"CINC(ln). Each binned estimate is represented by a circular marker, gradating in color across levels of expected status from small states (dark) to world powers (light). Dashed red"' `"lines represent best fit rom OLS regression of CPR estimates on CINC/CINC(ln). Solid blue lines represent locally smoothed fit from LOWESS regression using a bandwidth of .05."' "The coefficient for status deficit from each model is reported in bold, with goodness-of-fit statistics below."

16 x 20
20 pt
vsmall small circle
18pt 
16pt
crimson
dodgerblue
iscale 1.075
