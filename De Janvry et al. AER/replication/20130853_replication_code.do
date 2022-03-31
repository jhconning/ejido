******************************************************************************************************
/*REPLICATION FILE FOR DELINKING LAND RIGHTS FROM LAND USE: CERTIFICATION AND MIGRATION IN MEXICO
THE FILE CONSISTS OF CODE AND COMMENTS THAT ALLOW FOR REPLICATION OF ALL RESULTS IN MAIN TEXT AND
APPENDIX OF THE PAPER*/
*CONTACT KYLE EMERICK KYLE.EMERICK@TUFTS.EDU WITH QUESTIONS
******************************************************************************************************
clear all
set more off, permanently
*change the line below to directory where all the data are
cd "/Users/replication_data/"


*******************************************
*TABLE 1 IN MAIN TEXT
*******************************************
use "progresa_hhdata.dta", clear
*generate time trends interacted with HH characteristics
*state dummies interacted with time effects
foreach i in 13 16 21 22 24 30 {
gen state_`i' = (entidad==`i')
foreach k in 1998 1999 2000 {
gen statetr_`i'_`k' = state_`i'*yeardum_`k'
}
}
*time trends interacted with household characteristics
foreach i in hh_ag males_17to30_1997 female_head_1997 age_head {
foreach k in 1998 1999 2000 {
gen spectr_`i'_`k' = `i'*yeardum_`k'
}
}
gen outsiders = posesionar+avecindado /*number of non-voting members*/
gen parcelratio = supparcela / suptotalnu /*share of land in parcels in the ejido*/
foreach i in ejidatario suptotalnu outsiders latitude longitude nearcity_km parcelratio iml7954r {
foreach k in 1998 1999 2000 {
gen specej_`i'_`k' = `i'*yeardum_`k'
}
}
*column 1
xi: reg mig_away full_titled i.year i.ejido_numid, vce(cluster ejido_numid)
*column 2
xi: reg mig_away full_titled hh_ag males_17to30_1997 female_head_1997 age_head value_ag i.year i.ejido_numid, vce(cluster ejido_numid)
*column 3
xtset hh_numid year
xtreg mig_away full_titled i.year, fe vce(cluster ejido_numid)
*column 4
xi: reg mig_away full_titled i.ejido_numid statetr_* yeardum_*, vce(cluster ejido_numid)
*column 5
xi: reg mig_away full_titled hh_ag males_17to30_1997 female_head_1997 age_head yeardum_* spectr_* i.ejido_numid, vce(cluster ejido_numid)
*column 6
xi: reg mig_away full_titled yeardum_* specej_* i.ejido_numid, vce(cluster ejido_numid)

*******************************************
*TABLE 2 IN MAIN TEXT
*******************************************
use "locality_level.dta", clear
*column 1
xtreg pobtot after program_after if mpob90>20 & mpob90 !=., fe i(ejido_numid) vce(cluster ejido_numid)
*column 2
xtreg logpop after program_after if mpob90>20 & mpob90 !=., fe i(ejido_numid) vce(cluster ejido_numid)
*column 3
xtreg logpop after program_after value_ag if mpob90>20 & mpob90 !=., fe i(ejido_numid) vce(cluster ejido_numid)
*column 4
gen yearst = 0
replace yearst = 2000 - procedeyea if procedeyea<2000
gen program_after_yt = program_after*yearst
xtreg logpop after program_after program_after_yt if mpob90>20 & mpob90 !=., fe i(ejido_numid) vce(cluster ejido_numid)
*column 5
gen early_after = early_s*after
gen late_after = late_s*after
xtreg logpop after early_after late_after if mpob90>20 & mpob90 !=., fe i(ejido_numid) vce(cluster ejido_numid)
test early_after - late_after = 0
*column 6
xtreg logpop year90dummy promax_year90 if inlist(year,1980,1990) & maxpob80>20 & maxpob80 !=., fe i(ejido_numid) vce(cluster ejido_numid)

*******************************************
*TABLE 3 IN MAIN TEXT
*******************************************
use "ejido_census.dta", clear
*column 1
xi: reg young_mig years_titled i.ESTADO, vce(cluster muni_numid)
*column 2
xi: reg young_mig years_titled improve_seed tractor lights logdist i.ESTADO, vce(cluster muni_numid)

*******************************************
*TABLE 4 IN MAIN TEXT
*******************************************
use "landuse_satellite.dta", clear
*column 1
xtreg lnagri titled sd2 sd3, fe i(ejidoid) vce(cluster ejidoid)
*column 2
xtreg lnagri titled titled_hyield sd2 sd3, fe i(ejidoid) vce(cluster ejidoid)
*column 3
xtreg lnagri titled titled_hyield sd2 sd3 sdT*, fe i(ejidoid) vce(cluster ejidoid)

*******************************************
*TABLE 5 IN MAIN TEXT
*******************************************
use "consumption.dta", clear
gen titled = (date-procededat > 0) /*program completed*/
gen titled1 = (date-procededat>0 & date-procededat<=180) /*6 months or less*/
gen titled3 = (date-procededat > 180) /*more than 6 months*/
gen logcpc = ln(consumo/hhsize)
gen logfood = ln(food/hhsize)
gen lognfood = ln(nonfood/hhsize)

*column 1
xi: reg logcpc titled i.ejido_numid i.date, vce(cl ejido_numid)
*column 2
gen titled_highyield = titled*highyield
xi: reg logcpc titled titled_highyield i.ejido_numid i.date, vce(cl ejido_numid)
*column 3
xi: reg lognfood titled i.ejido_numid i.date, vce(cl ejido_numid)
*column 4
xi: reg lognfood titled1  titled3 i.ejido_numid i.date, vce(cl ejido_numid)
*column 5
xi: reg logfood titled i.ejido_numid i.date, vce(cl ejido_numid)
*column 6
xi: reg logfood titled1  titled3 i.ejido_numid i.date, vce(cl ejido_numid)

*******************************************
*TABLE 6 IN MAIN TEXT
*******************************************
use "progresa_hhdata.dta", clear
gen protreat = (contba==1 & year>=1998)
replace protreat = . if (contba==. | pobre==.)
gen titled_protreat = full_titled*protreat
*year dummies specific for progresa treatment HH
foreach j in 1998 1999 2000 {
gen Qiprotreat`j' = yeardum_`j'*protreat
}
*column 1
xi: reg mig_away full_titled titled_protreat yeardum_1999 yeardum_2000 i.ejido_numid if pobre==1 & year>=1998, vce(cluster ejido_numid)
*column 2
xi: reg mig_away full_titled titled_protreat yeardum_1999 yeardum_2000 Qiprotreat1999 Qiprotreat2000 i.ejido_numid if pobre==1 & year>=1998, vce(cluster ejido_numid)

*****************************************************************
*FIGURE 2 IN MAIN TEXT AND FIGURE A3 IN ONLINE APPENDIX
*****************************************************************
*PROCAMPO LONG DIFFERENCE ANALYSIS
use "procampo2.dta", clear
gen year_t = 1 if year==1995
replace year_t=2 if year==2012
xtset ejido_numid year_t
gen delta_area = logarea - l.logarea
gen delta_benef = logbenef - l.logbenef
gen lareabenef = logarea - logbenef
gen delta_lab = lareabenef - l.lareabenef
gen procedeyear = year(procededat_r)
drop if procedeyear==.
*egen state_phina_id = group(state_phina_2010)
*egen muni_id = group(state_phina_2010 muni_phina_2010)

forvalues l=1993(1)2006 {
qui gen pyear_`l' = (procedeyear==`l')
}
replace pyear_2005 = 1 if procedeyear==2006 |  procedeyear==2005
drop pyear_2006
*run regressions first
reg delta_benef pyear_* i.state_phina_id, vce(cl muni_id)
mat b1 = e(b)'
mat v1 = e(V)
reg delta_area pyear_* i.state_phina_id, vce(cl muni_id)
mat b2 = e(b)'
mat v2 = e(V)
reg delta_lab pyear_* i.state_phina_id, vce(cl muni_id)
mat b3 = e(b)'
mat v3 = e(V)

reg delta_benef pyear_* i.state_phina_id if supusocomu==0, vce(cl muni_id) /*areas with no common land*/
mat b4 = e(b)'
mat v4 = e(V)
reg delta_area pyear_* i.state_phina_id if supusocomu==0, vce(cl muni_id) /*areas with no common land*/
mat b5 = e(b)'
mat v5 = e(V)
reg delta_lab pyear_* i.state_phina_id if supusocomu==0, vce(cl muni_id) /*areas with no common land*/
mat b6 = e(b)'
mat v6 = e(V)

forvalues i=1(1)6 {
mat s`i' = J(44,1,0)
forvalues k=1(1)44 {
mat s`i'[`k',1] = sqrt(v`i'[`k',`k'])
}
}

forvalues l=1(1)6 {
svmat b`l', names(b`l')
svmat s`l', names(s`l')
gen b`l'low = b`l'1 - 1.96*s`l'1
gen b`l'high = b`l'1 + 1.96*s`l'1
}



*create figure 2 in main text
gen index = _n+1992
twoway (rcap b1low b1high index if index<=2005 & index>=1993, lcolor(black)) (scatter b11 index if index<=2005, mcolor(black) msize(med)), ///
scheme(s1color) xtitle(Year of Procede Completion, size(large)) ytitle(Coefficient, size(large)) legend(off) ///
name(b1, replace) title("Change in Log number farmers") xlabel(1993 1996 1999 2002 2005, labsize(medlarge)) yline(0, lpattern(dash) lcolor(black))
twoway (rcap b2low b2high index if index<=2005 & index>=1993, lcolor(black)) (scatter b21 index if index<=2005, mcolor(black) msize(med)), ///
scheme(s1color) xtitle(Year of Procede Completion, size(large)) ytitle(Coefficient, size(large)) legend(off) ///
name(b2, replace) title("Change in Log area") xlabel(1993 1996 1999 2002 2005, labsize(medlarge)) yline(0, lpattern(dash) lcolor(black))
twoway (rcap b3low b3high index if index<=2005 & index>=1993, lcolor(black)) (scatter b31 index if index<=2005, mcolor(black) msize(med)), ///
scheme(s1color) xtitle(Year of Procede Completion, size(large)) ytitle(Coefficient, size(large)) legend(off) ///
name(b3, replace) title("Change in Log area per farmer") xlabel(1993 1996 1999 2002 2005, labsize(medlarge)) yline(0, lpattern(dash) lcolor(black))
graph combine b1 b2 b3, scheme(s1color) xsize(12) ysize(5) ycommon rows(1)



*create figure a3 in online appendix
twoway (rcap b4low b4high index if index<=2005 & index>=1993, lcolor(black)) (scatter b41 index if index<=2005, mcolor(black) msize(med)), ///
scheme(s1color) xtitle(Year of Procede Completion, size(large)) ytitle(Coefficient, size(large)) legend(off) ///
name(b4, replace) title("Change in Log number farmers") xlabel(1993 1996 1999 2002 2005, labsize(medlarge)) yline(0, lpattern(dash) lcolor(black))
twoway (rcap b5low b5high index if index<=2005 & index>=1993, lcolor(black)) (scatter b51 index if index<=2005, mcolor(black) msize(med)), ///
scheme(s1color) xtitle(Year of Procede Completion, size(large)) ytitle(Coefficient, size(large)) legend(off) ///
name(b5, replace) title("Change in Log area") xlabel(1993 1996 1999 2002 2005, labsize(medlarge)) yline(0, lpattern(dash) lcolor(black))
twoway (rcap b6low b6high index if index<=2005 & index>=1993, lcolor(black)) (scatter b61 index if index<=2005, mcolor(black) msize(med)), ///
scheme(s1color) xtitle(Year of Procede Completion, size(large)) ytitle(Coefficient, size(large)) legend(off) ///
name(b6, replace) title("Change in Log area per farmer") xlabel(1993 1996 1999 2002 2005, labsize(medlarge)) yline(0, lpattern(dash) lcolor(black))
graph combine b4 b5 b6, scheme(s1color) xsize(12) ysize(5) ycommon rows(1)



*******************************************
*TABLE A1 IN ONLINE APPENDIX
*******************************************
use "progresa_hhdata.dta", clear
gen titled_problem = full_titled*hasproblem
*generate specific time trends
foreach j in 1998 1999 2000 {
gen Qihas`j' = yeardum_`j'*hasproblem
}
*column 1
xi: reg mig_away full_titled titled_problem highland_r age_head female_head_1997 males_17to30_1997 yeardum_* i.ejido_numid, vce(cluster ejido_numid)
*column 2
xi: reg mig_away full_titled titled_problem highland_r age_head males_17to30_1997 female_head_1997 yeardum_* Qiha* i.ejido_numid, vce(cluster ejido_numid)
*column 3
gen titled_highyield = full_titled*highyield
xi: reg mig_away full_titled titled_highyield age_head female_head_1997 males_17to30_1997 highland_r yeardum_* i.ejido_numid, vce(cluster ejido_numid)
*column 4
foreach j in 1998 1999 2000 {
gen hy_tt_`j' = highyield*(year==`j')
}
xi: reg mig_away full_titled titled_highyield age_head female_head_1997 males_17to30_1997 highland_r hy_tt* yeardum_* i.ejido_numid, vce(cluster ejido_numid)
*column 5
gen titled_highland = full_titled*highland_r
xi: reg mig_away full_titled titled_highland age_head female_head_1997 males_17to30_1997 highland_r yeardum_* i.ejido_numid, vce(cluster ejido_numid)
*column 6
foreach j in 1998 1999 2000 {
gen landtt_`j' = highland_r*(year==`j')
}
xi: reg mig_away full_titled titled_highland age_head female_head_1997 males_17to30_1997 highland_r yeardum_* landtt_* i.ejido_numid, vce(cluster ejido_numid)

*******************************************
*TABLE A2 IN ONLINE APPENDIX
*******************************************
use "locality_level.dta", clear
*column 1
xtreg logpop after program_after if Fhat2>0.5 & Fhat2!=. & mpob90>20 & mpob90 !=., fe vce(cluster ejido_numid) i(ejido_numid)
*column 2
xtreg logpop after program_after if Fhat2<0.5 & Fhat2!=. & mpob90>20 & mpob90 !=., fe vce(cluster ejido_numid) i(ejido_numid)
*column 3
gen Fhat2_after = Fhat2*after
gen Fhat2_proaft = Fhat2*program_after
xtreg logpop after program_after Fhat2_after Fhat2_proaft if mpob90>20 & mpob90 !=. , fe i(ejido_numid) vce(cluster ejido_numid)

*******************************************
*TABLE A3 IN ONLINE APPENDIX
*******************************************
use "progresa_hhdata.dta", clear
gen protreat = (contba==1 & year>=1998)
replace protreat = . if (contba==. | pobre==.)
gen titled_protreat = full_titled*protreat
foreach j in 1998 1999 2000 {
gen Qiprotreat`j' = yeardum_`j'*protreat
}
*columns 1-3
foreach j in 1998 1999 2000 {
xi: reg mig_away protreat age_head hh_ag males_17to30_1997 female_head_1997 if pobre==1 & year==`j', vce(cl loc_numid)
}

*******************************************
*TABLE A4 IN ONLINE APPENDIX
*******************************************
use "progresa_hhdata.dta", clear
collapse (mean) mig_away, by(ejido_numid procedeyea year)
sort ejido_numid year
xtset ejido_numid year
gen lag_mig_away = l.mig_away
gen changmig = mig_away - lag_mig_away
drop if changmig == .
tab procedeyea
keep if year <= procedeyea /*only keep pre-period observations*/
gen py99 = (procedeyea==1999)
gen py00 = (procedeyea==2000)
gen py01 = (procedeyea>2000)
*column 1
xi: reg changmig py99 py00 py01 if year==1998, vce(robust)
test py99 py00 py01
*column 2
xi: reg changmig py00 py01 i.year if year <= 1999 & procedeyea >=1999, vce(cluster ejido_numid)
test py00 py01
*column 3
xi: reg changmig py01 i.year if year <= 2000 & procedeyea >= 2000, vce(cluster ejido_numid)
*column 4
use "progresa_hhdata.dta", clear
collapse (mean) mig_away, by(ejido_numid procedeyea year)
keep if year<=procedeyea
gen year0b = (year==procedeyea)
gen year1b = (year==procedeyea-1)
gen year2b = (year==procedeyea-2)
gen year3b = (year==procedeyea-3)
gen py99 = (procedeyea==1999)
gen py00 = (procedeyea==2000)
gen py01 = (procedeyea>2000)
xi: reg mig_away year0b year1b year2b i.year i.ejido_numid, vce(cluster ejido_numid)

*******************************************
*TABLE A5 IN ONLINE APPENDIX
*******************************************
use "progresa_hhdata.dta", clear
keep if inlist(year,1998,1997)
gen year_mod = 1999 /*just modify years since no attrition in 1997 and 1998*/
replace year_mod = 2000 if year==1998
gen attrition_ind = 0
replace attrition_ind = 1 if year_mod==1999 & drop99==1
replace attrition_ind = 1 if year_mod==2000 & drop00==1
gen titledvar = 0
replace titledvar = 1 if year_mod==1999 & procedeyea<=1998
replace titledvar = 1 if year_mod==2000 & procedeyea<=1999
xi: reg attrition_ind titledvar hh_ag males_17to30_1997 female_head_1997 age_head i.ejido_numid i.year_mod, vce(cluster ejido_numid)

*******************************************
*FIGURE A2 IN ONLINE APPENDIX
*******************************************
use "locality_level.dta", clear
keep if inlist(year,1990,2000) & mpob90>20 & mpob90 !=.
gen year0 = 0
replace year0 = 1 if year==2000
sort loc_id year0
xtset loc_id year0
gen lag_pop = l.pobtot
gen change_pop= pobtot - lag_pop
sum change_pop, detail
drop if change_pop < -89 | change_pop > 57 /*drop top and bottom 5% for purposes of making graph*/
sort program
by program: cumul change_pop, gen(Fhat)
sort program Fhat
twoway (line Fhat change_pop if program==0, xlabel(-89 0 57) lcolor(black) lpattern(shortdash)) (line Fhat change_pop if program==1, ///
xlabel(-89 0 57) lcolor(black)) ///
, xtitle("Population Change 1990-2000") ytitle(Empirical CDF) name(fig1, replace) ///
legend(label(1 "Certified 2000 or Later") label(2 "Certified 1993-1999")) scheme(s1color)
