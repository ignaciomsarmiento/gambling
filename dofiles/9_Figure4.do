set more off
clear all
set matsize 11000


*** Set WD
if c(username)=="nlb75" {
cd  "/Users/nlb75/Dropbox/Research/gambling"
}
else if c(username)=="ham_andres" {
cd  "/Users/ham_andres/Dropbox/research/nico/gambling"
}
else {
	cd  "~/Dropbox/Phd Illinois/Research/gambling/"
	global path_tables  "~/Dropbox/Apps/ShareLaTeX/Gambling/material/Tables/ihs/_raw"
	global path_figures  "~/Dropbox/Apps/ShareLaTeX/Gambling/material/Figures/ihs"
}

use data/prepared_data.dta, clear

xtset id time

* ACS Controls
*global ACS  population popdensity   hh_units pop_15_35_p white_p  black_p hh_vacant_p renter_p
*global ACS "c.population_2000#c.time c.black_2000#c.time c.hispanic_2000#c.time c.pop_15_35_2000#c.time c.hh_vacant_2000#c.time"
global ACS "c.pop_density_2000#c.time c.med_age_2000#c.time c.black_2000_p#c.time c.hispanic_2000_p#c.time c.hh_vacant_2000_p#c.time c.renter_p#c.time"

summarize $ACS

* Tax collected per VGT in 10,000s
gen taxpvgtw2=NTITaxRate_within2/VGTcount_within2
replace taxpvgtw2=0 if taxpvgtw2==.
replace taxpvgtw2=taxpvgtw2/10000


replace violent=asinh(violent)
replace property=asinh(property)
replace domestic_violent=asinh(domestic_violent)





********************************************************************
*       MAIN REGRESSIONS - Effects by blocks
********************************************************************

foreach depvar in violent property domestic_violent {


    preserve
    qui reghdfe `depvar'  one_block two_block three_block four_block five_block    $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    
    
    matrix coef=e(b)
    matrix var=vecdiag(e(V))
    matrix define B=J(6,3,.)
    forvalues i=1/5 {
        matrix B[`i',1] = coef[1,`i'] - invttail(e(N),0.05)*sqrt(var[1,`i'])
        matrix B[`i',2] = coef[1,`i']
        matrix B[`i',3] = coef[1,`i'] + invttail(e(N),0.05)*sqrt(var[1,`i'])
        }
    clear
    svmat B
    gen n=_n
    replace B1=0 if _n==6
    replace B2=0 if _n==6
    replace B3=0 if _n==6
    
    twoway scatter B2 n, msymbol(+) mcolor(black)  || rcap B1 B3 n, lcolor(black) xlabel(1 "1 Block" 2 "2 Blocks" 3 "3 Blocks" 4 "4 Blocks" 5 "5 Blocks" 6 "6 Blocks", axis(1) angle(90)) yline(0, lcolor(gray)) yscale(range(-0.05(.05)0.15)) ylabel(-0.05(0.05)0.15)  legend(label(1 "Coefficient") label(2 "90% CI") position(4) ring(0) region(lcolor(none) fcolor(none))) xtitle("Blocks to closest VG establishment") ytitle("Coefficient") ylabel(,nogrid) graphregion(color(white)) plotregion(lstyle(solid))
    graph export "${path_figures}/`depvar'_byblock.eps", as(eps) replace
    restore

    
}


