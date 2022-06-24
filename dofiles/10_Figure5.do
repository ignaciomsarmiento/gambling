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






* Generates time since exposure (in months)
egen mint=min(time), by(id within2)
replace mint=. if within2==0
egen minT=max(mint), by(id)
gen T=time-minT

* Generates time since exposure in years
gen esgru=floor(T/12)

gen less_5=esgru<-4 if esgru!=.
replace less_5=0 if less_5==.
forvalues i=4(-1)1 {
    gen less_`i'=esgru==-`i'
    }
gen mid=esgru==0
forvalues i=1/2 {
    gen more_`i'=esgru==`i'
    }
replace more_2=1 if esgru>=3 & esgru!=.
*gen more_3=esgru>=3 if esgru~=.



    
local k=0
foreach depvar in violent property domestic_violent  {
local k=`k'+1

    preserve
    qui reghdfe `depvar'  less_5 less_4 less_3 less_2 mid more_1 more_2    $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)

    
    matrix coef=e(b)
    matrix var=vecdiag(e(V))
    matrix define B=J(8,3,.)
    forvalues i=1/4 {
        matrix B[`i',1] = coef[1,`i'] - invttail(e(N),0.05)*sqrt(var[1,`i'])
        matrix B[`i',2] = coef[1,`i']
        matrix B[`i',3] = coef[1,`i'] + invttail(e(N),0.05)*sqrt(var[1,`i'])
    }
    matrix B[5,1] = 0
    matrix B[5,2] = 0
    matrix B[5,3] = 0
    forvalues i=5/7 {
        local j=`i'+1
        matrix B[`j',1] = coef[1,`i'] - invttail(e(N),0.05)*sqrt(var[1,`i'])
        matrix B[`j',2] = coef[1,`i']
        matrix B[`j',3] = coef[1,`i'] + invttail(e(N),0.05)*sqrt(var[1,`i'])
    }
    clear
    svmat B
    gen n=_n
    twoway scatter B2 n, msymbol(+) mcolor(black)  || rcap B1 B3 n, lcolor(black) xlabel(1 "-5/less" 2 "-4" 3 "-3" 4 "-2" 5 "-1" 6 "+1" 7 "+2" 8 "+3/more", axis(1) angle(90)) yline(0, lcolor(gray)) yscale(range(-0.1(.05)0.15)) ylabel(-0.1(0.05)0.15)  legend(label(1 "Coefficient") label(2 "90% CI") position(4) ring(0) region(lcolor(none) fcolor(none))) xtitle("Years with respect to video gambling establishment opening within 2 blocks") ytitle("Coefficient") ylabel(,nogrid) graphregion(color(white)) plotregion(lstyle(solid))
    graph export "${path_figures}/`depvar'_evstudy_yr.eps", replace
    
    restore

}

