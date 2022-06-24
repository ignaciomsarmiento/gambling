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


gen ihs_violent=asinh(violent)
gen ihs_property=asinh(property)



********************************************************************
*       PLACEBO REGRESSIONS
********************************************************************

** Generate Pre-trends variables
preserve

keep id time year month
keep if year==2015
bys id year month: keep if _n==1
replace year=year+1 if month>=8
replace year=year+2 if month<8

tempfile y1
save "`y1'", replace

replace year=year+1
tempfile y2
save "`y2'", replace

replace year=year+1
append using "`y1'" "`y2'"


sort id year month
bys id: replace time=127+_n

order time year month

tempfile aux1
save "`aux1'", replace

restore
preserve


qui sum time
keep if time==r(max)

keep id within2

merge 1:m id using "`aux1'", nogen

save "`aux1'", replace

restore
preserve

append using "`aux1'"


keep id time within2 year month
order id time within2 year month

xtset id time

forvalues i=1/3 {
    local j=`i'*12
    gen placebo`i'yr=f`j'.within2
    replace placebo`i'yr=0 if placebo`i'yr==.
    replace placebo`i'yr=0 if within2==1
    }


drop if time>127

drop within2

tempfile placebo
save "`placebo'", replace

restore


merge 1:1 id time using "`placebo'", nogen


*** Placebo Regressions
foreach depvar in ihs_violent ihs_property {


    *Base Model - All Controls
    reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    nlcom within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar), post
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model1_`depvar' 

forvalues i=2/3 {
    *Base Model - No Controls 2 years prior
    reghdfe `depvar'  within2  placebo`i'yr , absorb(id time i.comarea#c.time) vce(cl id)
    nlcom within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar), post
    test within2 = placebo`i'yr
    estadd local pval=round(r(p),.0001)    
    estadd local rivboat      = "No", replace 
    estadd local demog        = "No", replace 
    estadd local neigh_trends = "No", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model2_p_`i'_`depvar' 
    
    *Base Model - 2 years prior
    reghdfe `depvar'  within2  placebo`i'yr  $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    nlcom within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar), post
    test within2 = placebo`i'yr
    estadd local pval=round(r(p),.0001)    
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model3_p_`i'_`depvar' 
    
    }
}


************************************************************
* estout Panel A
************************************************************

#delimit ; 
esttab  model1_ihs_violent
        model2_p_2_ihs_violent
        model3_p_2_ihs_violent
        model2_p_3_ihs_violent
        model3_p_3_ihs_violent
       using "${path_tables}/placebo.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label
       stats(pval, fmt(%9.4gc)
             labels("\\ P-value (Whithin2= Placebo)") ) 
       noobs
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(within2     "Within 2 Blocks (=1)"
                placebo2yr  "Placebo (2 years prior)"
                placebo3yr  "Placebo (3 years prior)")
       keep( within2  placebo2yr placebo3yr)
       order(within2  placebo2yr placebo3yr)
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(95) 
       prehead( 
\begin{tabular}{lccccc}

\toprule
\\
 & (1) & (2) & (3) & (4) & (5) \\
 
\midrule
 & \multicolumn{5}{c}{(a) Violent Crime} \\ 
 \cmidrule(lr){2-6}
 \\
       )
       posthead( ) 
       prefoot( ) 
       postfoot( )
       replace;
#delimit cr

************************************************************
* estout Panel B
************************************************************



#delimit ; 
esttab model1_ihs_property 
        model2_p_2_ihs_property 
        model3_p_2_ihs_property 
        model2_p_3_ihs_property 
        model3_p_3_ihs_property 
       using "${path_tables}/placebo.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label 
       stats(pval
             rivboat
             demog
             neigh_trends
             obs
             blocks, fmt(%9.4gc 0 0 0 %9.0gc %9.0gc)
             labels("\\ P-value (Whithin2= Placebo)"
            "\midrule
             f(Distance to Riverboats) "
                   "Demographic controls" 
                   "Neighborhood trends" 
                   "\midrule Observations"
                   "Number of blocks"
                   ) ) 
       mlabels( ,none)  
       noobs
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(within2     "Within 2 Blocks (=1)"
                placebo2yr  "Placebo (2 years prior)"
                placebo3yr  "Placebo (3 years prior)")
       keep( within2  placebo2yr placebo3yr)
       order(within2  placebo2yr placebo3yr)
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(95) 
       prehead( 
               \midrule
        & \multicolumn{5}{c}{(b) Property Crime} \\ \cmidrule(lr){2-6}
            \\
       )
       posthead( ) 
      prefoot( ) 
       postfoot(
            \bottomrule
            \end{tabular}
      )
       
       append;
#delimit cr



