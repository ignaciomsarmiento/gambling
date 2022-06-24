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
* 		MAIN REGRESSIONS - Aggregate Within 2
********************************************************************


* Bars in 100s
gen liq_estab=on_block_n_liq_estab
gen liq_estab_within2=(one_block_n_liq_estab+two_block_n_liq_estab)
replace nrofficers_beat=nrofficers_beat

egen xbar = max(within2) 

foreach depvar in ihs_violent ihs_property {

	 egen ybar = mean(`depvar')

    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    nlcom within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar), post
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model1_`depvar'


    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 if liq_estab!=., absorb(id time i.comarea#c.time) vce(cl id)
    nlcom within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar), post
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model2_`depvar'


    qui reghdfe `depvar'  within2  liq_estab $ACS mindistrb mindistrb2 if liq_estab!=., absorb(id time i.comarea#c.time) vce(cl id)
    nlcom (within2: (_b[within2]*xbar*((sqrt(ybar^2+1))/ybar))) ///
         (liq_estab: (_b[liq_estab]*xbar*((sqrt(ybar^2+1))/ybar))), post
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model3_`depvar'

    qui reghdfe `depvar'  within2  liq_estab_within2 $ACS mindistrb mindistrb2 if liq_estab!=., absorb(id time i.comarea#c.time) vce(cl id)
    nlcom (within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar)) ///
          (liq_estab_within2: _b[liq_estab_within2]*xbar*((sqrt(ybar^2+1))/ybar)), post
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model4_`depvar'

    qui reghdfe `depvar'  within2  liq_estab liq_estab_within2 $ACS mindistrb mindistrb2 if liq_estab!=., absorb(id time i.comarea#c.time) vce(cl id)
    nlcom (within2: _b[within2]*xbar*((sqrt(ybar^2+1))/ybar)) ///
           (liq_estab: _b[liq_estab]*xbar*((sqrt(ybar^2+1))/ybar)) ///
           (liq_estab_within2: _b[liq_estab_within2]*xbar*((sqrt(ybar^2+1))/ybar)), post
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model5_`depvar'

    drop ybar

}
 
************************************************************
* estout Panel A
************************************************************

#delimit ; 
esttab model1_ihs_violent
        model2_ihs_violent
        model3_ihs_violent
        model4_ihs_violent
        model5_ihs_violent
       using "${path_tables}/robustness_number_bars.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label
       noobs
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(within2     "Within 2 Blocks (=1)"
                liq_estab "Number of Bars in:     Same block "
                liq_estab_within2 "Within 2 Blocks ")
       keep( within2 liq_estab liq_estab_within2)
       order( within2 liq_estab liq_estab_within2)
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
        model2_ihs_property 
        model3_ihs_property 
        model4_ihs_property 
        model5_ihs_property 
       using "${path_tables}/robustness_number_bars.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label 
       stats(rivboat
       		 demog
       		 neigh_trends
       		 obs
             blocks, fmt(0 0 0 %9.0gc %9.0gc)
             labels("\midrule
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
                liq_estab "Number of Bars in:     Same block "
                liq_estab_within2 "Within 2 Blocks ")
       keep( within2 liq_estab liq_estab_within2)
       order( within2 liq_estab liq_estab_within2)
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


