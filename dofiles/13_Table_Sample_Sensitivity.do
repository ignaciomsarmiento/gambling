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



foreach depvar in ihs_violent ihs_property {

	


        * Baseline
    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model1_`depvar' 
    
    * All Chicago
    preserve
    
    use data/prepared_data_allchicago.dta, clear
    xtset id time
    drop if zero_property==1
    drop if zero_violent==1
    egen aux=rowtotal(on_block_t-ten_block_t)
    gen ihs_violent=asinh(violent)
    gen ihs_property=asinh(property)

    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model2_`depvar' 

    * Within 10
    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 if aux>0, absorb(id time i.comarea#c.time) vce(cl id)
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model3_`depvar' 
    
    
    restore
    
    
    * Within 6 - Donut hole
    preserve
    

    gen aux=(one_block_t==0 & two_block_t==0 & three_block_t==1) if time==127
    egen donut=max(aux), by(blgid)
    drop aux

    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 if !donut, absorb(id time i.comarea#c.time) vce(cl id)
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model4_`depvar' 
    

    restore
    
    
    * Within 6 - Drop Industrial
    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 if !industrial, absorb(id time i.comarea#c.time) vce(cl id)
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model5_`depvar' 


    * Within 6 - Drop close to newest RB
    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2  if !inlist(comarea,76,9,10,17), absorb(id time i.comarea#c.time) vce(cl id)
      estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model6_`depvar' 


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
        model6_ihs_violent
       using "${path_tables}/sample_sensitivity.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label
       noobs
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(within2     "Within 2 Blocks (=1)")
       keep( within2)              
       order(within2)
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
        model6_ihs_property 
       using "${path_tables}/sample_sensitivity.tex",
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
       varlabels(within2     "Within 2 Blocks (=1)")
       keep( within2)              
       order(within2)
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


