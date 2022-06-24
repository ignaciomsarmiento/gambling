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
}

use data/prepared_data.dta, clear

xtset id time

* ACS Controls
*global ACS  population popdensity   hh_units pop_15_35_p white_p  black_p hh_vacant_p renter_p
global ACS "c.population_2000#c.time c.black_2000#c.time c.hispanic_2000#c.time c.pop_15_35_2000#c.time c.hh_vacant_2000#c.time"
*global ACS "c.pop_density_2000#c.time c.med_age_2000#c.time c.black_2000_p#c.time c.hispanic_2000_p#c.time  c.hh_vacant_2000_p#c.time c.hh_owned_2000_p#c.time"

gen ihs_violent=asinh(violent)
gen ihs_property=asinh(property)


qui reghdfe ihs_violent  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
estadd local rivboat 	  = "Yes", replace 
estadd local demog 		  = "Yes", replace 
estadd local neigh_trends = "Yes", replace 
estadd local obs = e(N)
estadd local blocks=e(N_clust)
estimates store model1_violent

qui reghdfe ihs_property  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
estadd local demog 		  = "Yes", replace 
estadd local neigh_trends = "Yes", replace 
estadd local obs = e(N)
estadd local blocks=e(N_clust)
estimates store model1_property

foreach depvar in violent property {

	

	*Poisson
	ppmlhdfe `depvar'  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
	estadd local rivboat 	  = "Yes", replace 
	estadd local demog 		  = "Yes", replace 
	estadd local neigh_trends = "Yes", replace 
	estadd local obs = e(N)
	estadd local blocks=e(N_clust)
	estimates store model2_`depvar'	

	xtpoisson `depvar'  within2 i.time mindistrb mindistrb2 $ACS i.comarea#c.time, fe vce(robust)
	estadd local rivboat 	  = "Yes", replace 
	estadd local demog 		  = "Yes", replace 
	estadd local neigh_trends = "Yes", replace 
	estadd local obs = e(N)
	estadd local blocks=e(N_clust)
	estimates store model3_`depvar'	
	
	xtnbreg `depvar'  within2 i.time mindistrb mindistrb2 $ACS i.comarea#c.time, fe vce(boot)
	estadd local rivboat 	  = "Yes", replace 
	estadd local demog 		  = "Yes", replace 
	estadd local neigh_trends = "Yes", replace 
	estadd local obs = e(N)
	estadd local blocks=e(N_clust)
	estimates store model4_`depvar'	

	

}



************************************************************
* estout Panel A
************************************************************

#delimit ; 
esttab model1_violent
        model2_ihs_violent
        model3_ihs_violent
        model4_ihs_violent
       using "${path_tables}/estimators.tex",
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
esttab model1_property 
        model2_ihs_property 
        model3_ihs_property 
        model4_ihs_property 
       using "${path_tables}/estimators.tex",
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



