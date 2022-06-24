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






********************************************************************
*       RESULTS BY TYPE OF CRIME
********************************************************************

* Generate proportion of each type within category before gambling
gen before=year<=2011

foreach depvar in violent robbery battery_aggravated assault_aggravated sexual_assault homicide  {
    egen before_`depvar'=mean(`depvar'), by(before)
    replace before_`depvar'=. if before==0
    gen share_`depvar'=before_`depvar'/before_violent
    }
foreach depvar in property larceny burglary motor_vehicle_theft arson {
    egen before_`depvar'=mean(`depvar'), by(before)
    replace before_`depvar'=. if before==0
    gen share_`depvar'=before_`depvar'/before_property
    }
drop before*


foreach depvar in robbery battery_aggravated assault_aggravated sexual_assault homicide larceny burglary motor_vehicle_theft arson{
    replace `depvar'=asinh(`depvar')
} 

foreach depvar in robbery battery_aggravated assault_aggravated sexual_assault homicide larceny burglary motor_vehicle_theft arson {

    qui reghdfe `depvar'  within2   $ACS mindistrb mindistrb2 , absorb(id time i.comarea#c.time) vce(cl id)
    sum share_`depvar'
    estadd local share=r(mean)
    estadd local rivboat      = "Yes", replace 
    estadd local demog        = "Yes", replace 
    estadd local neigh_trends = "Yes", replace 
    estadd local obs = e(N)
    estadd local blocks=e(N_clust)
    estimates store model_`depvar'

}

************************************************************
* estout Panel A
************************************************************

#delimit ; 
esttab model_robbery
        model_battery_aggravated
        model_assault_aggravated
        model_sexual_assault
        model_homicide
       using "${path_tables}/types_crimes.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label
       stats(share
             obs
             blocks, fmt(0 0 0 %9.0gc %9.0gc %9.0gc)
             labels("\midrule 
                    Share
                   Observations"
                   "Number of blocks"
                   ) ) 
       noobs
       mlabels( ,none)  
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(within2     "Within 2 Blocks (=1)")
       keep( within2 )
       order( within2 )
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(95) 
       prehead( 
\begin{tabular}{lccccc}

\toprule
\\
 & (1) & (2) & (3) & (4) & (5) \\
    \midrule
 & \multicolumn{5}{c}{(a) Violent Crime} \\ \cmidrule(lr){2-6}
                     &             & Aggravated  & Aggravated  & Sexual       &           \\
                     &  Robbery                  &    Battery                &   Assault        &   Assault       &    Homicide     \\
                     \midrule \\

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
esttab model_larceny
        model_burglary
        model_motor_vehicle_theft
        model_arson
       using "${path_tables}/types_crimes.tex",
       style(tex) 
       cells(b(star fmt(4)) se(par fmt(4)))
       label 
       stats(rivboat
       		 demog
       		 neigh_trends
       		 share
             obs
             blocks, fmt(0 0 0 %9.0gc %9.0gc %9.0gc)
             labels("\midrule 
                    Share
                   Observations"
                   "Number of blocks"
                   "\midrule
             f(Distance to Riverboats) "
                   "Demographic controls" 
                   "Neighborhood trends" 
                   ) ) 
       mlabels( ,none)  
       noobs
       nonumbers
       collabels(,none) 
       eqlabels(,none)
       varlabels(within2     "Within 2 Blocks (=1)")
       keep( within2 )
       order( within2 )
       starl(* 0.1 ** 0.05 *** 0.01)   
       level(95) 
       prehead( 
               \midrule
 		& \multicolumn{5}{c}{(b) Property Crime} \\ \cmidrule(lr){2-6} \\
        \cmidrule(lr){2-6}
             &             &            & Motor                &     &         \\
                     &     Larceny               &     Burglary               & Vehicle Theft       &   Arson       &         \\
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


