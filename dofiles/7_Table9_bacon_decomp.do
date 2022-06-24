set more off
clear all
set matsize 11000
set emptycells drop
set maxvar 120000


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


gen ihs_violent=asinh(violent)
gen ihs_property=asinh(property)



xtreg ihs_violent  within2   i.time ,  fe vce(robust)
bacondecomp ihs_violent  within2, ddetail

xtreg ihs_property  within2   i.time , fe vce(robust)
bacondecomp ihs_property within2 , ddetail



xtreg ihs_violent  within2  mindistrb mindistrb2 i.time ,  fe vce(robust)
bacondecomp ihs_violent within2 mindistrb mindistrb2 , stub(Bacon_) robust  



xtreg ihs_property  within2  mindistrb mindistrb2 i.time , fe vce(robust)
bacondecomp ihs_property within2 mindistrb mindistrb2 , stub(Bacon_) robust  



