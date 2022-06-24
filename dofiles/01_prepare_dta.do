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


use "data/data_wp_v5.dta", clear
compress
save "data/data_wp_v5.dta", replace


encode blgid, gen(id)
encode tract, gen(tracts)

gen pop_density_2000=population_2000/ALAND10

foreach i in  black_2000 hispanic_2000  med_age_2000  pop_15_35_2000  {
	gen `i'_p=`i'/population_2000
}

foreach i in    hh_vacant_2000 hh_owned_2000 {
	gen `i'_p=`i'/hh_units_2000
}



* Define controls: Census characteristics in 2000 interacted with time
*global ACS "c.population_2000#c.time c.black_2000#c.time c.hispanic_2000#c.time c.pop_15_35_2000#c.time c.hh_vacant_2000#c.time"

* Old: Time varying census controls -- BAD controls
*global ACS  population popdensity medianage hhsize housingunits pct_male15_35 black pct_vacant pct_owner


*** Add Liquor license data
merge 1:1 blgid year month using "data/data_liq_licenses_v2.dta"

foreach var in on_block_n_liq_estab one_block_n_liq_estab two_block_n_liq_estab three_block_n_liq_estab {
	replace `var'=0 if _merge==2 & inrange(time,25 ,127)
}
drop _merge


*** Add travel time
gen GEOID=blgid
merge 1:1 GEOID year month using "data/min_travel_time.dta"
keep if _mer==3
drop _mer
merge 1:1 GEOID year month using "data/travel_distance_time_linear_distance_chicago.dta"
drop _mer GEOID


*** Add indicator for industrial blocks
preserve
insheet using "data/intermediate/industrial_blocks.csv", clear
keep geoid
tostring geoid, replace format(%13.0f)
rename geoid blgid
gen industrial=1
tempfile ind
save "`ind'", replace
restore

merge m:1 blgid using "`ind'"
replace industrial=0 if industrial==.
drop _mer


* Distance to riverboad squared
gen mindistrb2=mindistrb^2

gen popdensity = population/ALAND10


* Add Chicago Sides
preserve
insheet using "data/Chicago_sides.csv", clear
tempfile aux
save "`aux'", replace
restore

merge m:1 comarea using "`aux'"
drop _mer


* Add officers by police district
preserve
forvalues i=1/25 {
	if `i'!=13 & `i'!=21 & `i'!=23 {
	dis "ROUND `i'"
	import excel "OLD/data/FOIA_officers/officers.xlsx", sheet("D`i'") allstring clear
	
	rename A period
	foreach var of varlist B-K {
		local x = `var'[1]
		rename `var' y`x'
	}
	destring period, replace force
	drop if period==.
	merge 1:m period using data/intermediate/month_period_weights_forOfficers.dta, nogen
	gen i=_n
	reshape long y, i(i) j(year)
	destring y, replace i(" -")
	order year period y
	sort year period
	rename y nrofficers_beat
	
	collapse (mean) nrofficers_beat [w=weight], by(year month)
	
	gen police_district=string(`i',"%9.0f")
	
	if `i'==1 {
		tempfile officers
		save "`officers'", replace
		}
	else {
		append using "`officers'"
		compress
		save "`officers'", replace
	}
}
}
restore

merge m:1 police_district year month using "`officers'"
drop if _mer==2
drop _mer


*** Homogenize sample (drop block groups with no variation)
egen zero_violent=max(violent), by(blgid)
replace zero_violent=zero_violent==0
egen zero_property=max(property), by(blgid)
replace zero_property=zero_property==0

compress
save data/prepared_data_allchicago.dta, replace


*** Keep blocks ever within 6
keep if one_block_t==1 | two_block_t==1 | three_block_t==1 | four_block_t==1 | five_block_t==1 | six_block_t==1


drop if zero_property==1
drop if zero_violent==1


save data/prepared_data.dta, replace

