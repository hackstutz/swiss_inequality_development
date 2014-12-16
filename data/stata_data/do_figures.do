* Pfad
capture cd "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\figures\"
capture cd "C:\Users\rudi\Dropbox\Git\swiss_inequality_development\data\stata_data\figures\"

* Black/White Theme
set scheme rudi
*set scheme s1mono

* Wir wollen 6 Grafiken: 
	* 1: reldist Beispiel 2003 vs 2011
	* 2: special vs normal 1993/94
	* 3: special vs normal 2011
	* 4: married HBS vs ESTV
	* 5: Bern Tax Data vs. HBS
	* 6: Bern Tax Data: tax units vs households

* Figure 1:Distribution change over time
use "..\bd2011_2003.dta", clear
* Perzentilsgrenzen anschaune
su bd if group==2003, det
local p25=round(r(p25))
local p50=round(r(p50))
local p75=round(r(p75))
reldist bd, by(group) title("2003 vs. 2011") xtitle("% of ref. group (2003)") olabel(`p25' `p50' `p75') otitle("taxable income (in 1000 CHF)") ylabel(0 0.5 1 1.5 2)
graph save figure1_2011_2003, replace


* Figure 2:Comparison of normal and special cases 1993/94
use "..\ns1993_94.dta", clear
* Gruppen tauschen
replace group = 3-group
* Perzentilsgrenzen anschaune
su income if group==1, det
local p25=round(r(p25))
local p50=round(r(p50))
local p75=round(r(p75))
reldist income, by(group) title("including special cases 93/94") xtitle("% of ref. group (only normal cases)") ytitle("") olabel(`p25' `p50' `p75') otitle("taxable income (in 1000 CHF)") ylabel(0 0.5 1 1.5 2)
graph save figure2_ns_1993_94, replace


* Figure 3:Comparison of normal and special cases 2011
use "..\ns2011.dta", clear
* Gruppen tauschen
replace group = 3-group
* Perzentilsgrenzen anschaune
su income if group==1, det
local p25=round(r(p25))
local p50=round(r(p50))
local p75=round(r(p75))
reldist income, by(group) title("including special cases 2011") xtitle("% of ref. group (only normal cases)") ytitle("") olabel(`p25' `p50' `p75') otitle("taxable income (in 1000 CHF)")  ylabel(0 0.5 1 1.5 2)
graph save figure3_ns_2011, replace


* Comparison of tax and survey data
*use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010.dta", clear
*su income, det
* wir machen hier shape effects. verschiebung um log(mean) durch "shape mean mult"
*reldist income [pweight=weight], by(group) shape mean mult title("tax vs. survey data") subtitle("Shape Effect") xtitle("% of ref. group (Survey)") olabel(27.9 51.2 81.9) otitle("taxable income (in 1000 CHF)") 
*graph save figure11_habe_bd, replace


* Figure 4:Comparison of tax and survey data for married
use "..\habe_bd_2011_married.dta", clear
* Gruppen tauschen
replace group = 3-group
su income if group==1, det
local p25=round(r(p25))
local p50=round(r(p50))
local p75=round(r(p75))
reldist income [pweight=weight], by(group) title("Married: tax data vs HBS")  xtitle("% of ref. group (tax data)") olabel(`p25' `p50' `p75') otitle("taxable income (in 1000 CHF)")  shape mean mult  ylabel(0 0.5 1 1.5 2)
graph save figure4_habe_bd_married, replace


* Figure 4:Comparison of tax and survey data for married
*use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_married.dta", clear
*su income, det
*ebenfalls shape effects
*reldist income [pweight=weight], by(group) shape mean mult title("tax vs HBS data - married") subtitle("shape effect")  xtitle("% of ref. group (Survey)") olabel(46.6 70.8 103.4) otitle("taxable income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 2 3 4 6 8)
*graph save figure4_habe_bd_married, replace


* Figure 13:Comparison of tax and survey data for unmarried
*use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_unmarried.dta", clear
*su income, det
*reldist income, by(group) shape mean mult title("tax vs. survey data") subtitle("Shape Effect - unmarried")  xtitle("% of ref. group (Survey)") olabel(15.7 35.9 55.2) otitle("taxable income (in 1000 CHF)") 
*graph save figure13_habe_bd_unmarried, replace


* Figure 5: Bern tax data vs HBS
use "..\bern_habe.dta", clear
* Gruppen tauschen
replace group = 3-group
* Perzentilsgrenzen anschauen
su income if group==1, det
local p25=round(r(p25))
local p50=round(r(p50))
local p75=round(r(p75))
reldist income, by(group) title("Bern: tax data vs HBS") xtitle("% of ref. group (tax data)") ytitle("") olabel(`p25' `p50' `p75') otitle("primary income (in 1000 CHF)")  ylabel(0 0.5 1 1.5 2)
graph save figure5_bern_habe, replace

* Figure 6: Bern tax data: tax units vs households
use "..\bern_plain_hh.dta", clear
* Perzentilsgrenzen anschaune
su income if group==1, det
local p25=round(r(p25))
local p50=round(r(p50))
local p75=round(r(p75))
reldist income [pweight=weight], by(group) title("Bern: tax units vs households") xtitle("% of ref. group (tax units)") ytitle("") olabel(`p25' `p50' `p75') otitle("taxable income (in 1000 CHF)")  ylabel(0 0.5 1 1.5 2) bw(0.4)
graph save figure6_bern_plain_hh, replace


graph combine figure1_2011_2003.gph figure2_ns_1993_94.gph figure3_ns_2011.gph figure4_habe_bd_married.gph figure5_bern_habe.gph figure6_bern_plain_hh.gph, title("Relative distributions over time, population and data source")
graph export combined_figures.png, replace

