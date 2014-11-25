* Pfad
cd "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\figures\"

* Black/White Theme
set scheme rudi

* Bern tax data vergleich
use "C:\Users\rudi\Dropbox\Git\swiss_inequality_development\data\stata_data\bern_habe.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("HBS vs Bern tax data") xtitle("Proportion of Reference Group (HBS)") olabel(8.9 47.8 85.5) otitle("primary income (in 1000 CHF)") 
graph save figure_bern_habe, replace


* Figure 7:Distribution change over time
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\bd2010_2003.dta", clear
* Perzentilsgrenzen anschaune
su bd, det
reldist bd, by(group) title("2003 vs. 2010") xtitle("Proportion of Reference Group (2003)") olabel(21.9 43.7 68.2) otitle("taxable income (in 1000 CHF)") 
graph save figure7_2010_2003, replace


* Figure 8:Comparison of normal and special cases 1993/94
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\ns1993_94.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("normal vs. all cases 93/94") xtitle("Proportion of Reference Group (all cases)") olabel(32.5 45.0 62.3) otitle("taxable income (in 1000 CHF)") 
graph save figure8_ns_1993_94, replace


* Figure 9:Comparison of normal and special cases 2010
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\ns2010.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("normal vs. all cases 2010") xtitle("Proportion of Reference Group (all cases)") olabel(37.0 52.3 73.6) otitle("taxable income (in 1000 CHF)") 
graph save figure9_ns_2010, replace

* Figure 11:Comparison of tax and survey data
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010.dta", clear
su income, det
* wir machen hier shape effects. verschiebung um log(mean) durch "shape mean mult"
reldist income [pweight=weight], by(group) shape mean mult title("tax vs. survey data") subtitle("Shape Effect") xtitle("Proportion of Reference Group (Survey)") olabel(27.9 51.2 81.9) otitle("taxable income (in 1000 CHF)") 
graph save figure11_habe_bd, replace

* Figure 12:Comparison of tax and survey data for married
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_married.dta", clear
su income, det
*ebenfalls shape effects
reldist income [pweight=weight], by(group) shape mean mult title("tax vs. survey data - married") subtitle("Shape Effect - married")  xtitle("Proportion of Reference Group (Survey)") olabel(46.6 70.8 103.4) otitle("taxable income (in 1000 CHF)") 
graph save figure12_habe_bd_married, replace

* Figure 13:Comparison of tax and survey data for unmarried
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_unmarried.dta", clear
su income, det
reldist income, by(group) shape mean mult title("tax vs. survey data") subtitle("Shape Effect - unmarried")  xtitle("Proportion of Reference Group (Survey)") olabel(15.7 35.9 55.2) otitle("taxable income (in 1000 CHF)") 
graph save figure13_habe_bd_unmarried, replace

graph combine figure7_2010_2003.gph figure8_ns_1993_94.gph figure9_ns_2010.gph figure11_habe_bd.gph figure12_habe_bd_married.gph figure13_habe_bd_unmarried.gph, title("Relative distributions over time, population and data source")
graph export combined_figures.png, replace
