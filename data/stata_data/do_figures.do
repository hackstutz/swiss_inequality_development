* Pfad
cd "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\figures\"

* Black/White Theme
set scheme rudi

* Wir wollen 6 Grafiken: 
	* 1: reldist Beispiel 2003 vs 2010
	* 2: special vs normal 1993/94
	* 3: special vs normal 2010
	* 4: married HBS vs ESTV
	* 5: Bern Tax Data vs. HBS
	* 6: Bern Tax Data: tax units vs households

* Figure 1:Distribution change over time
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\bd2010_2003.dta", clear
* Perzentilsgrenzen anschaune
su bd, det
reldist bd, by(group) title("2003 vs. 2010") xtitle("% of ref. group (2003)") subtitle(" ") olabel(21.9 43.7 68.2) otitle("taxable income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 1.5 2 2.5 3)
graph save figure1_2010_2003, replace


* Figure 2:Comparison of normal and special cases 1993/94
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\ns1993_94.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("normal vs. all cases 93/94") subtitle(" ") xtitle("% of ref. group (all cases)") olabel(32.5 45.0 62.3) otitle("taxable income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 1.5 2 2.5 3)
graph save figure2_ns_1993_94, replace


* Figure 3:Comparison of normal and special cases 2010
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\ns2010.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("normal vs. all cases 2010") subtitle(" ") xtitle("% of ref. group (all cases)") olabel(37.0 52.3 73.6) otitle("taxable income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 1.5 2 2.5 3)
graph save figure3_ns_2010, replace

/* 
* Comparison of tax and survey data
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010.dta", clear
su income, det
* wir machen hier shape effects. verschiebung um log(mean) durch "shape mean mult"
reldist income [pweight=weight], by(group) shape mean mult title("tax vs. survey data") subtitle("Shape Effect") xtitle("% of ref. group (Survey)") olabel(27.9 51.2 81.9) otitle("taxable income (in 1000 CHF)") 
graph save figure11_habe_bd, replace
*/

* Figure 4:Comparison of tax and survey data for married
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_married.dta", clear
su income, det
*ebenfalls shape effects
reldist income [pweight=weight], by(group) shape mean mult title("tax vs HBS data - married") subtitle("shape effect")  xtitle("% of ref. group (Survey)") olabel(46.6 70.8 103.4) otitle("taxable income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 1.5 2 2.5 3)
graph save figure4_habe_bd_married, replace

/*
* Figure 13:Comparison of tax and survey data for unmarried
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_unmarried.dta", clear
su income, det
reldist income, by(group) shape mean mult title("tax vs. survey data") subtitle("Shape Effect - unmarried")  xtitle("% of ref. group (Survey)") olabel(15.7 35.9 55.2) otitle("taxable income (in 1000 CHF)") 
graph save figure13_habe_bd_unmarried, replace
*/

* Figure 5: Bern tax data vs HBS
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\bern_habe.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("HBS vs Bern tax data") subtitle(" ") xtitle("% of ref. group (HBS households)") olabel(52.8 84.2 127.9) otitle("primary income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 1.5 2 2.5 3)
graph save figure5_bern_habe, replace

* Figure 6: Bern tax data: tax units vs households
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\bern_plain_hh.dta", clear
* Perzentilsgrenzen anschaune
su income, det
reldist income, by(group) title("HBS vs Bern tax data") subtitle(" ") xtitle("% of ref. group (HBS households)") olabel(44.36 70.9 111.1) otitle("primary income (in 1000 CHF)")  yscale(range(0 3)) ylabel(0 0.5 1 1.5 2 2.5 3)
graph save figure6_bern_plain_hh, replace



graph combine figure1_2010_2003.gph figure2_ns_1993_94.gph figure3_ns_2010.gph figure4_habe_bd_married.gph figure5_bern_habe.gph figure6_bern_plain_hh.gph, title("Relative distributions over time, population and data source")
graph export combined_figures.png, replace

