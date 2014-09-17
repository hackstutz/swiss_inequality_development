* Pfad
cd "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\figures\"

* Black/White Theme
set scheme rudi

* Figure 7:Distribution change over time
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\bd2010_2003.dta", clear
reldist bd, by(group) hist title("2003 vs. 2010")
graph save figure7_2010_2003, replace


* Figure 8:Comparison of normal and special cases 1993/94
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\ns1993_94.dta", clear
reldist income, by(group) hist title("normal vs. special 93/94")
graph save figure8_ns_1993_94, replace


* Figure 9:Comparison of normal and special cases 2010
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\ns2010.dta", clear
reldist income, by(group) hist title("normal vs. special 2010")
graph save figure9_ns_2010, replace

* Figure 11:Comparison of tax and survey data
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010.dta", clear
reldist income [pweight=weight], by(group) hist shape title("tax vs. survey data") subtitle("Shape Effect")
graph save figure11_habe_bd, replace

* Figure 12:Comparison of tax and survey data for married
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_married.dta", clear
reldist income [pweight=weight], by(group) hist shape title("tax vs. survey data - married") subtitle("Shape Effect - married)
graph save figure12_habe_bd_married, replace

* Figure 13:Comparison of tax and survey data for unmarried
use "C:\Users\Hackstutz\Dropbox\Git\swiss_inequality_development\data\stata_data\habe_bd_2010_unmarried.dta", clear
reldist income, by(group) hist shape title("tax vs. survey data") subtitle("Shape Effect - unmarried")
graph save figure13_habe_bd_unmarried, replace

graph combine figure7_2010_2003.gph figure8_ns_1993_94.gph figure9_ns_2010.gph figure11_habe_bd.gph figure12_habe_bd_married.gph figure13_habe_bd_unmarried.gph, title("Relative distributions over time, population and data source")
graph export combined_figures.png
