capture cd C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/data
capture cd C:/Users/rudi/Dropbox/Git/swiss_inequality_development/data
//add more paths; using capture -> no need to comment out

use steuerdaten20140522_stata12.dta, clear

sort steuerperiode kanton eink_ll

* nur Normalfälle und Reineinkommen EK behalten
keep if eink_art==1 & fall==0

*** Gini berechnen
sort kanton steuerperiode eink_ll
by kanton steuerperiode: gen id =_n

by kanton steuerperiode: gen cpop = sum(anz_pflichtige)
by kanton steuerperiode: gen cpopp = cpop/cpop[_N]
by kanton steuerperiode: gen ppop = anz_pflichtige/cpop[_N]

* Gini für reink
by kanton steuerperiode: gen cinc = sum(reink)
by kanton steuerperiode: gen finc = cinc/cinc[_N]
by kanton steuerperiode: gen Gc = ppop * finc / 2
by kanton steuerperiode: replace Gc = Gc + ppop * finc[_n-1] / 2 if _n>1
by kanton steuerperiode: gen G_reink = (0.5 - sum(Gc))/0.5
by kanton steuerperiode: replace G_reink = . if _n<_N

* Inkl. Nuller-Kategorie
bysort kanton steuerperiode (eink_ll): gen first=1 if _n==1
expand 2 if first==1, gen(expanded)
replace eink_ll=0 if expanded==1
replace eink_ul=0 if expanded==1
replace steuerertrag=0 if expanded==1
replace steink=0 if expanded==1
replace reink=0 if expanded==1
replace anz_pflichtige=null_norm if expanded==1
sort kanton steuerperiode eink_ll eink_ul

* Ginis für steink noch mal berechnen inkl. Nuller; einmal mit Annahme inc=0 und einmal inc=0.5*untergrenze

by kanton steuerperiode: gen cpop0 = sum(anz_pflichtige)
by kanton steuerperiode: gen cpopp0 = cpop0/cpop0[_N]
by kanton steuerperiode: gen ppop0 = anz_pflichtige/cpop0[_N]

by kanton steuerperiode: gen cinc0 = sum(reink)
by kanton steuerperiode: gen finc0 = cinc0/cinc0[_N]
by kanton steuerperiode: gen Gc0 = ppop0 * finc0 / 2
by kanton steuerperiode: replace Gc0 = Gc0 + ppop0 * finc0[_n-1] / 2 if _n>1
by kanton steuerperiode: gen G_reink0 = (0.5 - sum(Gc0))/0.5
by kanton steuerperiode: replace G_reink0 = . if _n<_N

drop if expanded==1

* Perzentile Ansatz nach wikipedia http://en.wikipedia.org/wiki/Pareto_interpolation

*Anteil des Samples unterhalb von a
gen a = eink_ll
gen b = eink_ul
gen P_a = cpopp-ppop
gen P_b = cpopp

gen theta = (log(1-P_a)-log(1-P_b))/(log(b)-log(a))

gen k = ((P_b-P_a)/((1/(a^theta))-(1/(b^theta))))^(1/theta)

gen dist = .

foreach j of numlist 1 5 10 20 25 30 40 50 60 70 75 80 90 95 96 97 98 99 99.5 99.9 99.99 {
local varname=`j'
if (`varname'>99) local varname = `varname' *100

replace dist = cpopp-`j'/100
*wir wollen die kategorie die kleinstöglich über dem perzentil liegt, also tun wir die die kleinstmöglich darunter liegen weit weg -> 1
replace dist=1 if dist <0
bysort kanton steuerperiode dist: gen p`varname' = k * (1/(1-`j'/100))^(1/theta)
by kanton steuerperiode: replace p`varname' = p`varname'[1]
}
sort kanton steuerperiode eink_ll

* Mean
by kanton steuerperiode: gen mean = cinc[_N]/cpop[_N]
by kanton steuerperiode: gen sum_pflichtige = sum(anz_pflichtige)

*drop if G_steink== . | G_steink0== . | G_steink==1 | G_steink0==1 |  G_steink_halb==1 | G_steink_halb==. | G_reink== . | G_reink==1 |  G_taxed== . | G_taxed==1
* Gini steht immer in der letzten Zeile, nur die behalten wir
bysort kanton steuerperiode (eink_ll): gen last=1 if _n==_N
drop if last!=1

*1er zu Missings
foreach var of varlist G_* {
replace `var' = . if `var'==1	
}

keep steuerperiode G_* kanton p* mean sum_pflichtige cpop cinc null*
drop ppop

outsheet using "gini_reink_und_perzentile_normal.csv", replace
