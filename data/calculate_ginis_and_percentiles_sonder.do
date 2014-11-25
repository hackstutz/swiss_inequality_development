capture cd C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/data
capture cd C:/Users/rudi/Dropbox/Git/swiss_inequality_development/data
//add more paths; using capture -> no need to comment out

use steuerdaten20141118_stata12.dta, clear

sort steuerperiode kanton eink_ll
replace steink = steink[_n+481]-steink_diff if steuerperiode ==1945.5
replace anz_pflichtige = anz_pflichtige[_n+481]-anz_pflichtige_diff if steuerperiode ==1945.5

* nur Sonderf�lle und steuerbares EK behalten, reines EK kommt weg, Sonderfälle auch
keep if eink_art==0 & fall==1

*** Gini berechnen
sort kanton steuerperiode eink_ll
by kanton steuerperiode: gen id =_n

by kanton steuerperiode: gen cpop = sum(anz_pflichtige)
by kanton steuerperiode: gen cpopp = cpop/cpop[_N]
by kanton steuerperiode: gen ppop = anz_pflichtige/cpop[_N]

* Gini für steink
by kanton steuerperiode: gen cinc = sum(steink)
by kanton steuerperiode: gen finc = cinc/cinc[_N]
by kanton steuerperiode: gen Gc = ppop * finc / 2
by kanton steuerperiode: replace Gc = Gc + ppop * finc[_n-1] / 2 if _n>1
by kanton steuerperiode: gen G_steink = (0.5 - sum(Gc))/0.5
by kanton steuerperiode: replace G_steink = . if _n<_N

* Perzentile Ansatz nach wikipedia http://en.wikipedia.org/wiki/Pareto_interpolation

*Anteil des Samples unterhalb von a
gen a = eink_ll
gen b = eink_ul
gen P_a = cpopp-ppop
gen P_b = cpopp

gen theta = (log(1-P_a)-log(1-P_b))/(log(b)-log(a))

gen k = ((P_b-P_a)/((1/(a^theta))-(1/(b^theta))))^(1/theta)

gen dist = .

foreach j of numlist 1 5 10 20 25 30 40 50 60 70 75 80 90 95 99 {
replace dist = cpopp-`j'/100
*wir wollen die kategorie die kleinstöglich über dem perzentil liegt, also tun wir die die kleinstmöglich darunter liegen weit weg -> 1
replace dist=1 if dist <0
bysort kanton steuerperiode dist: gen p`j' = k * (1/(1-`j'/100))^(1/theta)
by kanton steuerperiode: replace p`j' = p`j'[1]
}
sort kanton steuerperiode eink_ll

* nun gibt es fehlende Werte für p99 und manchmal p95, immer dann wenn das Perzentil in das letzte Intervall fällt, hierfür extrapolieren wir:
*p99
*bysort kanton steuerperiode: gen extra99=ppop[_N]>0.02 if ppop[_N]!=.

*gen lncinc = ln(cinc)
*gen lncpopp = ln(cpopp)
*bysort kanton steuerperiode: gen last10 = _N-_n<10
*bysort kanton steuerperiode: gen last5 = _N-_n<5
*bysort kanton steuerperiode: gen last3 = _N-_n<3
*bysort kanton steuerperiode: gen last2 = _N-_n<2
*bysort kanton steuerperiode: gen last1 = _N-_n<1

*encode kanton, gen(kanton_id)
*gen steuerperiode_rounded = round(steuerperiode)

*reg lncinc (i.kanton_id##i.steuerperiode_rounded)#c.lncpopp if extra & last10
*clonevar lncpopp_klon =lncpopp
*replace lncpopp = ln(0.99)
*predict ln99 if e(sample)

*bysort kanton steuerperiode: gen p99_ex = (exp(lncinc)-exp(ln99))/(0.01/ppop*anz_pflichtige) if e(sample)&last1
*/

* Mean
by kanton steuerperiode: gen mean = cinc[_N]/cpop[_N]

drop if G_steink== . | G_steink==1

keep steuerperiode G_* kanton p* mean anz_pflichtige cpop cinc null*
drop ppop


outsheet using "ginis_und_perzentile_sonder.csv", replace

