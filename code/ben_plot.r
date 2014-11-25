library(foreign)
library(dplyr)
library(reldist)
ben <- read.dta("C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/data/estv-gini-kt.dta")
estv <- read.dta("C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/data/steuerdaten20140522_stata12.dta")
filter(ben, kanton=="CH")

#estv <- arrange(estv, steuerperiode, kanton, eink_ll)
#1945 fixen (funktioniert nicht)
#index45 <- which(estv$steuerperiode==1945.5)
#estv$steink[index45] <- estv$steink[index45+481]-estv$steink_diff[index45]
#estv$anz_pflichtige[index45] <- estv$anz_pflichtige[index45+481]-estv$anz_pflichtige_diff[index45]

estv_gini <- estv %.% 
	filter(kanton=="CH",eink_art=="Nach steuerbarem Einkommen",fall=="Normalfälle",anz_pflichtige>0,steuerperiode!=1945.5) %.% 
	select(steuerperiode, steink, kanton, eink_ll, eink_ul,anz_pflichtige) %.%
	group_by(factor(steuerperiode)) %.%
	mutate(mean_steink=steink/anz_pflichtige) %.%
	summarise(gini(mean_steink,weights=anz_pflichtige))

	#%.%
	#group_by(factor(steuerperiode)) %.%
	#summarise(gini(steink,weights=anz_pflichtige))

unique(ave(estv_ch$steink, estv_ch$steuerperiode,FUN=gini,weights=estv_ch$anz_pflichtige))