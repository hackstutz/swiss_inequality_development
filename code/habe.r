##############################################################
### Libraries ################################################
##############################################################

library(foreign)
library(ggplot2)
library(dplyr)
library(reldist)

##############################################################
### Setup Data ###############################################
############################################################## 

setwd("C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/")
habe0911<-read.table("C:/Users/Hackstutz/Dropbox/Ungleichheit/HABE/HABE091011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe0608<-read.table("C:/Users/Hackstutz/Dropbox/Ungleichheit/HABE/HABE060708/HABE060708_Standard_130124UOe.txt", header=TRUE)
names(habe0608)<-ifelse(names(habe0608)==names(habe0911),names(habe0608),names(habe0911))
habe <- rbind(habe0608,habe0911)

habe <- habe %.%
	group_by(Jahr08) %.%
	mutate(percentiles = percent_rank(VerfuegbaresEinkommen08))

##############################################################
### Analyses #################################################
##############################################################

ggplot(habe, aes(x=log(VerfuegbaresEinkommen08)))+geom_histogram()+facet_wrap(~Jahr08)+theme_bw()
ggplot(habe, aes(y=log(VerfuegbaresEinkommen08),x=percentiles))+geom_line()+facet_wrap(~Jahr08)+theme_bw()
ggplot(habe, aes(x=log(VerfuegbaresEinkommen08),group=Jahr08,color=factor(Jahr08)))+geom_density()+theme_bw()
ggplot(filter(habe,Jahr08%in%c(2006,2008)), aes(x=log(VerfuegbaresEinkommen08),group=Jahr08,fill=factor(Jahr08)))+geom_density(alpha=0.5)+theme_bw()

reldist(y=habe$VerfuegbaresEinkommen08[habe$Jahr08==2008],yo=habe$VerfuegbaresEinkommen08[habe$Jahr08==2006])
reldist(y=habe$VerfuegbaresEinkommen08[habe$Jahr08==2011],yo=habe$VerfuegbaresEinkommen08[habe$Jahr08==2006])
reldist(y=habe$VerfuegbaresEinkommen08[habe$Jahr08==2011],yo=habe$VerfuegbaresEinkommen08[habe$Jahr08==2010])