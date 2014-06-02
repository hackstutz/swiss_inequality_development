##############################################################
### Libraries ################################################
##############################################################

library(foreign)
library(ggplot2)
library(dplyr)
library(reldist)
library(doBy)


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
### Setup Data ##             Dateien lokal an der BFH    ####
############################################################## 

# Gini Time Series equivalized disposal income 2000 until 2011 
# Achtung: Zahlen sind ungewichtet. Keine Äquivalenzskalengewichtung.

setwd("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE")

habe0011<-data.frame(c(2000:2011),seq(0))
colnames(habe0011)<-c("Year","Gini")
habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe0011$Gini[12]<-by(habe0911$VerfuegbaresEinkommen08,habe0911$Jahr08,gini)[3]
habe0011$Gini[11]<-by(habe0911$VerfuegbaresEinkommen08,habe0911$Jahr08,gini)[2]
habe0011$Gini[10]<-by(habe0911$VerfuegbaresEinkommen08,habe0911$Jahr08,gini)[1]
habe0608<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2006 bis 2008/HABE060708_Standard_130124UOe.txt", header=TRUE)
habe0011$Gini[09]<-by(habe0608$VerfuegbaresEinkommen08,habe0608$Jahr08,gini)[3]
habe0011$Gini[08]<-by(habe0608$VerfuegbaresEinkommen08,habe0608$Jahr08,gini)[2]
habe0011$Gini[07]<-by(habe0608$VerfuegbaresEinkommen08,habe0608$Jahr08,gini)[1]
habe05<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat0_2005_070601pp.txt", header=TRUE)
habe05$y<-ifelse(habe05$B_AUSGABE==0,habe05$SUMME_BETRAG_CHF,habe05$SUMME_BETRAG_CHF*(-1))
habe0011$Gini[06]<-gini(summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe05)$y.sum)
habe04<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat0_2004_061127pp.txt", header=TRUE)
habe04$y<-ifelse(habe04$B_AUSGABE==0,habe04$SUMME_BETRAG_CHF,habe04$SUMME_BETRAG_CHF*(-1))
habe0011$Gini[05]<-gini(summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe04)$y.sum)
habe03<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat0_2003_050614pp.txt", header=TRUE)
habe03$y<-ifelse(habe03$B_AUSGABE==0,habe03$SUMME_BETRAG_CHF,habe03$SUMME_BETRAG_CHF*(-1))
habe0011$Gini[04]<-gini(summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe03)$y.sum)
habe02<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/eintrag_hh_aggregat0_2002_050104pp.txt", header=TRUE)
habe02$y<-ifelse(habe02$B_AUSGABE==0,habe02$SUMME_BETRAG_CHF,habe02$SUMME_BETRAG_CHF*(-1))
habe0011$Gini[03]<-gini(summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe02)$y.sum)
habe01<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/eintrag_hh_aggregat0_2001_30_07_03.txt", header=TRUE)
habe01$y<-ifelse(habe01$B_AUSGABE==0,habe01$SUMME_BETRAG_CHF,habe01$SUMME_BETRAG_CHF*(-1))
habe0011$Gini[02]<-gini(summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe01)$y.sum)


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