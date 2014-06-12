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
# Achtung: Keine Querschnittgewichte verwendet
# Äquivalenzskalen: In der BFS Publikation 2013 wird bei der Äquivalenzskalenberechnung zwischen Kindern
# und Erwachsenen unterschieden (modifizierte OECD-Skala. Als Kinder gelten Personen unter 14
# In den gelieferten Daten ist die Kinder-Variable allerdings mit unter 15 gelabelt (so verwendet)
# von 2000 bis 2005 gibt es in den Daten keine Unterscheidung von Erwachsenen und Kindern

setwd("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE")


habe0011<-data.frame(c(2000:2011),seq(0))
colnames(habe0011)<-c("Year","Gini")
# 2009 bis 2011
habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe0911$AnzErw<-habe0911$AnzahlPersonen98-habe0911$AnzahlKinder05
habe0911$modOECDSk<-1+0.5*(habe0911$AnzErw-1)+0.3*habe0911$AnzahlKinder05
habe0911$VerfuegbaresEinkommen08gew<-(habe0911$VerfuegbaresEinkommen08/habe0911$modOECDSk)
habe0011$Gini[12]<-by(habe0911$VerfuegbaresEinkommen08gew,habe0911$Jahr08,gini)[3]
habe0011$Gini[11]<-by(habe0911$VerfuegbaresEinkommen08gew,habe0911$Jahr08,gini)[2]
habe0011$Gini[10]<-by(habe0911$VerfuegbaresEinkommen08gew,habe0911$Jahr08,gini)[1]

##
# Exkurs: Replikation offizieler Werte zum verfügbaren Einkommen

weighted.mean(habe0911$VerfuegbaresEinkommen08,habe0911$Gewicht10_091011)

# 2006 bis 2008
habe0608<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2006 bis 2008/HABE060708_Standard_130124UOe.txt", header=TRUE)
habe0608$AnzErw<-habe0608$AnzahlPersonen98-habe0608$AnzahlKinder05
habe0608$modOECDSk<-1+0.5*(habe0608$AnzErw-1)+0.3*habe0608$AnzahlKinder05
habe0608$VerfuegbaresEinkommen08gew<-(habe0608$VerfuegbaresEinkommen08/habe0608$modOECDSk)
habe0011$Gini[09]<-by(habe0608$VerfuegbaresEinkommen08gew,habe0608$Jahr08,gini)[3]
habe0011$Gini[08]<-by(habe0608$VerfuegbaresEinkommen08gew,habe0608$Jahr08,gini)[2]
habe0011$Gini[07]<-by(habe0608$VerfuegbaresEinkommen08gew,habe0608$Jahr08,gini)[1]



# 2001 bis 2005
habe05<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat1_2005_070601pp.txt", header=TRUE)
habe05hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/haushalt_2005_070601pp.txt", header=TRUE, sep="\t" )
habe05$y<-ifelse(habe05$B_AUSGABE==0,habe05$SUMME_BETRAG_CHF,habe05$SUMME_BETRAG_CHF*(-1))
habe05$y[habe05$NOMENKLATUR_STUFE1_ID=="5"]<-0
habe05<-summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe05)
habe05<-merge(habe05,habe05hh)
habe05$y.sum.ag<-habe05$y.sum/(1+0.5*(habe05$ANZ_PERSONEN-1))
habe0011$Gini[06]<-gini(habe05$y.sum.ag,habe05$GEWICHT)

##
# Exkurs: Replikation offizieler Werte zum verfügbaren Einkommen
habe05long<-reshape(habe05,direction="wide",idvar="HAUSHALT_ID",timevar="NOMENKLATUR_STUFE1_ID",v.names="SUMME_BETRAG_CHF")
habe05long[is.na(habe05long)]<-0
habe05long<-merge(habe05long,habe05hh)
habe05long$bruttoeinkommen<-(habe05long$SUMME_BETRAG_CHF.1+habe05long$SUMME_BETRAG_CHF.2+habe05long$SUMME_BETRAG_CHF.3)
weighted.mean(habe05long$bruttoeinkommen,habe05long$GEWICHT)
# Brutoeinkommen stimmt

# Transferausgaben auf Agg_1 sind zu umfassend
# obligatorische Abzüge lassen sich auf Agg_4 abbilden
# Nur obligatorische Abzüge werden vom Primäreinkommen abgezogen
# Obligatorische Abzüge sind
# Sozialversicherungsbeiträge (128)
# Steuern (136) 
# Krankenkassenprämien (129)
# regelmässige Transferzahlungen an andere Privathaushalte (141) > nicht 100% sicher ob Legate und Schenkungen auch in diese Kategorien fallen
# Liegenschaftssteuern (vermutlich in 136)

habe05ag4<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat4_2005_070601pp.txt", header=TRUE)
habe05ag4long<-reshape(habe05ag4,direction="wide",idvar="HAUSHALT_ID",timevar="NOMENKLATUR_STUFE4_ID",v.names="SUMME_BETRAG_CHF")
habe05ag4long[is.na(habe05ag4long)]<-0
habe05ag4long$ausgaben<-(habe05ag4long$SUMME_BETRAG_CHF.128+habe05ag4long$SUMME_BETRAG_CHF.136+habe05ag4long$SUMME_BETRAG_CHF.129+habe05ag4long$SUMME_BETRAG_CHF.141)
habe05ag4long<-merge(habe05ag4long,habe05hh)
weighted.mean(habe04ag4long$ausgaben,habe05ag4long$GEWICHT)
# passt nicht


# obligatorische Abzüge lassen sich auf Agg_5 abbilden
# Sozialversicherungsbeiträge (821,822,823,824,825)
# Steuern (843,844,845,847) 
# Krankenkassenprämien (829)
# regelmässige Transferzahlungen an andere Privathaushalte (866,867) > nicht 100% sicher ob Legate und Schenkungen auch in diese Kategorien fallen
# Liegenschaftssteuern (848) (allenfalls auch 849> Zweitwohnsitze)

# Vergleich mit Detailtabelle Ausgaben 2009-2011
# >Liegenschaftssteuern sind nicht aufgeführt.
# Krankenkassen: Prämien für die Unfallversicherung (ohne Berufsunfälle)> nicht auffindbar
# Terminologie bei Kategorie Transferausgaben an andere Haushalte ist anders > schwierig zu überprüfen, ob das selbe abgebildet ist

habe05ag5<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat5_2005_070601pp.txt", header=TRUE)
habe05ag5long<-reshape(habe05ag5,direction="wide",idvar="HAUSHALT_ID",timevar="NOMENKLATUR_STUFE5_ID",v.names="SUMME_BETRAG_CHF")
habe05ag5long[is.na(habe05ag5long)]<-0
habe05ag5long$ausgaben<-(habe05ag5long$SUMME_BETRAG_CHF.821+habe05ag5long$SUMME_BETRAG_CHF.822+habe05ag5long$SUMME_BETRAG_CHF.823+habe05ag5long$SUMME_BETRAG_CHF.824+habe05ag5long$SUMME_BETRAG_CHF.825+
                         habe05ag5long$SUMME_BETRAG_CHF.843+habe05ag5long$SUMME_BETRAG_CHF.844+habe05ag5long$SUMME_BETRAG_CHF.845+habe05ag5long$SUMME_BETRAG_CHF.847+
                         habe05ag5long$SUMME_BETRAG_CHF.829+habe05ag5long$SUMME_BETRAG_CHF.866+habe05ag5long$SUMME_BETRAG_CHF.867)
habe05ag5long<-merge(habe05ag5long,habe05hh)
weighted.mean(habe05ag5long$ausgaben,habe05ag4long$GEWICHT)

# Fortsetzung mit 2004

habe04<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat1_2004_061127pp.txt", header=TRUE)
habe04hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/haushalt_2004_061127pp.txt", header=TRUE,sep="\t")
habe04$y<-ifelse(habe04$B_AUSGABE==0,habe04$SUMME_BETRAG_CHF,habe04$SUMME_BETRAG_CHF*(-1))
habe04$y[habe04$NOMENKLATUR_STUFE1_ID=="5"]<-0
habe04<-summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe04)
habe04<-merge(habe04,habe04hh)
habe04$y.sum.ag<-habe04$y.sum/(1+0.5*(habe04$ANZ_PERSONEN-1))
habe0011$Gini[05]<-gini(habe04$y.sum.ag,habe04$GEWICHT)

habe03<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat1_2003_050614pp.txt", header=TRUE)
habe03hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/haushalt_2003_050614pp.txt", header=TRUE,sep="\t")
habe03$y<-ifelse(habe03$B_AUSGABE==0,habe03$SUMME_BETRAG_CHF,habe03$SUMME_BETRAG_CHF*(-1))
habe03$y[habe03$NOMENKLATUR_STUFE1_ID=="5"]<-0
habe03<-summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe03)
habe03<-merge(habe03,habe03hh)
habe03$y.sum.ag<-habe03$y.sum/(1+0.5*(habe03$ANZ_PERSONEN-1))
habe0011$Gini[04]<-gini(habe03$y.sum.ag,habe03$GEWICHT)

habe02<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/eintrag_hh_aggregat1_2002_050104pp.txt", header=TRUE)
habe02hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/haushalt_2002_050615pp.txt", header=TRUE,sep="\t")
habe02$y<-ifelse(habe02$B_AUSGABE==0,habe02$SUMME_BETRAG_CHF,habe02$SUMME_BETRAG_CHF*(-1))
habe02$y[habe02$NOMENKLATUR_STUFE1_ID=="5"]<-0
habe02<-summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe02)
habe02<-merge(habe02,habe02hh)
habe02$y.sum.ag<-habe02$y.sum/(1+0.5*(habe02$ANZ_PERSONEN-1))
habe0011$Gini[03]<-gini(habe02$y.sum.ag,habe02$GEWICHT)

habe01<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/eintrag_hh_aggregat1_2001_30_07_03.txt", header=TRUE)
habe01hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/haushalt_2001_02_04_03.txt", header=TRUE,sep="\t")
habe01$y<-ifelse(habe01$B_AUSGABE==0,habe01$SUMME_BETRAG_CHF,habe01$SUMME_BETRAG_CHF*(-1))
habe01$y[habe01$NOMENKLATUR_STUFE1_ID=="5"]<-0
habe01<-summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe01)
habe01<-merge(habe01,habe01hh)
habe01$y.sum.ag<-habe01$y.sum/(1+0.5*(habe01$ANZ_PERSONEN-1))
habe0011$Gini[02]<-gini(habe01$y.sum.ag,habe01$GEWICHT)

# 2000
##
# Achtung: Andere Codierung
# Nur obligatorische Abzüge werden vom Bruttoeinkommen abgezogen
# Operationalisierung mit Aggregat4
# Obligatorische Abzüge sind
# Sozialversicherungsbeiträge (197)
# Steuern (205) 
# Krankenkassenprämien (198)
# regelmässige Transferzahlungen an andere Privathaushalte (210) > nicht 100% sicher ob Legate und Schenkungen auch in diese Kategorien fallen
# Liegenschaftssteuern (vermutlich in 205)
# Vermutlich ist Aggregat4 zu grob (bspw. umfasst Krankenkassenprämien das Total und nicht nur die Grundversicherung)

habe00hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/HAUSHALT_2000.txt", header=TRUE,sep="\t")
habe00<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/EINTRAG_HH_AGGREGAT4.txt", header=TRUE)
habe00$y<-ifelse(habe00$B_AUSGABE==0, habe00$SUMME_BETRAG_CHF, ifelse(habe00$NOMENKLATUR_STUFE4_ID%in%c(197,205,198,210),habe00$SUMME_BETRAG_CHF*(-1),0))

## Operationalisierung mit Aggregat5
# Obligatorische Abzüge sind
# Sozialversicherungsbeiträge (821,822,823,824,825)
# Steuern (843,844,845,847) 
# Krankenkassenprämien (829)
# regelmässige Transferzahlungen an andere Privathaushalte (866,867) > nicht 100% sicher ob Legate und Schenkungen auch in diese Kategorien fallen
# Liegenschaftssteuern (848) (allenfalls auch 849> Zweitwohnsitze)

habe00hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/HAUSHALT_2000.txt", header=TRUE,sep="\t")
habe00<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/EINTRAG_HH_AGGREGAT5.txt", header=TRUE)
habe00$bruttoeinkommen<-ifelse(habe00$B_AUSGABE==0,ifelse(habe00$NOMENKLATUR_STUFE5_ID%in%c(50,51,52,63,64,65,66,67,68,69,70),0,habe00$SUMME_BETRAG_CHF),0)                     
habe00long<-summaryBy(bruttoeinkommen~HAUSHALT_ID,FUN=sum,data=habe00)
# Test Bruttoeinkommen
habe00long<-merge(habe00long,habe00hh)
weighted.mean(habe00long$bruttoeinkommen.sum,habe00long$GEWICHT)
# zu hoch
# Test Ausgaben
habe00long<-reshape(habe00,direction="wide",idvar="HAUSHALT_ID",timevar="NOMENKLATUR_STUFE5_ID",v.names="SUMME_BETRAG_CHF")
habe00long[is.na(habe00long)]<-0
habe00long$ausgaben<-(habe00long$SUMME_BETRAG_CHF.821+habe00long$SUMME_BETRAG_CHF.822+habe00long$SUMME_BETRAG_CHF.823+habe00long$SUMME_BETRAG_CHF.824+habe00long$SUMME_BETRAG_CHF.825+
                           habe00long$SUMME_BETRAG_CHF.843+habe00long$SUMME_BETRAG_CHF.844+habe00long$SUMME_BETRAG_CHF.845+habe00long$SUMME_BETRAG_CHF.847+
                           habe00long$SUMME_BETRAG_CHF.829+habe00long$SUMME_BETRAG_CHF.866+habe00long$SUMME_BETRAG_CHF.867)
weighted.mean(habe00long$ausgaben,habe00long$GEWICHT)

#Äquivalenzskala (nicht korrigiert)
habe00$y.sum.ag<-habe00$y.sum/(1+0.5*(habe00$ANZ_PERSONEN-1))
habe0011$Gini[01]<-gini(habe00$y.sum.ag,habe00$GEWICHT)
habe00<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2000 bis 2002/EINTRAG_HH_AGGREGAT1.txt", header=TRUE)
habe00$y<-ifelse(habe00$B_AUSGABE==0,habe00$SUMME_BETRAG_CHF,0)
habe00<-summaryBy(y~HAUSHALT_ID,FUN=sum,data=habe00)
habe00<-merge(habe00,habe00hh)


number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(data=habe0011,aes(x=Year,y=Gini))+
  scale_y_continuous(limits=c(0.2,0.5),breaks=number_ticks(4))+ 
  scale_x_continuous(limits=c(2000,2011),breaks=number_ticks(11)) +
  ylab("Gini mit Gewichten für 2000 bis 2005")+
         geom_line()+
         geom_point()+
         theme_bw()

##############################################################
### Analyses #################################################
##############################################################

habe<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)



ggplot(habe, aes(x=log(VerfuegbaresEinkommen08)))+geom_histogram()+facet_wrap(~Jahr08)+theme_bw()
ggplot(habe, aes(y=log(VerfuegbaresEinkommen08),x=percentiles))+geom_line()+facet_wrap(~Jahr08)+theme_bw()
ggplot(habe, aes(x=log(VerfuegbaresEinkommen08),group=Jahr08,color=factor(Jahr08)))+geom_density()+theme_bw()
ggplot(filter(habe,Jahr08%in%c(2006,2008)), aes(x=log(VerfuegbaresEinkommen08),group=Jahr08,fill=factor(Jahr08)))+geom_density(alpha=0.5)+theme_bw()

reldist(y=habe$VerfuegbaresEinkommen08[habe$Jahr08==2008],yo=habe$VerfuegbaresEinkommen08[habe$Jahr08==2006])
reldist(y=habe$VerfuegbaresEinkommen08[habe$Jahr08==2011],yo=habe$VerfuegbaresEinkommen08[habe$Jahr08==2006])
reldist(y=habe$VerfuegbaresEinkommen08[habe$Jahr08==2011],yo=habe$VerfuegbaresEinkommen08[habe$Jahr08==2010])