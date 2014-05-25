##############################################################
### Libraries ################################################
##############################################################

library(foreign)
library(ggplot2)
library(dplyr)

##############################################################
### Setup Data ###############################################
############################################################## 

setwd("C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/")
habe0911<-read.table("C:/Users/Hackstutz/Dropbox/Ungleichheit/HABE/HABE091011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe0608<-read.table("C:/Users/Hackstutz/Dropbox/Ungleichheit/HABE/HABE060708/HABE060708_Standard_130124UOe.txt", header=TRUE)
names(habe0608)<-ifelse(names(habe0608)==names(habe0911),names(habe0608),names(habe0911))
habe <- rbind(habe0608,habe0911)

##############################################################
### Analyses #################################################
##############################################################

ggplot(habe, aes(x=log(VerfuegbaresEinkommen08)))+geom_histogram()+facet_wrap(~Jahr08)

