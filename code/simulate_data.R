## program:     simulate_data.R
## task:        Simulate individual data points out of distribution percentiles and apply relative distribution techniques
## project:     Inequality of income and wealth in switzerland
## subproject:  Assessing inequality with tax data
## date:        June2014


library(dplyr, quietly=TRUE, warn.conflicts = FALSE)
library(ggplot2)
library(reldist)
library(foreign)
#########################
# Compare FTA Data and HABE Data
#############################


####
#     2010            #
####

##
# Prepare FTA-data for 2010 (out of Rudis code) Normal cases
normal <- read.csv("data/ginis_und_perzentile_normal.csv",sep="\t")
normal <- normal %.% select(-G_reink, -G_taxed, -ppop0, -G_steink0)
normal<-normal %.% 
  filter(kanton=="CH",steuerperiode==2010)
start <- which(names(normal)=="p1")
end <- which(names(normal)=="p95")
percentile <- c(1,5,10,20,25,30,40,50,60,70,75,80,90,95)
normal <- data.frame(t(normal[1,start:end]),percentile,anz_pflichtige=89817)
names(normal)[1]=c("inc")


##
# Simulate individual data points
# Percentile-Plot (descriptive  purpose)
ggplot(normal, aes(x=percentile,y=inc))+
  geom_line()+theme_bw()
# cumulative probabilites
cum.p<-c(1,5,10,20,25,30,40,50,60,70,75,80,90,95)/100
# probabilities
prob<-c(cum.p[1],diff(cum.p), .01)
# extreme values beyond x (to sample)
#freq<-max(normal$anz_pflichtige)
freq<-3000
init<-0
fin<-(abs(max(normal$inc,na.rm=TRUE))+5)
# generate the sequence to take pairs from
ival<-c(init,normal$inc,fin)
len<-10000 # sequence of each pair
s<-sapply(2:length(ival),function(i){
  seq(ival[i-1],ival[i],length.out=len)
})
s<-sample(s,freq,prob=rep(prob, each=len),replace=T)
normal.ind<-data.frame(s)
names(normal.ind)[1]=c("inc")
summary(normal.ind)
hist(normal.ind$inc)
#normal.ind$inc<-normal.ind$inc*7 # Rescale income 
#hist(normal.ind$inc)


##
# Prepare HABE Data - 2010
# ATTENTION: not weighted and with disposable income
#habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
#habe10<-habe0911 %.% 
 # filter(Jahr08==2010)
#habe10<-data.frame(habe10$VerfuegbaresEinkommen08)
#names(habe10)<-"inc_habe"
#habe10$inc_habe[habe10$inc_habe<0]<-0 # One values is -20'000
#habe10$inc_habe<-habe10$inc_habe*12/1000 # transform from monthly income to income per year

# Alternative mit Bruttoeinkommen
habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe10<-habe0911 %.% 
  filter(Jahr08==2010)
habe10<-data.frame(habe10$Bruttoeinkommen08)
names(habe10)<-"inc_habe"
habe10$inc_habe<-habe10$inc_habe*12/1000 # transform from monthly income to income per year

##
# Apply Relative Distribution Methods



##
# Graphical Analysis 1


# Compare the distributions
# Traditional Approach - probability density function


density1 <- density(habe10$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-70, 900),
     ylim=c(0,1.559e-02))
title(main="FTA vs HBS income distribution",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(400,400),y=c(1.559e-02,1.559e-02))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("HBS","FTA"))
density2 <- density(normal.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)

##
# Relative Distribution

fig2a <- reldist(y=normal.ind$inc,yo=habe10$inc_habe,
                 ci=FALSE,smooth=0.4,
                 cdfplot=TRUE,
                 yolabs=seq(-1,3,by=0.5),
                 ylabs=seq(-1,3,by=0.5),
                 cex=0.8,
                 ylab="proportion of FTA Distribution",
                 xlab="proportion of HBS Distribution")
title(main="Relative CDF",cex=0.6)

fig2b <- reldist(y=normal.ind$inc,yo=habe10$inc_habe,
                 ci=FALSE,smooth=0.4,
                 yolabs=seq(-1,3,by=0.5),
                 ylim=c(0,5),cex=0.8,
                 ylab="Relative Density",
                 xlab="Proportion of HBS Distribution")
title(main="Relative PDF",cex=0.6)

##
# Decomposing the Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=normal.ind$inc, yo=habe10$inc_habe,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,5.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of HBS Distribution")
title(main=paste("Overall realtive density"))
abline(h=1,lty=2)
g1A <- reldist(y=normal.ind$inc, yo=habe10$inc_habe,
               show="effect",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of Median shift"))
abline(h=1,lty=2)
gA0 <- reldist(y=normal.ind$inc, yo=habe10$inc_habe,
               smooth=0.4, ci=FALSE,
               show="residual",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of different shape"))
abline(h=1,lty=2)
par(mfrow=c(1,1))


##
# statistical Summary 2

# It seems, that the function needs weigths, so we give it some weights...

habe10$wgt<-1
normal.ind$wgt<-1


format(rpy(y=normal.ind$inc,yo=habe10$inc_habe,
           ywgt=normal.ind$wgt,yowgt=habe10$wgt,pvalue=TRUE),
       digits=3)
format(rpluy(y=normal.ind$inc,yo=habe10$inc_habe,
             ywgt=normal.ind$wgt,yowgt=habe10$wgt,pvalue=TRUE),
       digits=3)
format(rpluy(y=normal.ind$inc,yo=habe10$inc_habe,
             ywgt=normal.ind$wgt,yowgt=habe10$wgt,pvalue=TRUE,
             upper=TRUE),
       digits=3)


##
#     2005            #
###


####
# We can do comparison with weights for other years - for example for 2005
# Bruttoeinkommen stimmt in diesem Jahr (verfügbares Einkommen nicht)
# Gewichte haben wir auch



##
# Prepare FTA-data for 2010 (out of Rudis code) Normal cases
normal <- read.csv("data/ginis_und_perzentile_normal.csv",sep="\t")
normal <- normal %.% select(-G_reink, -G_taxed, -ppop0, -G_steink0)
normal<-normal %.% 
  filter(kanton=="CH",steuerperiode==2005)
start <- which(names(normal)=="p1")
end <- which(names(normal)=="p95")
percentile <- c(1,5,10,20,25,30,40,50,60,70,75,80,90,95)
normal <- data.frame(t(normal[1,start:end]),percentile,anz_pflichtige=89817)
names(normal)[1]=c("inc")


##
# Simulate individual data points
# Percentile-Plot (descriptive  purpose)
ggplot(normal, aes(x=percentile,y=inc))+
  geom_line()+theme_bw()
# cumulative probabilites
cum.p<-c(1,5,10,20,25,30,40,50,60,70,75,80,90,95)/100
# probabilities
prob<-c(cum.p[1],diff(cum.p), .01)
# extreme values beyond x (to sample)
#freq<-max(normal$anz_pflichtige)
freq<-3000
init<-0
fin<-(abs(max(normal$inc,na.rm=TRUE))+5)
# generate the sequence to take pairs from
ival<-c(init,normal$inc,fin)
len<-100 # sequence of each pair
s<-sapply(2:length(ival),function(i){
  seq(ival[i-1],ival[i],length.out=len)
})
s<-sample(s,freq,prob=rep(prob, each=len),replace=T)
normal.ind<-data.frame(s)
names(normal.ind)[1]=c("inc")
summary(normal.ind)
hist(normal.ind$inc)
#normal.ind$inc<-normal.ind$inc*7 # Rescale income 
#hist(normal.ind$inc)

##
# Prepare HABE Data - 2005
# ATTENTION: not weighted and with disposable income
habe05<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/eintrag_hh_aggregat1_2005_070601pp.txt", header=TRUE)
habe05hh<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2003 bis 2005/haushalt_2005_070601pp.txt", header=TRUE, sep="\t" )
habe05long<-reshape(habe05,direction="wide",idvar="HAUSHALT_ID",timevar="NOMENKLATUR_STUFE1_ID",v.names="SUMME_BETRAG_CHF")
habe05long[is.na(habe05long)]<-0
habe05long<-merge(habe05long,habe05hh)
habe05long$bruttoeinkommen<-(habe05long$SUMME_BETRAG_CHF.1+habe05long$SUMME_BETRAG_CHF.2+habe05long$SUMME_BETRAG_CHF.3)
habe05long$inc_habe<-habe05long$bruttoeinkommen*13/1000 # transform from monthly income to income per year

# Alternative mit Bruttoeinkommen
#habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
#habe10<-habe0911 %.% 
#  filter(Jahr08==2010)
#habe10<-data.frame(habe10$Bruttoeinkommen08)
#names(habe10)<-"inc_habe"
#habe10$inc_habe<-habe10$inc_habe*13/1000 # transform from monthly income to income per year

##
# Apply Relative Distribution Methods



##
# Graphical Analysis 1


# Compare the distributions
# Traditional Approach - probability density function

##
# we need the HABE data to be weighted

## ATTENTION: Do we have frequency weights or probabilitie weights?
# I think we have frequency weights

# Mögliche wäre es bestehende Fälle anhand der Frequency-weights zu replizieren... Allenfalls besser?

s<-sample(habe05long$inc_habe,size=3087,
                prob=habe05long$GEWICHT/sum(habe05long$GEWICHT),replace=TRUE)
habe05w<-data.frame(s)
names(habe05w)[1]=c("inc_habe")


# Gewichtet
density1 <- density(habe05w$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-100, 950),
     ylim=c(0,7.150e-03 ))
title(main="FTA vs HBS income distribution (weighted)",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(400,400),y=c(7.150e-03 ,7.150e-03 ))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("HBS","FTA"))
density2 <- density(normal.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)

#Ungewichtet
density1 <- density(habe05long$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-100, 950),
     ylim=c(0,7.150e-03 ))
title(main="FTA vs HBS income distribution (unweighted)",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(400,400),y=c(7.150e-03 ,7.150e-03 ))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("HBS","FTA"))
density2 <- density(normal.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)



##
# Relative Distribution

fig2a <- reldist(y=normal.ind$inc,yo=habe05long$inc_habe,
                 ci=FALSE,smooth=0.4,
                 cdfplot=TRUE,
                 yolabs=seq(-1,3,by=0.5),
                 ylabs=seq(-1,3,by=0.5),
                 cex=0.8,
                 ylab="proportion of FTA Distribution",
                 xlab="proportion of HBS Distribution")
title(main="Relative CDF",cex=0.6)

fig2b <- reldist(y=normal.ind$inc,yo=habe05long$inc_habe,
                 ci=FALSE,smooth=0.4,
                 yolabs=seq(-1,3,by=0.5),
                 ylim=c(0,2.5),cex=0.8,
                 ylab="Relative Density",
                 xlab="Proportion of HBS Distribution")
title(main="Relative PDF",cex=0.6)

##
# Decomposing the Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=normal.ind$inc, yo=habe05long$inc_habe,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,3.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of HBS Distribution")
title(main=paste("Overall realtive density",cex=0.6)
      abline(h=1,lty=2)
      g1A <- reldist(y=normal.ind$inc, yo=habe05long$inc_habe,
                     show="effect",
                     bar=TRUE, quiet=FALSE,
                     ylim=c(0.5,3.0), ylab="",
                     smooth=0.4, ci=FALSE,
                     yolabs=seq(-1,3,by=0.5),
                     xlab="Proportion of HBS Distribution")
      title(main=paste("Effect of Median shift",cex=0.6)
            abline(h=1,lty=2)
            gA0 <- reldist(y=normal.ind$inc, yo=habe05long$inc_habe,
                           smooth=0.4, ci=FALSE,
                           show="residual",
                           bar=TRUE, quiet=FALSE,
                           ylim=c(0.5,3.0), ylab="",
                           yolabs=seq(-1,3,by=0.5),
                           xlab="Effect of different shape")
            title(main=paste("Overall realtive density",cex=0.6)
                  abline(h=1,lty=2)
                  par(mfrow=c(1,1))
                  
                  
                  ##
                  # statistical Summary 2
                  
                  # It seems, that the function needs weigths, so we give it some weights...
                  
                  habe10$wgt<-1
                  normal.ind$wgt<-1
                  
                  
                  format(rpy(y=normal.ind$inc,yo=habe05long$inc_habe,
                             ywgt=normal.ind$wgt,yowgt=habe10$wgt,pvalue=TRUE),
                         digits=3)
                  format(rpluy(y=normal.ind$inc,yo=habe05long$inc_habe,
                               ywgt=normal.ind$wgt,yowgt=habe10$wgt,pvalue=TRUE),
                         digits=3)
                  format(rpluy(y=normal.ind$inc,yo=habe05long$inc_habe,
                               ywgt=normal.ind$wgt,yowgt=habe10$wgt,pvalue=TRUE,
                               upper=TRUE),
                         digits=3)



                  


###########################################################################################
# Comparing HBS-data to FTA-data, we see that FTA-Data exceeds HBS-Data in the middle of the distribution
# We think it's because we compare tax units to households
# To find out about that, we try to focus the comparision on a) singles and b) married to see
###########################################################################################


# Get Brülhartdata for 2010
estv2010 <- read.csv("data/estv_normal_sonder_mitNuller_stE_2010.csv",sep=";")


####
# Select percentiles for all cases (total)

estv2010.total<-estv2010[c(18:38),c(1,4)]

##
# Simulate individual data points
# cumulative probabilites
cum.p<-c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99,99.5,99.9,99.99)/100
# probabilities
prob<-c(cum.p[1],diff(cum.p), .01)
# extreme values beyond x (to sample)
#freq<-max(normal$anz_pflichtige)
freq<-3000
init<-0
fin<-(abs(max(estv2010.total$Total,na.rm=TRUE))+5)
# generate the sequence to take pairs from
ival<-c(init,estv2010.total$Total,fin)
len<-10000 # sequence of each pair
s<-sapply(2:length(ival),function(i){
  seq(ival[i-1],ival[i],length.out=len)
})
s<-sample(s,freq,prob=rep(prob, each=len),replace=T)
estv2010.total.ind<-data.frame(s)
names(estv2010.total.ind)[1]=c("inc")
summary(estv2010.total.ind)
hist(estv2010.total.ind$inc)
estv2010.total.ind$inc<-estv2010.total.ind$inc/1000
estv2010.total
summary(estv2010.total.ind$inc)

# HABE - 2010 all cases 

habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe10<-habe0911 %.% 
  filter(Jahr08==2010)
habe10<-data.frame(habe10$VerfuegbaresEinkommen08)
names(habe10)<-"inc_habe"
habe10$inc_habe[habe10$inc_habe<0]<-0 # One values is -20'000
habe10$inc_habe<-habe10$inc_habe*12/1000 # transform from monthly income to income per year
summary(habe10$inc_habe)


# Probability density Function

density1 <- density(habe10$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-70, 4600),
     ylim=c(0,1.103e-02))
title(main="FTA vs HBS income distribution",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(2500,2500),y=c(1.103e-02,1.103e-02))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("HBS","FTA"))
density2 <- density(estv2010.total.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)

# Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=estv2010.total.ind$inc, yo=habe10$inc_habe,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,5.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of HBS Distribution")
title(main=paste("Overall realtive density"))
abline(h=1,lty=2)
g1A <- reldist(y=estv2010.total.ind$inc, yo=habe10$inc_habe,
               show="effect",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of Median shift"))
abline(h=1,lty=2)
gA0 <- reldist(y=estv2010.total.ind$inc, yo=habe10$inc_habe,
               smooth=0.4, ci=FALSE,
               show="residual",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of different shape"))
abline(h=1,lty=2)
par(mfrow=c(1,1))


####
# Separate comparison for unmarried


# Select married - ESTV

estv2010.married<-estv2010[c(18:38),c(1,2)]
names(estv2010.married)[2]=c("married")
##
# Simulate individual data points
# Percentile-Plot (descriptive  purpose)
# cumulative probabilites
cum.p<-c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99,99.5,99.9,99.99)/100
# probabilities
prob<-c(cum.p[1],diff(cum.p), .01)
# extreme values beyond x (to sample)
#freq<-max(normal$anz_pflichtige)
freq<-3000
init<-0
fin<-(abs(max(estv2010.married$married,na.rm=TRUE))+5)
# generate the sequence to take pairs from
ival<-c(init,estv2010.married$married,fin)
len<-10000 # sequence of each pair
s<-sapply(2:length(ival),function(i){
  seq(ival[i-1],ival[i],length.out=len)
})
s<-sample(s,freq,prob=rep(prob, each=len),replace=T)
estv2010.married.ind<-data.frame(s)
names(estv2010.married.ind)[1]=c("inc")
summary(estv2010.married.ind)
hist(estv2010.total.ind$inc)
estv2010.married.ind$inc<-estv2010.married.ind$inc/1000
estv2010.married
summary(estv2010.married.ind$inc)



# Select married - HABE

habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe10<-habe0911 %.% 
  filter(Jahr08==2010)
habe10.married<-habe0911 %.% 
  filter(Familientyp08>130,Jahr08==2010)
habe10.married<-data.frame(habe10.married$VerfuegbaresEinkommen08)
names(habe10.married)<-"inc_habe"
habe10.married$inc_habe[habe10.married$inc_habe<0]<-0 # One values is -20'000
habe10.married$inc_habe<-habe10.married$inc_habe*12/1000 # transform from monthly income to income per year

# Probability Density Function

density1 <- density(habe10.married$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-257, 8000),
     ylim=c(0,0.0101390))
title(main="FTA vs HBS income distribution",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(2500,2500),y=c(0.0101390,0.0101390))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("HBS","FTA"))
density2 <- density(estv2010.married.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)

# Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=estv2010.married.ind$inc, yo=habe10.married$inc_habe,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,5.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of HBS Distribution")
title(main=paste("Overall realtive density"))
abline(h=1,lty=2)
g1A <- reldist(y=estv2010.married.ind$inc, yo=habe10.married$inc_habe,
               show="effect",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of Median shift"))
abline(h=1,lty=2)
gA0 <- reldist(y=estv2010.married.ind$inc, yo=habe10.married$inc_habe,
               smooth=0.4, ci=FALSE,
               show="residual",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of different shape"))
abline(h=1,lty=2)
par(mfrow=c(1,1))


####
# Separate comparison for unmaried
####

##
# Unmarried ESTV

estv2010.unmarried<-estv2010[c(18:38),c(1,3)]
names(estv2010.unmarried)[2]=c("unmarried")
##
# Simulate individual data points
# Percentile-Plot (descriptive  purpose)
# cumulative probabilites
cum.p<-c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99,99.5,99.9,99.99)/100
# probabilities
prob<-c(cum.p[1],diff(cum.p), .01)
# extreme values beyond x (to sample)
#freq<-max(normal$anz_pflichtige)
freq<-3000
init<-0
fin<-(abs(max(estv2010.unmarried$unmarried,na.rm=TRUE))+5)
# generate the sequence to take pairs from
ival<-c(init,estv2010.unmarried$unmarried,fin)
len<-10000 # sequence of each pair
s<-sapply(2:length(ival),function(i){
  seq(ival[i-1],ival[i],length.out=len)
})
s<-sample(s,freq,prob=rep(prob, each=len),replace=T)
estv2010.unmarried.ind<-data.frame(s)
names(estv2010.unmarried.ind)[1]=c("inc")
summary(estv2010.unmarried.ind)
hist(estv2010.total.ind$inc)
estv2010.unmarried.ind$inc<-estv2010.unmarried.ind$inc/1000
estv2010.unmarried
summary(estv2010.unmarried.ind$inc)

# Select unmarried - HABE

habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe10<-habe0911 %.% 
  filter(Jahr08==2010)
habe10.unmarried<-habe0911 %.% 
  filter(Familientyp08<140,Jahr08==2010)
habe10.unmarried<-data.frame(habe10.unmarried$VerfuegbaresEinkommen08)
names(habe10.unmarried)<-"inc_habe"
habe10.unmarried$inc_habe[habe10.unmarried$inc_habe<0]<-0 # One values is -20'000
habe10.unmarried$inc_habe<-habe10.unmarried$inc_habe*12/1000 # transform from monthly income to income per year
summary(habe10.unmarried$inc_habe)
summary(habe10.married$inc_habe)

# Probability Density Function

density1 <- density(habe10.unmarried$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-50, 2500),
     ylim=c(0,1.764e-02))
title(main="FTA vs HBS income distribution",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(1200,1200),y=c(1.764e-02,1.764e-02))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("HBS","FTA"))
density2 <- density(estv2010.unmarried.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)

# Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=estv2010.unmarried.ind$inc, yo=habe10.unmarried$inc_habe,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,5.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of HBS Distribution")
title(main=paste("Overall realtive density"))
abline(h=1,lty=2)
g1A <- reldist(y=estv2010.unmarried.ind$inc, yo=habe10.unmarried$inc_habe,
               show="effect",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of Median shift"))
abline(h=1,lty=2)
gA0 <- reldist(y=estv2010.unmarried.ind$inc, yo=habe10.unmarried$inc_habe,
               smooth=0.4, ci=FALSE,
               show="residual",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of different shape"))
abline(h=1,lty=2)
par(mfrow=c(1,1))



####################################################
# Sind die klassierten Steuerdaten so anders als die Brülhartdaten?
#######################################################

# Brülhart
estv2010.total.ind
# ESTV
normal.ind

density1 <- density(normal.ind$inc)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-50, 5000),
     ylim=c(0,0.02))
title(main="FTA (incomes classes) vs FTA (individual data) ",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(1200,1200),y=c(0.0113410,0.0113410 ))
legend(fig1legend,lty=1:2,cex=0.5, bty="n",
       legend=c("FTA(iC)","FTA(individual"))
density2 <- density(estv2010.total.ind$inc)
lines(x = (density2$x), y = density2$y, type = "l",lty=2)

##############Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=estv2010.total.ind$inc, yo=normal.ind$inc,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,5.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of FTA (income classes)")
title(main=paste("Overall realtive density"))
abline(h=1,lty=2)
g1A <- reldist(y=estv2010.total.ind$inc, yo=normal.ind$inc,
               show="effect",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of FTA (income classes)")
title(main=paste("Effect of Median shift"))
abline(h=1,lty=2)
gA0 <- reldist(y=estv2010.total.ind$inc, yo=normal.ind$inc,
               smooth=0.4, ci=FALSE,
               show="residual",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,5.0), ylab="",
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of FTA (income classes)")
title(main=paste("Effect of different shape"))
abline(h=1,lty=2)
par(mfrow=c(1,1))




########################################################################
# Ist es möglich direkt mit den Perzentilen anstelle des simulierten Datensatzes zu rechnen
# Die Funktion Gini kann mit Einkommensklassen arbeiten
##########################################################################

# Prepare Data (Brülhart mit Nuller)
library(dplyr, quietly=TRUE, warn.conflicts = FALSE)
library(foreign)
# Ohne Nuller
bd.10<-read.csv("data/data_Schweiz_mitNull.csv", header=TRUE,sep=";")
bd.10$Jahr <- as.numeric(substr(bd.10$Veranlagungsperiode,1,4))
bd.10$Jahr[nchar(as.character(bd.10$Veranlagungsperiode))>4] <- bd.10$Jahr[nchar(as.character(bd.10$Veranlagungsperiode))>4] - 2
bd.2010<-bd.10 %.% 
  filter(Jahr==2010,Einheit=="Total")

start.p <- which(names(bd.2010)=="p1")
end.p <- which(names(bd.2010)=="p99_99")
start.stpf <- which(names(bd.2010)=="stpf_100")
end.stpf <- which(names(bd.2010)=="stpf_u2000")
percentile <- c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99,99.5,99.9,99.99)
test.0 <- data.frame(t(bd.2010[1,start.p:end.p]),percentile,t(bd.2010[1,start.stpf:end.stpf]))

bd.10<-read.csv("data/data_Schweiz_mitNull.csv", header=TRUE,sep=";")
bd.10$Jahr <- as.numeric(substr(bd.10$Veranlagungsperiode,1,4))
bd.10$Jahr[nchar(as.character(bd.10$Veranlagungsperiode))>4] <- bd.10$Jahr[nchar(as.character(bd.10$Veranlagungsperiode))>4] - 2
bd.2005<-bd.10 %.% 
  filter(Jahr==2005,Einheit=="Total")

start.p <- which(names(bd.2005)=="p1")
end.p <- which(names(bd.2005)=="p99_99")
start.stpf <- which(names(bd.2005)=="stpf_100")
end.stpf <- which(names(bd.2005)=="stpf_u2000")
percentile <- c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99,99.5,99.9,99.99)
test.comp <- data.frame(t(bd.2005[1,start.p:end.p]),percentile,t(bd.2005[1,start.stpf:end.stpf]))



library(reldist)
fig2b <- reldist(y=test.comp$X1,yo=test.0$X1,
                 yowgt=test.0$X1.1,ywgt=test.comp$X1.1,
                 ci=FALSE,smooth=0.4,
                 yolabs=seq(-1,3,by=0.5),
                 ylim=c(0,5),cex=0.8,
                 ylab="Relative Density",
                 xlab="Proportion of 2010")
title(main="Relative PDF",cex=0.6)



