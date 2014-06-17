## program:     simulate_data.R
## task:        Simulate individual data points out of distribution percentiles and apply relative distribution techniques
## project:     Inequality of income and wealth in switzerland
## subproject:  Assessing inequality with tax data
## date:        June2014


##
# Prepare FTA-data for 2010 (out of Rudis code) Normal cases
normal <- read.csv("data/ginis_und_perzentile_normal.csv",sep="\t")
normal <- normal %.% select(-G_reink, -G_taxed, -ppop0, -G_steink0)
normal<-normal %.% 
  filter(kanton=="CH",steuerperiode==2010)
start <- which(names(normal)=="p1")
end <- which(names(normal)=="p95")
percentile <- c(1,5,10,20,25,30,40,50,60,70,75,80,90,95)
normal <- data.frame(t(d[1,start:end]),percentile,anz_pflichtige=89817)
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
  seq(ival[i-1],ival[1],length.out=len)
})
s<-sample(s,freq,prob=rep(prob, each=len),replace=T)
normal.ind<-data.frame(s)
names(normal.ind)[1]=c("inc")
summary(normal.ind)
hist(normal.ind$inc)
normal.ind$inc<-normal.ind$inc*7 # Rescale income 
hist(normal.ind$inc)

##
# Prepare HABE Data - 2010
# ATTENTION: not weighted and with disposable income
habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
habe10<-habe0911 %.% 
  filter(Jahr08==2010)
habe10<-data.frame(habe10$VerfuegbaresEinkommen08)
names(habe10)<-"inc_habe"
habe10$inc_habe[habe10$inc_habe<0]<-0 # One values is -20'000
habe10$inc_habe<-habe10$inc_habe*13/1000 # transform from monthly income to income per year

# Alternative mit Bruttoeinkommen
#habe0911<-read.table("P:/WGS/FBS/ISS/Projekte laufend/SNF Ungleichheit/Datengrundlagen/HABE/2009 bis 2011/HABE091011_Standard_130717UOe.txt", header=TRUE)
#habe10<-habe0911 %.% 
#  filter(Jahr08==2010)
#habe10<-data.frame(habe10$Bruttoeinkommen08)
#names(habe10)<-"inc_habe"
#habe10$inc_habe<-habe10$inc_habe*13/1000 # transform from monthly income to income per year

##
# Apply Relative Distribution Methods
library(reldist)


##
# Graphical Analysis 1


# Compare the distributions
# Traditional Approach - probability density function


density1 <- density(habe10$inc_habe)
plot(x = (density1$x), y = density1$y, type = "l",
     xlab = "Einkommen", ylab = "density",
     axes = FALSE,
     xlim = c(-70, 900),
     ylim=c(0,8.652e-03))
title(main="FTA vs HBS income distribution",cex=0.6)
axis(side = 1)
axis(side = 2)
fig1legend <- list(x=c(400,400),y=c(8.652e-03,8.652e-03))
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
                 ylim=c(0,2.5),cex=0.8,
                 ylab="Relative Density",
                 xlab="Proportion of HBS Distribution")
title(main="Relative PDF",cex=0.6)

##
# Decomposing the Relative Distribution

par(mfrow=c(1,3))
g10 <- reldist(y=normal.ind$inc, yo=habe10$inc_habe,
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               ylim=c(0.5,3.0),
               bar=TRUE, quiet=FALSE,
               xlab="Proportion of HBS Distribution")
title(main=paste("Overall realtive density",cex=0.6)
abline(h=1,lty=2)
g1A <- reldist(y=normal.ind$inc, yo=habe10$inc_habe,
               show="effect",
               bar=TRUE, quiet=FALSE,
               ylim=c(0.5,3.0), ylab="",
               smooth=0.4, ci=FALSE,
               yolabs=seq(-1,3,by=0.5),
               xlab="Proportion of HBS Distribution")
title(main=paste("Effect of Median shift",cex=0.6)
abline(h=1,lty=2)
gA0 <- reldist(y=normal.ind$inc, yo=habe10$inc_habe,
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

# It seems, that the function needs weigths, so give it some weights...

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


