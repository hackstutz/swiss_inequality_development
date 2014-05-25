
## ----options, results='hide',echo=FALSE----------------------------------
opts_chunk$set(out.width = '800px', dev='svg', out.height = '500px',fig.width=10,fig.height=6.25,cache=TRUE, echo=FALSE, warning=FALSE)


## ----datenaufbereitung, echo=FALSE, results='hide',warning=FALSE,--------
library(plm)
library(foreign)
library(ggplot2)
setwd("C:/Users/Hackstutz/Dropbox/")
daten <- read.dta("PX-Daten/kontextvariablen0.02.dta")

cpi <- read.csv('http://www.quandl.com/api/v1/datasets/WORLDBANK/CHE_FP_CPI_TOTL_ZG.csv?&trim_start=1961-12-31&trim_end=2011-12-31&sort_order=desc', colClasses=c('Date'='Date'))
cpi$year <- as.numeric(substr(as.character(cpi$Date),1,4))
cpi <- cpi[order(cpi$year),]
cpi$cumInf <- cumprod(1+cpi$Value/100)
# Referenzzeitpunkt 2009 (für die Interpretation der y-Skalen)
referenz09 <- cpi$cumInf[cpi$year==2009]

daten$kj <- paste(daten$Kantonname, daten$Jahr)
daten$bev <- daten$frau_ausl+daten$frau_schw+daten$mann_schw+daten$mann_ausl
kbev<-aggregate(bev~Kantonname+Jahr, data=daten,FUN="sum")
kbev$kj <- paste(kbev$Kantonname, kbev$Jahr)
daten$kanton_bev<-kbev[match(daten$kj,kbev$kj),"bev"]
  
daten <- within(daten, {
  cumInf <- cpi[match(Jahr, cpi$year),"cumInf"]/referenz09
  real_mean_reink <- mean_reink / cumInf
  real_median_reink <- median_reink / cumInf
  real_mean_steink <- mean_steink / cumInf
  real_median_steink <- median_steink / cumInf
  real_mean_reinka <- mean_reinka / cumInf
  real_median_reinka <- median_reinka / cumInf
  real_mean_steinka <- mean_steinka / cumInf
  real_median_steinka <- median_steinka / cumInf
  bev <- frau_ausl+frau_schw+mann_schw+mann_ausl
  ausl <- (frau_ausl+mann_ausl)/bev
  maenner <- (mann_ausl+mann_schw)/bev
  primp <- prim/bev
  sekp <- sek/bev
  tertp <- tert/bev
  bild <- (bildungsausgaben/kanton_bev)/cumInf
  uni <- (bachelor+master+diplom+bachelor_fh+master_fh+diplom_fh)/kanton_bev
})
#



## ----qplot, warning=FALSE,eval=FALSE,echo=FALSE--------------------------
qplot(Jahr,bild,data=daten[daten$Jahr>1990,],geom="line")+facet_wrap(~Kantonname,ncol=7)+ggtitle("Bildungsausgaben pro Kopf nach Kantonen\nPreisnveau 2009")
qplot(Jahr,uni,data=daten[daten$Jahr>1999,],geom="line")+facet_wrap(~Kantonname,ncol=7)+ggtitle("Hochschulabschlüsse pro Kopf und Jahr nach Kantonen")


## ----typologisierung_aufbereitung,,echo=FALSE,results='hide'-------------
setwd("C:/Users/Hackstutz/Dropbox/")
kdaten <- read.dta("Ginis/data/data_Kantone.dta")
kdaten$Jahr <- as.numeric(substr(kdaten$Veranlagungsperiode,1,4))
### Veranlagungsperiode anpassen ###
kdaten$Jahr[nchar(as.character(kdaten$Veranlagungsperiode))>4] <- kdaten$Jahr[nchar(as.character(kdaten$Veranlagungsperiode))>4] - 2
# Basel
kdaten$Jahr[kdaten$Kantonname=="BS" & kdaten$Veranlagungsperiode=="1997_1998"] <- 1997
kdaten$kj<-paste(kdaten$Kantonname, kdaten$Jahr)
kdaten$kanton_bev <- daten[match(kdaten$kj,daten$kj),"kanton_bev"]

kdaten <- within(kdaten, {
  cumInf <- cpi[match(Jahr, cpi$year),"cumInf"]/referenz09
  real_mean_reink <- mean_reink / cumInf
  real_median_reink <- median_reink / cumInf
  real_mean_steink <- mean_steink / cumInf
  real_median_steink <- median_steink / cumInf
  real_mean_reinka <- mean_reinka / cumInf
  real_median_reinka <- median_reinka / cumInf
  real_mean_steinka <- mean_steinka / cumInf
  real_median_steinka <- median_steinka / cumInf
})

#  zwei Zeitpunkte Daten
daten2009 <- subset(kdaten, (Jahr==2009)&Einheit=="Total",select=c("Kantonnr","Kantonname","real_mean_steink","real_median_steink","gini_steink","Jahr",names(kdaten)[22:39],"cumInf"))
daten1979 <- subset(kdaten, (Jahr==1979)&Einheit=="Total",select=c("Kantonnr","Kantonname","real_mean_steink","real_median_steink","gini_steink","Jahr",names(kdaten)[22:39],"cumInf"))

# die perzentile sind noch nicht inflationsbereinigt:
daten2009[,7:24]<-daten2009[,7:24]/daten2009$cumInf
daten1979[,7:24]<-daten1979[,7:24]/daten1979$cumInf

diffdaten <- cbind(daten2009[,1:2],daten2009[,3:25]-daten1979[,3:25])
# in logdiffdaten sind noch 0->Inf, braucht ggf eine Lösung
logdiffdaten <- cbind(daten2009[,1:2],log(daten2009[,3:25])-log(daten1979[,3:25]))


## ----typoranking, dev='svg', echo=FALSE----------------------------------
rankdata <- daten1979
rankdata$gini_steink09 <- daten2009$gini_steink[match(rankdata$Kantonname,daten2009$Kantonname)]
rankdata <- rankdata[order(rankdata$gini_steink),]
rankdata$change <- ifelse(rankdata$gini_steink09<rankdata$gini_steink,"down","up")
ggplot(data=rankdata, aes(y=gini_steink,ymin=gini_steink, ymax=gini_steink09, x = reorder(Kantonname,gini_steink),color=change)) + geom_pointrange(size=1) + scale_x_discrete('') + scale_y_continuous('Change in Gini 1979 to 2009') + theme_bw()


## ----subsampling,, echo=FALSE--------------------------------------------
fulldaten <- daten
daten <- daten[!is.na(match(daten$Kantonname,c("SZ","GE","ZH","FR","BE"))),]
kdaten <- kdaten[!is.na(match(kdaten$Kantonname,c("SZ","GE","ZH","FR","BE"))),]
fulldiffdaten <-logdiffdaten
logdiffdaten <- logdiffdaten[!is.na(match(logdiffdaten$Kantonname,c("SZ","GE","ZH","FR","BE"))),]


## ----typocolor,, echo=FALSE,eval=FALSE-----------------------------------
qplot(gini_steink,gini_steink09-gini_steink, data=rankdata,geom="text",label=Kantonname,color=change) + geom_vline(xintercept=median(rankdata$gini_steink), linetype="dashed") + geom_hline(yintercept=median(rankdata$gini_steink09-rankdata$gini_steink), linetype="dotted")+ geom_hline(yintercept=0, linetype="dashed") + theme_bw()


## ----mean_over_time,echo=FALSE-------------------------------------------
ggplot(kdaten, aes(x=Jahr,y=real_mean_steink,group=Einheit,color=Einheit))+geom_line(size=1)+facet_wrap(~Kantonname,ncol=7)+theme_bw()


## ----gini_over_time, , echo=FALSE----------------------------------------
ggplot(kdaten, aes(x=Jahr,y=gini_steink,group=Einheit,color=Einheit))+geom_line(size=1)+facet_wrap(~Kantonname,ncol=7)+theme_bw()


## ----theilindex,,eval=FALSE,echo=FALSE-----------------------------------
library(ineq)
# Theil-Indizes für Jahre und Kantone
theils <- aggregate(real_mean_reinka~Kantonname+Jahr, data=daten,FUN=Theil)
qplot(Jahr,real_mean_reink,data=theils,geom="line")+facet_wrap(~Kantonname)+ggtitle("Entwicklung des Theil-Index \nnach Kantonen")


## ----p9010_p8020,,echo=FALSE---------------------------------------------
# die perzentile sind noch nicht inflationsbereinigt:
kdaten[,22:42]<-kdaten[,22:42]/kdaten$cumInf
kdaten <- within(kdaten,{
  p9505 <- p95/p5
  p9010 <- p90/p10
  p8020 <- p80/p20
})
#ggplot(kdaten[kdaten$Einheit=="Total",], aes(x=Jahr))+geom_line(aes(y=p8020),colour="blue")+facet_wrap(~Kantonname)+ggtitle("Perzentilverhältnis 80/20 nach Kantonen")
#ggplot(kdaten[kdaten$Einheit=="Total",], aes(x=Jahr))+geom_line(aes(y=p9010),colour="blue")+facet_wrap(~Kantonname)+ggtitle("Perzentilverhältnis 90/10 nach Kantonen")
#ggplot(kdaten[kdaten$Einheit=="Total",], aes(x=Jahr))+geom_line(aes(y=p9505),colour="blue")+facet_wrap(~Kantonname)+ggtitle("Perzentilverhältnis 95/5 nach Kantonen")


## ----verteilungen2009,, echo=FALSE---------------------------------------
long2009 <- reshape(daten2009[!is.na(match(daten2009$Kantonname,c("SZ","GE","ZH","FR","BE"))),], direction="long",idvar="Kantonname",v.names="real_income",varying=list(names(daten2009)[7:24]))

percentilmatch<-data.frame(1:18, c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99))
names(percentilmatch)<-c("time","perc")
long2009$perzentile <- percentilmatch[match(long2009$time,percentilmatch$time),"perc"]

ggplot(long2009,aes(x=perzentile,y=real_income))+geom_line(colour="turquoise",size=1) + geom_hline(yintercept=0,colour="red", linetype="dashed",size=1)+facet_wrap(~Kantonname,ncol=7)+xlab("Percentiles") + theme_bw()



## ----verteilungen1979,, echo=FALSE---------------------------------------
long1979 <- reshape(daten1979[!is.na(match(daten1979$Kantonname,c("SZ","GE","ZH","FR","BE"))),], direction="long",idvar="Kantonname",v.names="real_income",varying=list(names(daten1979)[7:24]))

percentilmatch<-data.frame(1:18, c(1,5,10,20,25,30,40,50,60,70,75,80,90,95,96,97,98,99))
names(percentilmatch)<-c("time","perc")
long1979$perzentile <- percentilmatch[match(long1979$time,percentilmatch$time),"perc"]

long2009_1979 <- rbind(long2009,long1979)
ggplot(long2009_1979,aes(x=perzentile,y=real_income,group=factor(Jahr),colour=factor(Jahr)))+geom_line(size=1) + geom_hline(yintercept=0,colour="red", linetype="dashed",size=1)+facet_wrap(~Kantonname,ncol=7)+xlab("Percentiles") + theme_bw()


## ----perzentilsdifferenz,,echo=FALSE-------------------------------------
# ins Longformat:
long_logdiffdaten <- reshape(logdiffdaten, direction="long",idvar="Kantonname",v.names="log_diff",varying=list(names(logdiffdaten)[7:24]))
long_logdiffdaten$perzentile <- percentilmatch[match(long_logdiffdaten$time,percentilmatch$time),"perc"]
long_logdiffdaten<-subset(long_logdiffdaten, perzentile>1)

# statt 0 linie das durchschnittswachstum:
gline<-weighted.mean((kdaten$real_mean_steink[kdaten$Einheit=="Total"&kdaten$Jahr==2009] - kdaten$real_mean_steink[kdaten$Einheit=="Total"&kdaten$Jahr==1979])/kdaten$real_mean_steink[kdaten$Einheit=="Total"&kdaten$Jahr==1979],kdaten$kanton_bev[kdaten$Einheit=="Total"&kdaten$Jahr==2009])
 

ggplot(long_logdiffdaten,aes(x=perzentile,y=log_diff))+geom_line(colour="turquoise",size=1) + geom_hline(yintercept=gline,colour="red", linetype="dashed",size=1)+facet_wrap(~Kantonname,ncol=7)+xlab("Percentiles") + theme_bw()


## ----perzentilsdifferenz2, , echo=FALSE----------------------------------
long_logdiffdaten2<-subset(long_logdiffdaten, perzentile>5)
ggplot(long_logdiffdaten2,aes(x=perzentile,y=log_diff))+geom_line(colour="turquoise",size=1) + geom_hline(yintercept=gline,colour="red", linetype="dashed",size=1)+facet_wrap(~Kantonname,ncol=7)+xlab("Percentiles") + theme_bw()


## ----perzentilsquotient.js,results='asis',eval=FALSE,echo=FALSE----------
#klappt nicht wirklich
library(googleVis)
wide_diffdaten<-reshape(long_logdiffdaten2,direction="wide",idvar="Kantonname")
p<-gvisLineChart(data = wide_diffdaten, yvar=c("log_diff.3","log_diff.4","log_diff.5","log_diff.6","log_diff.7","log_diff.8"), xvar=c("Kantonnr.10"))
plot(p)



## ----ols, results='asis',,echo=FALSE,results='hide',eval=FALSE-----------
fit.ols <- lm(gini_steink~real_mean_steink+real_median_steink, data=daten)
library(xtable)
print(xtable(fit.ols), type="html")


## ----xtreg, echo=FALSE,results='hide',,echo=FALSE,results='hide',eval=FALSE----
xtreg <- function(formula,data,index) {
  
  md <- model.frame(formula, data)
  X <- model.matrix(formula, md)[,-1]
  y <- model.extract(md,"response")
  
  index <- data[,index]
  X <- X-apply(X,2,ave,index)
  y <- y-ave(y,index)
  fit <- lm(y~X+0)
  fit$df.residual<-fit$df.residual-(length(unique(index)))
  fit
}


## ----fe, results='asis',,results='hide',echo=FALSE,eval=FALSE------------
fit.fe <- plm(gini_steink~real_mean_steink+real_median_steink, data=daten,index="Gemeindenr")
library(xtable)
print(xtable(fit.fe), type="html")



## ----smoothcoefplot, results='hide', echo=FALSE,,warning=FALSE-----------
SmoothCoefficientPlot <- function(models, modelnames = "", removeintercept = FALSE){
  # models must be a list()
 
  Alphas <- seq(1, 99, 2) / 100
 
  Multiplier <- qnorm(1 - Alphas / 2)
  zzTransparency <<- 1/(length(Multiplier)/4)
  #CoefficientTables <- lapply(models, function(x){summary(x)$coef})
  CoefficientTables <- summary(models)$coef
  #TableRows <- unlist(lapply(CoefficientTables, nrow))
  TableRows <- nrow(CoefficientTables)
 
  if(modelnames[1] == ""){
    ModelNameLabels <- rep(paste("Model", 1:length(TableRows)), TableRows)
    } else {
    ModelNameLabels <- rep(modelnames, TableRows)
    }
 
  MatrixofModels <- cbind(do.call(rbind, list(CoefficientTables)), ModelNameLabels)
  if(removeintercept == TRUE){
    MatrixofModels <- MatrixofModels[!rownames(MatrixofModels) == "(Intercept)", ]
    }
  MatrixofModels <- data.frame(cbind(rownames(MatrixofModels), MatrixofModels))
 
  MatrixofModels <- data.frame(cbind(MatrixofModels, rep(Multiplier, each = nrow(MatrixofModels))))
 
  colnames(MatrixofModels) <- c("IV", "Estimate", "StandardError", "TValue", "PValue", "ModelName", "Scalar")
  MatrixofModels$IV <- factor(MatrixofModels$IV, levels = MatrixofModels$IV)
  MatrixofModels[, -c(1, 6)] <- apply(MatrixofModels[, -c(1, 6)], 2, function(x){as.numeric(as.character(x))})
  MatrixofModels$Emphasis <- by(1 - seq(0, 1, length = length(Multiplier) + 1)[-1], as.character(round(Multiplier, 5)), mean)[as.character(round(MatrixofModels$Scalar, 5))]
 
  OutputPlot <- qplot(data = MatrixofModels, x = IV, y = Estimate,
   ymin = Estimate - Scalar * StandardError, ymax = Estimate + Scalar * StandardError,
   ylab = NULL, xlab = NULL, alpha = I(zzTransparency), colour = I(gray(0)), geom = "blank")
  OutputPlot <- OutputPlot + geom_hline(yintercept = 0, lwd = I(7/12), colour = I(hsv(0/12, 7/12, 7/12)), alpha = I(5/12))
  OutputPlot <- OutputPlot + geom_linerange(data = MatrixofModels, aes(size = as.integer(1/Emphasis)), alpha = I(zzTransparency), colour = I(gray(0)))
  OutputPlot <- OutputPlot + scale_size_continuous(legend = FALSE)
  OutputPlot <- OutputPlot + facet_grid(~ ModelName) + coord_flip() + geom_point(aes(x = IV, y = Estimate), colour = I(gray(0))) + theme_bw()
  return(OutputPlot)
  }


## ----coefplots,,warning=FALSE,echo=FALSE---------------------------------
daten <- within(daten, {
  # Skalieren
  Gini <- scale(gini_steink)
  Mean <- scale(real_mean_steink)
  Median <- scale(real_median_steink)
  Prim <- scale(primp)
  Sek <- scale(sekp)
  Tert <- scale(tertp)
  Uni <- scale(uni)
  Bild <- scale(bild)
  J<-factor(Jahr)
})

fulldaten <- within(fulldaten, {
  # Skalieren
  Gini <- scale(gini_steink)
  Mean <- scale(real_mean_steink)
  Median <- scale(real_median_steink)
  Prim <- scale(primp)
  Sek <- scale(sekp)
  Tert <- scale(tertp)
  Uni <- scale(uni)
  Bild <- scale(bild)
  J<-factor(Jahr)
})

kdaten$kj <- paste(kdaten$Kantonname, kdaten$Jahr)
kdaten$bild<-daten$bild[match(kdaten$kj,daten$kj)]
kdaten$uni<-daten$uni[match(kdaten$kj,daten$kj)]
kdaten <- within(kdaten, {
  Gini <- scale(gini_steink)
  Mean <- scale(real_mean_steink)
  Median <- scale(real_median_steink)
  Uni <- scale(uni)
  Bild <- scale(bild)
  J<-factor(Jahr)
})

plm_daten <- subset(daten,select=c("Gini","Mean","Median","J","primp","sekp","tertp","Gemeindenr","Kantonname"))
full_plm_daten <- subset(fulldaten,select=c("Gini","Mean","Median","J","primp","sekp","tertp","Gemeindenr","Kantonname","maenner","ausl","Bild","Uni"))


## ----bycanton.plot,,warning=FALSE,echo=FALSE-----------------------------
fit.canton <- plm(Gini~(Mean+Median):Kantonname+J, data=plm_daten,index="Gemeindenr")
SmoothCoefficientPlot(fit.canton,modelnames=paste0("By cantons: Impact of real mean and median income on inequality (Gini) Rsq-within=", round(r.squared(fit.canton),2)))


## ----sektorenmodell.fit,,echo=FALSE--------------------------------------
fit.sekt <- plm(Gini~Mean+Median+J+J:(primp+sekp+tertp),data=full_plm_daten,index="Gemeindenr")


## ----byyear.plot,,warning=FALSE,echo=FALSE,fig.width=16,fig.height=10----
fit.time <- plm(Gini~(Mean+Median):J+J, data=subset(plm_daten,J!=1996),index="Gemeindenr")
SmoothCoefficientPlot(fit.time,modelnames=paste0("By years: Impact of real mean and median income on inequality (Gini) Rsq-within=", round(r.squared(fit.time),2)))


## ----sektorenmodell.plot,,echo=FALSE,warning=FALSE-----------------------
SmoothCoefficientPlot(fit.sekt,modelnames=paste0("Main effects: Impact of sectoral change on inequality (Gini) Rsq-within=", round(r.squared(fit.sekt),2)))


## ----bildung,,warnings=FALSE,eval=FALSE,echo=FALSE-----------------------
fit.bild <- plm(Gini~Mean+Median+J:(Bild+Uni)+J,data=kdaten,index="Kantonnr")
SmoothCoefficientPlot(fit.bild,modelnames="Einfluss von Bildungsvariablen und Immigration auf Ungleichheit (Gini)")


## ----fe_other------------------------------------------------------------
fit.full <- plm(Gini~poly(Mean,2)+poly(Median,2)+J+ausl+maenner+Bild+Uni+Kantonname,data=full_plm_daten,index="Gemeindenr")
#SmoothCoefficientPlot(fit.full,modelnames=paste0("Impact of several other variables, Rsq-within=", round(r.squared(fit.full),2)))


## ----fe_other_table, results='asis'--------------------------------------
library(xtable)
xtable.plm <- xtable:::xtable.lm
print(xtable(fit.full), type="html")


## ----allcantons_percentiles, results='asis'------------------------------
long_fulldiffdaten <- reshape(fulldiffdaten, direction="long",idvar="Kantonname",v.names="log_diff",varying=list(names(fulldiffdaten)[7:24]))
long_fulldiffdaten$perzentile <- percentilmatch[match(long_fulldiffdaten$time,percentilmatch$time),"perc"]
long_fulldiffdaten<-subset(long_fulldiffdaten, perzentile>5)

ggplot(long_fulldiffdaten,aes(x=perzentile,y=log_diff))+geom_line(colour="turquoise",size=1) + geom_hline(yintercept=gline,colour="red", linetype="dashed",size=1)+facet_wrap(~Kantonname,ncol=7)+xlab("Percentiles") + theme_bw()



