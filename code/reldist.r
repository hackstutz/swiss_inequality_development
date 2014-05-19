##############################################################
### Libraries ################################################
##############################################################

library(foreign)
library(ggplot2)
library(dplyr)
library(rgl)
library(reldist)

##############################################################
### Setup Data ###############################################
##############################################################

setwd("C:/Users/Hackstutz/Dropbox/Git/swiss_inequality_development/")
estv<-read.dta("data/ginis_und_perzentile.dta")

cpi <- read.csv('http://www.quandl.com/api/v1/datasets/WORLDBANK/CHE_FP_CPI_TOTL_ZG.csv?&trim_start=1961-12-31&trim_end=2011-12-31&sort_order=desc', colClasses=c('Date'='Date'))
cpi$year <- as.numeric(substr(as.character(cpi$Date),1,4))
cpi <- cpi[order(cpi$year),]
cpi$cumInf <- cumprod(1+cpi$Value/100)
# Referenzzeitpunkt 2009
referenz09 <- cpi$cumInf[cpi$year==2009]
estv$jahr <- ifelse(nchar(estv$steuerperiode)>4, estv$steuerperiode-0.5, estv$steuerperiode)
estv$cumInf <- cpi$cumInf[match(estv$jahr,cpi$year)]
estv[,which(names(estv)=="p1"):which(names(estv)=="p99")] <- estv[,which(names(estv)=="p1"):which(names(estv)=="p99")] / estv$cumInf*referenz09

getDistribution <- function(years=2008, kanton="CH", data=estv) {
	percentiles <- data %.% 
		filter(kanton=="CH", steuerperiode%in%years) %.%
		select(p1:p99)
	res<-data.frame(t(rbind(as.numeric(gsub("[^0-9]","",names(percentiles))),percentiles)))
	names(res)<-c("percentiles",as.character(years))
	res
}
getDistribution(2003:2005)

##############################################################
### Analyses #################################################
##############################################################

relDistribution <- function(years=c(2003,2004,2005),rel=2004) {
	stopifnot(rel%in%years,is.numeric(years))
	data<-getDistribution(years=years)
	data[,-1] <- data[,-1]/data[,as.character(rel)]
	reshape(data,direction="long",timevar="year",varying=list(names(data)[-1]), times=names(data)[-1],v.names="value")
}
relDistribution()


plotReldist <- function(years=c(2003,2004,2005),rel=2004) {
	long <- relDistribution(years,rel)
	p <- ggplot(long, aes(y=value, x=percentiles,colour=year, group=year)) + geom_line()
	print(p)
}
plotReldist()

#bla <- relDistribution()#

#x <- as.numeric(bla$year)
#y <- bla$percentiles
#z <- c(sin(x) %*% t(sin(y)))
#persp3d(x, y, z)


##############################################################
### ToDo #####################################################
##############################################################

# 3d plot

##############################################################
### Export Results ###########################################
##############################################################
