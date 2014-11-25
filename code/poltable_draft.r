polTable <- function(y, yo, ywgt=1, yowgt=1,ndig=2,title="Inequality Indices") {
stopifnot(is.numeric(y), is.numeric(yo)) # all TRUE
#y vector with new distribution
#yo vector with reference distribution
#ywgt yowgt: weights to use (optional)
#ndig: digits to display, default 2
#title: table title (optional)

# load neccessary libraries
library(reldist,quietly=TRUE,warn.conflicts=FALSE) #for Latex tables
library(Hmisc,quietly=TRUE,warn.conflicts=FALSE)

# Create vector of weights 1 if nothing else is defined
if(length(ywgt)==1 & ywgt==1) ywgt <- rep(1, length(y))
if(length(yowgt)==1 & yowgt==1) yowgt <- rep(1, length(yo))

med <- rpy(y=y,yo=yo, ywgt=ywgt,yowgt=yowgt,pvalue=FALSE)[2] # Median Index, middle value is the estimate so we take [2]
lower <- rpluy(y=y,yo=yo, ywgt=ywgt,yowgt=yowgt,pvalue=FALSE)[2] # Lower Index
upper <- rpluy(y=y,yo=yo, ywgt=ywgt,yowgt=yowgt,pvalue=FALSE, upper=TRUE)[2] # Upper index
dgini <- gini(y)-gini(yo)

latex(format(c("Median Index"=med, "Lower Index"=lower, "Upper Index"=upper, "$\\Delta$ Gini"=dgini),digits=ndig),file="",title=title)
}

polTable(bd.2010.ind,bd.2003.ind)
