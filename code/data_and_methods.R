
## ----setup---------------------------------------------------------------
library(foreign)
df <- read.dta("../../data/ginis_und_perzentile.dta")
df$zeros <- df$null_norm/df$cpop # i only use normal cases here


## ----zero_descriptives---------------------------------------------------
library(ggplot2)
ggplot(df[df$steuerperiode>=1995,], aes(x=steuerperiode,y=zeros))+geom_line()+facet_wrap(~kanton)
ggplot(df[df$steuerperiode>=1995,], aes(x=steuerperiode,y=zeros))+geom_line()+facet_wrap(~kanton)+geom_line(aes(y=G_steink),color="blue")


## ----corrected_gini------------------------------------------------------
df$J<-factor(df$steuerperiode)
fit <- lm(G_steink~kanton+kanton:zeros+J+zeros:J,data=df)
#summary(fit)
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

library(grid) #for arrow() function
SmoothCoefficientPlot(fit,modelnames=paste0("By cantons: Impact of the share of zeros on inequality (Gini) Rsq=", round(summary(fit)$r.squared,2)))+geom_text(show_guide=FALSE,x=77,y=2.2,size=4,label="effect variation\nover time")+geom_segment(aes(x = 85, y = 1.5, xend = 71, yend = 1.5))+geom_segment(aes(x = 71, y = 1.5, xend = 71, yend = 1.3))+geom_segment(aes(x = 85, y = 1.5, xend = 85, yend = 1.3))+geom_text(show_guide=FALSE,x=57,y=1.9,size=4,label="effect variation\nby cantons")+geom_segment(aes(x = 71, y = 1.2, xend = 45, yend = 1.2))+geom_segment(aes(x = 71, y = 1.2, xend = 71, yend = 1.0))+geom_segment(aes(x = 45, y = 1.2, xend = 45, yend = 1.0))+ggtitle("Bias variation by time and cantons")



## ----ftest---------------------------------------------------------------
library(survey)
regTermTest(fit,"kanton:zeros")


## ----magnitude_of_bias---------------------------------------------------
reduced_fit <- lm(G_steink~kanton+factor(steuerperiode),data=df)
#summary(reduced_fit)


