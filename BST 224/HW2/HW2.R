
setwd("C:\\Users\\15302\\Desktop\\BST 224\\")
ori_anx_data = read.csv("anx.csv")
anx_data = na.omit(ori_anx_data)

new_data = anx_data
new_data$time = as.factor(new_data$time)
new_data$group = as.factor(new_data$group)

model = lm(anx ~ time : group, data = new_data)
new_data$resi = residuals(model)

## correlation matrix construction
subset = new_data[,c(1,4,9)]
wide<-reshape(subset,v.names="resi",idvar="id",timevar="time",direction="wide")
cor(wide[,-1],use="pairwise.complete.obs")

### Autocorrelation 
# This dataSet is a data.frame and only contains : id, time and resi 3 variables
auto_correlation = function(dataSet){
  idUni = unique(dataSet$id)
  lag = c()
  resi1 = c()
  resi2 = c()
  for (i in 1:length(idUni)){
    thisID_data = dataSet[which(dataSet$id == i),]
    thisTime = as.numeric(as.character(thisID_data$time))
    thisResi = thisID_data$resi
    for (j in 1:(length(thisTime) - 1)){
      for (k in (j + 1):length(thisTime)){
        lag = c(lag, abs(thisTime[j] - thisTime[k]))
        resi1 = c(resi1, thisResi[j])
        resi2 = c(resi2, thisResi[k])
      }
    }
  }
  allData = data.frame(lag = lag,resi1 = resi1,resi2 = resi2)
  print(allData)
  persons = c()
  tl = c()
  uniLag = sort(unique(allData$lag))
  for (oneLag in uniLag){
    thisData = allData[which(allData$lag == oneLag),]
    persons = c(persons,cor(thisData$resi1,thisData$resi2))
    tl = c(tl,1.96 / sqrt(dim(thisData)[1]))
  }
  return(data.frame(lag = uniLag,ACF = persons,TL = tl))
}
library("cowplot")
library("ggplot2")
library(tidyverse)
library(hrbrthemes)
library(viridis)
finalResult = auto_correlation(subset)
finalTL = finalResult$TL
finalResult = finalResult[,-3]
tlData = data.frame(lag = finalResult$lag,ACF = finalTL)
finalResult = rbind(finalResult,tlData)
finalResult$Legend = c(rep("Autocorrelation function",length(finalTL)),
                       rep("95% TL",length(finalTL)))
p = ggplot(data = finalResult,mapping = aes(x = lag,y = ACF,colour = Legend)) + 
  geom_line() + 
  geom_point(shape=21 ,size=2,color="black", fill="#69b3a2",) + 
  xlab("Time Lag") + ylab("ACF") + ggtitle("Correlogram.")

p


## (b)
library(nlme)
library(lme4)
b_data = anx_data
b_data$id = as.factor(b_data$id)
b_data$group = as.factor(b_data$group)
b_data$time = as.factor(b_data$time)
ar1 = lme(anx~time * group,random = ~1|id ,data = b_data,correlation = corAR1())
exchange = lme(anx~time * group,random = ~1|id ,data = b_data,correlation = corCompSymm(0.5))
summary(ar1)
summary(exchange)
gls_ar1 = gls(anx ~ time * group,data = b_data,correlation = corAR1())

