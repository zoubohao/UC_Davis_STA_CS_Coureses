library("readr")
setwd("C:\\Users\\15302\\Desktop\\BST 224\\")
data = read.table("afcr.raw",header = F)
colnames(data) = c("id","time","AFCR","group","priorTI","age")

### cowswide <- reshape(cows, idvar="id", 
### v.names="prot", timevar="week", direction="wide", times=c(1:19))

### convert to wide data set and explore the x-covarites distribution
### Explore the x-covariates first.
wideData = reshape(data,idvar = "id", v.names = "AFCR" , 
                   timevar = "time",direction = "wide", times = c(0, 3, 6, 9, 12, 15,18))
library("cowplot")
library("ggplot2")
library(tidyverse)
library(hrbrthemes)
library(viridis)
ageP = ggplot() + geom_histogram(data = wideData, aes(x = age) ,
                 binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("The distribution of age")

priorONum = length(which(wideData$priorTI == 0))
prior1Num = 150 - priorONum
priorDis = data.frame(group = c("No prior treatment", "Prior treatment"),
                      value = c(priorONum,prior1Num))
priorP = ggplot(priorDis,aes(x="",y=value,fill = group)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + ggtitle("The distribution of prior treatment") 
plot_grid(ageP,priorP,ncol = 2)
  
### At this step, we can explore the time variable and Y variable
timeVariableP =  ggplot(data = data,aes(x = time, y = AFCR,fill = as.factor(data$time)))+ 
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  geom_jitter(color="black", size=0.1, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Time Variable boxplot") +
  xlab("Discreated Time Points") + ylab("Sqrted AFCR Value")
timeVariableP

obs = data.frame(group = c("AZ","AZ + MP"),value = c(429,418))
obsInGr = priorP = ggplot(obs,aes(x="",y=value,fill = group)) + 
  geom_bar(stat="identity", width=1, color="white") +coord_polar("y", start=0)+
   ggtitle("The distribution of observarions in each group") 
obsInGr

### Explore the tendency of response variable and time
g1 = list()
g2 = list()
timeLevel = sort(unique(data$time))
for (i in 1:length(timeLevel)){
  g1[[i]] = data$AFCR[which(data$time == timeLevel[i] & data$group == 1)]
  g2[[i]] = data$AFCR[which(data$time == timeLevel[i] & data$group == 2)]
}
x1 = list()
x2 = list()
for (i in 1:length(timeLevel)){
  x1[[i]] = seq(from = timeLevel[i],to = timeLevel[i] + 3,by = 3 / length(g1[[i]]))
  x1[[i]] = x1[[i]][-length(x1[[i]])]
  x2[[i]] = seq(from = timeLevel[i],to = timeLevel[i] + 3,by = 3 / length(g2[[i]]))
  x2[[i]] = x2[[i]][-length(x2[[i]])]
}
x1Axis = c()
x2Axis = c()
for (i in 1:length(timeLevel)){
  x1Axis = c(x1Axis, x1[[i]])
  x2Axis = c(x2Axis, x2[[i]])
}
y1Axis = c()
y2Axis = c()
for (i in 1:length(timeLevel)){
  y1Axis = c(y1Axis, g1[[i]])
  y2Axis = c(y2Axis, g2[[i]])
}


dataG1 = data.frame( Month = x1Axis, AFCR = y1Axis)
library("KernSmooth")
z = locpoly(as.matrix(x1Axis),as.matrix(y1Axis),bandwidth = 2)
kerData = data.frame(x = z$x, y = z$y)
trend1 = ggplot(dataG1,aes(x =Month,y=AFCR)) + geom_point() + 
  geom_point(data = kerData,aes(x = x,y=y),color="red") + 
  ggtitle("Trend of Time and AFCR with treatment AZ")

dataG2 = data.frame( Month = x2Axis, AFCR = y2Axis)
library("KernSmooth")
z = locpoly(as.matrix(x2Axis),as.matrix(y2Axis),bandwidth = 2)
kerData = data.frame(x = z$x, y = z$y)
trend2 = ggplot(dataG2,aes(x =Month,y=AFCR)) + geom_point() + 
  geom_point(data = kerData,aes(x = x,y=y),color="blue") + 
  ggtitle("Trend of Time and AFCR with treatment AZ + MP")
plot_grid(trend1,trend2,ncol = 2)

### Explore the variance of the response variable as a function of time.
### Variance of  Y 
varY = c()
for (i in 1:length(timeLevel)){
  varY = c(varY,sd(data$AFCR[which(data$time == timeLevel[i])]))
}
timeX = timeLevel
varYTimeModel = lm(y ~ x,data = data.frame(y = varY,x = timeX))
plot(varYTimeModel,which = 1)
RVSF = ggplot(data.frame(x = timeX,y = varY),aes(x =x ,y = y)) + 
geom_line(color = "grey") + 
geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
ggtitle("Variance VS Time") + xlab("Time") + ylab("Variance of AFCR")
RVSF

#### explore the correlation structure of the response variable using correlation matrices and the sample autocorrelation
####function.

newData = data
newData$time = as.factor(newData$time)
newData = newData[,-1]
newData$group = as.factor(newData$group)
newData$priorTI = as.factor(newData$priorTI)
#  first remove the time trends by building a model 
#  and get the residuals of this model
newData$AFCR_RmT = newData$AFCR
model = lm(AFCR~time * group * priorTI * age,data = newData)
newData$resi = residuals(model)
for (i in 1:2){
  for (j in timeLevel){
    index = which(newData$group == i & newData$time == j)
    newData$AFCR_RmT[index] = newData$AFCR - mean(data[index,]$AFCR)#residual
  }
}
meanRES = c()
for (i in timeLevel){
  meanRES = c(meanRES,mean(newData[which(newData$time == i),]$resi))
}
p1 = ggplot(data = newData,mapping=aes(x = time, y = resi,fill = as.factor(time))) + 
  geom_boxplot(alpha=0.7) + 
  stat_summary(fun.y=mean, geom="point", shape=10, color="red", fill="red") + 
  ggtitle("Plot for mean (and IQR) of residuals by Month") + xlab("Month") + ylab("AFCR Residuals")
p2 = ggplot(data = data.frame(x = timeLevel, y = meanRES),aes(x = x,y=y)) + 
  geom_line() + 
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  ggtitle("The tendency of the mean of AFCR  residuals")+xlab("Month") + ylab("mean of AFCR Residuals")
  
plot_grid(p1,p2,ncol = 2)

### correlation matrix
newData$id = data$id
subset = newData[,c(1,7,8)]
wide<-reshape(subset,v.names="resi",idvar="id",timevar="time",direction="wide")
new_cols = c("id","resi.0", "resi.3", "resi.6","resi.9", "resi.12", "resi.15","resi.18")
wide = wide[,new_cols]
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





