setwd("C:\\Users\\15302\\Desktop\\BST 224\\Final\\")
data = read.csv("ichs.csv")

wideData = reshape(data,idvar = "id", v.names = "infect" , 
                   timevar = "time",direction = "wide", times = c(3, 6, 9, 12, 15))


#### Q1 explore data set
library("cowplot")
library("ggplot2")
library(tidyverse)
library(hrbrthemes)
library(viridis)

### The distribution of bage in subject
p1 = ggplot(data = wideData, mapping = aes(x = bage)) + 
  geom_histogram(binwidth = 0.5,fill="#69b3a2", color="#e9ecef", alpha=0.9) + xlab("Baseline Age") + ylab("Subjects number of Baseline Age") + 
  ggtitle("The distribution of Baseline Age across Subjects")
p1

### The distribution of vita in subject
p2 = ggplot(data = wideData, mapping = aes(x = vita)) + 
  geom_histogram(binwidth = 0.5,fill="#69b3a2", color="#e9ecef", alpha=0.9) + xlab("vitamin A deficiency, 0 = no, 1
= yes") + ylab("Subjects number of vitamin A deficiency") + 
  ggtitle("The distribution of vitamin A deficiency across Subjects")
p2 
plot_grid(p1,p2,ncol = 2)

### Barplot for time and infection
timeX = c(3, 6, 9, 12, 15)
x = c()
y = c()
for (t in timeX){
    x = c(x,t)
    y = c(y,sum(data$infect[which(data$time==t)]) / length(data$infect[which(data$time==t)]))
}
p3 = ggplot(data = data.frame(x = x, y = y), mapping = aes(x=x,y = y)) + 
  geom_line() + xlab("Time") +geom_point(shape=21 ,size=2,color="black", fill="#69b3a2",) + ylab("Infection Count") + 
  ggtitle("The relationship of time and infection")
p3


### barplot for time and infection with gender
timeX = c(3, 6, 9, 12, 15)
x = c()
y = c()
gender = c()
for (t in timeX){
  for (g in c(0,1)){
    x = c(x,t)
    y = c(y,sum(data$infect[which(data$time==t & data$gender == g)]) / length(data$infect[which(data$time==t & data$gender == g)]))
    if (g == 0){
      gender = c(gender,"Male")
    }
    else{
      gender = c(gender,"Female")
    }
  }
}
p4 = ggplot(data = data.frame(x = x, y = y, gender = gender), mapping = aes(x=x,y = y,fill= gender)) + 
  geom_bar(stat = "identity", width=2, position = "dodge") + xlab("Time") + ylab("Infection Count") + 
  ggtitle("The barplot for time and infection with gender")
p4

### barplot for time and infection with bage
timeX = c(3, 6, 9, 12, 15)
x = c()
y = c()
bage = c()
for (t in timeX){
  for (g in c(1, 2,3 ,4, 5, 6, 7)){
    x = c(t,x)
    y = c(y,sum(data$infect[which(data$time==t & data$bage == g)]) / length(data$infect[which(data$time==t & data$bage == g)]))
    bage = c(g,bage)
  }
}
p5 = ggplot(data = data.frame(x = x, y = y, BaselineAge = as.factor(bage)), mapping = aes(x=x,y = y,fill= BaselineAge)) + 
  geom_bar(stat = "identity", width=2, position = "dodge") + xlab("Time") + ylab("Infection Count") + 
  ggtitle("The relationship of time and infection with baseline age")
p5

### barplot for time and infection with vita
timeX = c(3, 6, 9, 12, 15)
x = c()
y = c()
vita = c()
for (t in timeX){
  for (g in c(0,1)){
    x = c(t,x)
    y = c(y,sum(data$infect[which(data$time==t & data$vita == g)]) / length(data$infect[which(data$time==t & data$vita == g)]))
    if (g == 0){
      vita = c(vita,"No vitamin A deficiency")
    }
    else{
      vita = c(vita,"vitamin A deficiency")
    }
  }
}
p6 = ggplot(data = data.frame(x = x, y = y, VitADefi = as.factor(vita)), mapping = aes(x=x,y = y,fill= VitADefi)) + 
  geom_bar(stat = "identity", width=2, position = "dodge") + xlab("Time") + ylab("Infection Count") + 
  ggtitle("The relationship of time and infection with vitamin A deficiency")
p6
plot_grid(p3,p4,p5,p6,ncol = 2)

### Q2
modelGLM = glm(infect ~ time + gender + bage + vita + time : gender + time : bage + time : vita,data = data, family = binomial())
summary(modelGLM)
resiModel = residuals(modelGLM)
Q2Data = data
Q2Data$resi = resiModel
subset = Q2Data[,c(1,2,7)]
wideSub = reshape(subset,v.names="resi",idvar="id",timevar="time",direction="wide")
wideSub = wideSub[,-1]
corrM = cor(wideSub,use="pairwise.complete.obs")
write.csv(corrM,file = "./CorrM.csv")
### plot correlogram
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
corrgram = auto_correlation(subset)
corrDataFrame1 = data.frame(lag = corrgram$lag, ACF = corrgram$ACF)
corrDataFrame2 = data.frame(lag = corrgram$lag, ACF = corrgram$TL)
group = c(rep("ACF",5),rep("TL",5))
newCorrDF = rbind(corrDataFrame1,corrDataFrame2)
newCorrDF = cbind(newCorrDF,group)
p = ggplot(data = newCorrDF,mapping = aes(x = lag,y = ACF,color = group)) + 
  geom_line(linetype = 1) + 
  geom_point(shape=21 ,size=2,color="black", fill="#69b3a2",) + 
  xlab("Time Lag") + ylab("ACF") + ggtitle("Correlogram.")

p

### Q3
library(ModelMetrics)
library(wgeesel)
beta_chi_cal = function(beta_numbers, model){
  result = c()
  betas = as.matrix(coefficients(model))
  for (i in c(1:beta_numbers)){
    L = rep(0,beta_numbers)
    L[i] = 1
    L = t(as.matrix(L))
    LB = L %*% betas
    med = solve(L %*% vcov(model) %*% t(L))
    result = c(result,pchisq(t(LB) %*% med %*% LB, df=1,lower.tail=FALSE))
  }
  return(result)
}

f1s = function(preV, labelV){
  cm = confusionMatrix(labelV,preV,cutoff = 0.5)
  tp = cm[1,1]
  fp = cm[1,2]
  fn = cm[2,1]
  tn = cm[2,2]
  f1s = tp / (tp + fn) * 0.5 + tn / (tn + fp) * 0.5
  return(f1s)
}

library(geepack)
### basic model 
modelGEEGLM1<-geeglm(infect ~   time + gender + bage + vita + 
                       time : gender + time : bage + time : vita + 
                      gender : bage + gender : vita + 
                      bage : vita, data=data, id = id, 
              family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM1)
f1s(predict(modelGEEGLM1,type="response"),data$infect)
QIC(modelGEEGLM1)


### gender : vita 0.957
modelGEEGLM2<-geeglm(infect ~   time + gender + bage + vita + 
                       time : gender + time : bage + time : vita + 
                       gender : bage + 
                       bage : vita, data=data, id = id, 
                     family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM2)
f1s(predict(modelGEEGLM2,type="response"),data$infect)
QIC(modelGEEGLM2)

### time : gender 0.861
modelGEEGLM3<-geeglm(infect ~   time + gender + bage + vita + time : bage + time : vita + 
                       gender : bage + 
                       bage : vita, data=data, id = id, 
                     family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM3)
f1s(predict(modelGEEGLM3,type="response"),data$infect)
QIC(modelGEEGLM3)

### gender 0.775
modelGEEGLM4<-geeglm(infect ~   time + bage + vita + time : bage + time : vita + 
                       gender : bage + 
                       bage : vita, data=data, id = id, 
                     family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM4)
f1s(predict(modelGEEGLM4,type="response"),data$infect)
QIC(modelGEEGLM4)

### time : vita 0.618
modelGEEGLM5<-geeglm(infect ~   time + bage + vita + time : bage+ 
                       gender : bage + 
                       bage : vita, data=data, id = id, 
                     family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM5)
f1s(predict(modelGEEGLM5,type="response"),data$infect)
QIC(modelGEEGLM5)

### bage 0.5328
modelGEEGLM6<-geeglm(infect ~   time + vita + time : bage+ 
                       gender : bage + 
                       bage : vita, data=data, id = id, 
                     family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM6)
f1s(predict(modelGEEGLM6,type="response"),data$infect)
QIC(modelGEEGLM6)

### vita 0.2386
modelGEEGLM7<-geeglm(infect ~   time  + time : bage+ 
                       gender : bage + 
                       bage : vita, data=data, id = id, 
                     family=binomial(link = "logit"), corstr="exch")
summary(modelGEEGLM7)
f1s(predict(modelGEEGLM7,type="response"),data$infect)
QIC(modelGEEGLM7)

library("tidyverse")
tidy(modelGEEGLM6,conf.int = T)

### Q4
### The 6-th model is the best.  & Q6Data$vita==1
Q6Data = data
Q6Data$pred = fitted.values(modelGEEGLM6)

timesC = sort(unique(Q6Data$time))
predictC = c()
dataC = c()
for (t in timesC){
  predictC = c(predictC,mean(Q6Data$pred[which(Q6Data$time == t )]))
  dataC = c(dataC, mean(Q6Data$infect[which(Q6Data$time == t)]))
}
preDataF = data.frame(time = timesC, y = predictC)
dataDF = data.frame(time = timesC, y = dataC)
finalDF = rbind(preDataF,dataDF)
group = c(rep("Predictive",6),rep("Empirical",6))
finalDF = cbind(finalDF,group)
finalDF
q6p1 = ggplot(finalDF,aes(x = time, y = y , color = group)) + 
  geom_line(linetype = 1) + 
  geom_point(shape=21 ,size=2,color="black", fill="#69b3a2",) + xlab("Time") + ylab("Predictive or Empirical Value")  + ylim(0,1) + 
  ggtitle("Checking the linearity of time variable")
q6p1


bageC = sort(unique(Q6Data$bage))
predictC = c()
dataC = c()
for (t in bageC){
  predictC = c(predictC,mean(Q6Data$pred[which(Q6Data$bage == t )]))
  dataC = c(dataC, mean(Q6Data$infect[which(Q6Data$bage == t )]))
}
preDataF = data.frame(bage = bageC, y = predictC)
dataDF = data.frame(bage = bageC, y = dataC)
finalDF = rbind(preDataF,dataDF)
group = c(rep("Predictive",7),rep("Empirical",7))
finalDF = cbind(finalDF,group)
finalDF
q6p2 = ggplot(finalDF,aes(x = bage, y = y , color = group)) + 
  geom_line(linetype = 1) + 
  geom_point(shape=21 ,size=2,color="black", fill="#69b3a2",) + xlab("Baseline age") + ylab("Predictive or Empirical Value") + ylim(0,1) + 
  ggtitle("Checking the linearity of baseline age variable")
q6p2
plot_grid(q6p1,q6p2,ncol = 2)

### Q6
library(lme4)
glmmmodel<-glmer(infect ~ (1|id) + time + vita + time:bage + bage:gender + vita:bage, 
                 data=data, family=binomial("logit"))
summary(glmmmodel)





