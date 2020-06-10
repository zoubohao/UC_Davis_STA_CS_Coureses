setwd("C:\\Users\\15302\\Desktop\\BST 224\\HW4")


#### Q1
## (a)
q1_data = read.table("exercise.raw",header = F)
datalong <- reshape(q1_data, idvar = "V1", direction = 'long', varying = c("V3","V4","V5","V6","V7","V8","V9"),
                    v.names = "y", timevar = "day", time = seq(0,12,2))
colnames(datalong) <- c("id", "treatment", "timedays", "measures")
head(datalong)
dim(data)
dim(datalong)
datalong[,4]<-as.numeric(datalong[,4])

## (b)
library(nlme)
model1 <- lme(measures ~ factor(treatment) * timedays, random = ~ 1 + timedays | id, data = na.omit(datalong), method = "REML")
summary(model1)


#(c)
model2 <- lme(measures ~ factor(treatment) * timedays, random = ~ 1 | id, data = na.omit(datalong), method = "REML")
summary(model2)
#remember to do this hypoythesis test with complete steps. (not just say p-value...)

#(d)
#not complete here!
#should impute the other two time points.


new_data = data.frame(id = c(24,24,24,24,24,24,24,24),treatment = as.factor(c(1,2,2,2,2,2,2,2)), timedays = c(0,0,2,4,6,8,10,12))
predV = predict(model1,newdata = new_data)[-1]
library("ggplot2")

Na_for_10 = na.omit(datalong[which(datalong$timedays == 10),])
lmModel_10 = lm(measures ~ as.factor(treatment), data = Na_for_10)
summary(lmModel_10)


Na_for_12 = na.omit(datalong[which(datalong$timedays == 12),])
lmModel_12 = lm(measures ~ as.factor(treatment), data = Na_for_12)
summary(lmModel_12)

##### Q2
#(a)
newdata <- read.csv("leprosy.csv")
head(newdata)
mean_a_0 = mean(newdata[which(newdata$Drug=="A"&newdata$visit==0),3])
var_a_0 = sd(newdata[which(newdata$Drug=="A"&newdata$visit==0),3])^2

mean_a_1 = mean(newdata[which(newdata$Drug=="A"&newdata$visit==1),3])
var_a_1 = sd(newdata[which(newdata$Drug=="A"&newdata$visit==1),3])^2

mean_b_0 = mean(newdata[which(newdata$Drug=="B"&newdata$visit==0),3])
var_b_0 = sd(newdata[which(newdata$Drug=="B"&newdata$visit==0),3])^2

mean_b_1 = mean(newdata[which(newdata$Drug=="B"&newdata$visit==1),3])
var_b_1 = sd(newdata[which(newdata$Drug=="B"&newdata$visit==1),3])^2

mean_c_0 = mean(newdata[which(newdata$Drug=="C"&newdata$visit==0),3])
var_c_0 = sd(newdata[which(newdata$Drug=="C"&newdata$visit==0),3])^2

mean_c_1 = mean(newdata[which(newdata$Drug=="C"&newdata$visit==1),3])
var_c_1 = sd(newdata[which(newdata$Drug=="C"&newdata$visit==1),3])^2

####(B)
# ii
attach(newdata)
library(geepack)
DrugA <- (newdata[,1] == "A")
DrugB <- (newdata[,1] == "B")
model<-geeglm(y ~ 1 + DrugA + DrugB + visit + DrugA : visit + DrugB : visit, data=newdata, id = id, 
              family=poisson("log"), corstr="exch")
summary(model)



# iii
library(MASS)
l1 = c(1,0,-1,0,0,0)
l2 = c(1,-1,0,0,0,0)
L = rbind(l1,l2)
beta = as.matrix(coefficients(model))
LCL = L%*%vcov(model)%*%t(L)
LB = L%*%beta
staT = t(LB) %*% solve(LCL)%*%(LB)


# iv
modelRobust<-geeglm(y ~ 1 + visit + DrugA : visit + DrugB : visit, data=newdata, id = id, family=poisson("log"), corstr="exch", std.err="san.se")
summary(modelRobust)



## (c)
library(lme4)
glmmmodel<-glmer(y ~ (1|id) + visit + DrugA : visit + DrugB : visit, data=newdata, family=poisson("log"),corstr="exch")
summary(glmmmodel)




