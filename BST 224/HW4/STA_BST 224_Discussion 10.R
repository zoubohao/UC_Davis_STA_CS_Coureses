library(foreign)

#hw 4
#question 2
#(a)
newdata <- read.csv("~/Desktop/HW4/leprosy.csv")
head(newdata)
#Just check the sample mean and sample variance for each treatment group
  #for two visits, respectively.
#Hint: Poisson model assumes that the variance is the same as the mean if no overdispersion.

#(b)
#i
#Important:
#Pay attention to the difference between
#log(E(Y|U)) & E(log(Y)|U)
#and
#General Linear Model & Generalized Linear Model

#ii
attach(newdata)
library(lme4)
DrugA <- (newdata[,1] == "A")
DrugB <- (newdata[,1] == "B")
model<-geeglm(y ~ 1 + DrugA + DrugB + visit + DrugA * visit + DrugB * visit, data=newdata, id = id, 
            family=poisson("log"), corstr="exch", std.err="san.se")
summary(model)
#Explaination needed here

#iii
#Pay attention to the trick here. This is NOT as easy as before.
#Hint: Consider the true meaning of beta's in our model.

#iv
newmodel<-geeglm(y ~ 1 + visit + DrugA : visit + DrugB : visit, data=newdata, id = id, 
              waves=time, family=poisson("log"), corstr="exch", std.err="san.se")
summary(newmodel)

#Again, pay attention to the true meaning of beta's in our model.

#v
summary(newmodel)

#(c)
library(lme4)
glmmmodel<-glmer(y ~ (1|id) + visit + DrugA : visit + DrugB : visit, data=newdata, family=poisson("log"))
summary(glmmmodel)


#Compare with what we did last time: Marginal Log-linear Regression Model
data <- read.dta("leprosy.dta")
head(data)
datalong <- reshape(data, idvar="id", varying=c("y1","y2"), 
                    v.names="y", timevar="time", time=0:1, direction="long")
datalong <- datalong[order(datalong$id, datalong$time),]
head(datalong)
attach(datalong) 
drugn <- as.numeric(drug)
timeA <- time*(drugn == 2)
timeB <- time*(drugn == 3)
timeAB <- time*I(drugn != 1)

model3 <- geeglm(y ~ time + timeA + timeB, data=datalong, id = id, 
                 waves=time, family=poisson("log"), corstr="exch", std.err="san.se")
summary(model3)

model4 <- geeglm(y ~ time + timeAB, data=datalong, id = id, 
                waves=time, family=poisson("log"), corstr="exch", std.err="san.se")
summary(model4)

#Final Project
#In case you are more interested in analyzing your own data,
#please contact instructor first to make sure it is ok to use it
#as a substitute for this problem.

#Please include program and important results.

#Due (for open) on noon! NOT midnight.