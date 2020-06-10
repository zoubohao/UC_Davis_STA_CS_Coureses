#INCOMPLETE examples
#need more details
library(nlme)
library(geepack)
#2
#a
#i #ii
mydata <- read.csv("aheadld.csv")
mydata
head(mydata)
newdata <- cbind(mydata,mydata[,2]+mydata[,3], mydata[,27]+mydata[,28])
newdata
head(newdata)
dim(newdata)
colnames(newdata)[30] <- "realage"
colnames(newdata)[31] <- "totword"
head(newdata)

#b
newdata$sex <- newdata$sex-1 #1 for female and 0 for male
newdata$sex <- as.factor(newdata$sex)
newdata <- cbind(newdata, newdata[,30]-80)
colnames(newdata)[32] <- "centerage"
newdata <- na.omit(newdata)
#model1 = gls(totword ~ centerage * sex + blks + strs + push + bag + dime, data = newdata)
#model1 = gls(totword ~ centerage * sex + blks + strs + push + bag + dime, data = newdata, cor = corCompSymm())
#model1 = gls(totword ~ centerage * sex + blks + strs + push + bag + dime, data = newdata, cor = corSymm())
#model1 = lm(totword ~ centerage * sex + blks + strs + push + bag + dime, data = newdata, cor = corCompSymm())



summary(model1)

#c
model1$coefficients
model1$coefficients[3]-10*model1$coefficients[9]

#d
L <- matrix(0,nrow=5,ncol=9)
L[,4:8] <- diag(5)
L
Lb <- L%*%coef(model1)
CLb <- L%*%vcov(model1)%*%t(L)
W2 <- t(Lb)%*%solve(CLb)%*%Lb
(1-pchisq(W2,df=5))/2

#e
model2 = geeglm(totword ~ centerage * sex + blks + strs + push + bag + dime, id = id, data = newdata)
summary(model2)

