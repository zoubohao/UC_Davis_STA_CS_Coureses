#5.1.1
library(foreign)
chol <- read.dta("cholesterol.dta")
head(chol)
chol$group
chol$id
chol$group <- as.factor(chol$group)

#5.1.2
mean1 <- apply(chol[chol$group==1,3:7],2,mean,na.rm=TRUE)
apply(chol[chol$group==1,3:7],2,sd,na.rm=TRUE)
apply(chol[chol$group==1,3:7],2,var,na.rm=TRUE)

mean2 <- apply(chol[chol$group==2,3:7],2,mean,na.rm=TRUE)
apply(chol[chol$group==2,3:7],2,sd,na.rm=TRUE)
apply(chol[chol$group==2,3:7],2,var,na.rm=TRUE)

#5.1.3 & 5.1.4
chollong  <-  reshape(chol, idvar="id", varying=c("y1","y2","y3","y4","y5"), v.names="y", timevar="time", time=1:5, direction="long")
head(chollong)
attach(chollong)
month  <-  time
month[time==1]  <-  0
month[time==2]  <-  6
month[time==3]  <-  12
month[time==4]  <-  20
month[time==5]  <-  24
month.f <- as.factor(month)
interaction.plot(month, group, y, type="b", pch=c(19,21), fun=function(x) mean(x, na.rm=TRUE),
                 xlab="Time (in months)", ylab="Serum Cholesterol", main="Plot of Mean Serum Cholesterol Levels",
                 col=c(2,4))

#5.1.5
group <- relevel(group,"2")
library(nlme)
model  <-  gls(y ~ group*month.f, corr=corSymm(form= ~ time | id), weights = varIdent(form = ~ 1 | month.f), na.action=na.omit)
summary(model)

L <- matrix(0,nrow=4,ncol=10)
L[,7:10] <- diag(4)
Lb <- L%*%coef(model)
CLb <- L%*%vcov(model)%*%t(L)
W2 <- t(Lb)%*%solve(CLb)%*%Lb
1-pchisq(W2,df=4)

#5.1.6
summary(model)
v1 <- c(1.0000000,0.9322229,0.8798471,0.8962106,1.0303536)
v <- v1*46.76062^2
v

#5.1.7
#no code

#5.1.8
#see 5.1.5
L

#5.1.9
beta <- coef(model)
beta

mean1new<-rep(0,5)
mean1new[1] <- beta[1]#beta0
mean1new[2] <- beta[1]+beta[3]#beta0+beta2
mean1new[3] <- beta[1]+beta[4]#beta0+beta3
mean1new[4] <- beta[1]+beta[5]#beta0+beta4
mean1new[5] <- beta[1]+beta[6]#beta0+beta5

mean2new<-rep(0,5)
mean2new[1] <- beta[1]+beta[2]#beta0+beta1
mean2new[2] <- beta[1]+beta[2]+beta[3]+beta[7]#beta0+beta1+beta2+beta6
mean2new[3] <- beta[1]+beta[2]+beta[4]+beta[8]#beta0+beta1+beta3+beta7
mean2new[4] <- beta[1]+beta[2]+beta[5]+beta[9]#beta0+beta1+beta4+beta8
mean2new[5] <- beta[1]+beta[2]+beta[6]+beta[10]#beta0+beta1+beta5+beta9

mean1new
mean2new

mean1
mean2
#not the same, why?

#5.1.10
#Explain (based on part 9).