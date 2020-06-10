#hw3
#3
#(a)
data <- read.table("~/Desktop/HW3/birthwt.raw", quote="\"", comment.char="")
head(data)
data[,2]
table(data[,2])
#more detail needed.

data[,4:6] <- data[,4:6]/10
head(data)
#data <- cbind(data, data[,2]==1)
data <- cbind(data, ifelse(data[,2]==1,1,0))
head(data)
colnames(data) <- c("id", "birthorder", "birthwt", "momage", "momage_avg", "momage_dev", "first_birth")
head(data)

#(b)
#i
#IMPORTANT!!!
#assumptions:
#distribution?
#intercept?
#slope?
#subject specific or not?

#ii&#iii
library(nlme)
model1 <- lme(birthwt ~ momage + factor(first_birth), random = ~ 1 | id, data = data, method = "ML")
summary(model1)

#(c)
#i
#IMPORTANT again.

#ii
model2 <- lm(birthwt ~ momage + factor(first_birth) + factor(id), data = data)
summary(model2)

#iii
#Why different?
#How different?

#iv
#Explanation
#hint 1: consider model assumptions and structures.
#hint 2: consider the relationship between $\hat{U_i}$ and $\bar{x'_i}\hat{\beta}$

######################
#how to do prediction for Individual Trajectories in R
pred <- matrix(predict(model2),878,5)
ori <- matrix(data[,3],878,5)
par(mfrow=c(1,2))
plot(c(1,2,3,4,5), pred[1,], col = 1, type = "l", ylim = c(min(pred),max(pred)))
#for(i in 2:878){
for(i in 2:10){
  points(c(1,2,3,4,5), pred[i,], col = i, type = "l")
}
plot(c(1,2,3,4,5), ori[1,], col = 1, type = "l", ylim = c(min(ori),max(ori)))
#for(i in 2:878){
for(i in 2:10){
  points(c(1,2,3,4,5), ori[i,], col = i, type = "l")
}
#Does this a good illustration?
#It's better to plot w.r.t. age.

