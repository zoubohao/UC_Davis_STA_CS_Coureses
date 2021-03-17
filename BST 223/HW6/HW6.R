pima.data = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW6\\pima-indians-diabetes.txt",sep=",")
glmModel = glm(V9~V2+V3+V4+V5+V6+V8, family = binomial(), data = pima.data)
res.D = residuals(glmModel, type = "deviance")
res.P = residuals(glmModel, type = "pearson")
par(mfrow= c(1,3))
### goodness of fit
P_stat = sum(res.P^2)
D_stat = sum(res.D^2)
par(mfrow=c(1,3))
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))
plot(glmModel$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(glmModel$fitted.values, res.P, spar=2.5), col=2)
abline(h=0, lty=2, col='grey')
plot(glmModel$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(glmModel$fitted.values, res.D, spar=2.5), col=2)
abline(h=0, lty=2, col='grey')
library(lawstat)
runs.test(y = res.P, plot.it = TRUE)
title(main='Pearson Residual Runs Test')
runs.test(y = res.D, plot.it = TRUE)
title(main='Deviance Residual Runs Test')

library(mgcv)
mgcvgam2 <- mgcv::gam(V9 ~ s(V2) + s(V3) +
                        s(V4) + s(V5) + s(V6) +
                        s(V8),
                      family=binomial(),data=pima.data)
# k specifies number of basis functions
# bs specifies the basis functions used here: cubic splines here; default is thin plate splines.
summary(mgcvgam2)
summary(glmModel)
library(broom)
outputTable = tidy(mgcvgam2)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
plot(mgcvgam2, residuals=TRUE, shade = TRUE)


library(gam)
gamgam1 <- gam::gam(V9 ~ s(V2,4) + s(V3,5) + V4 + V5 + s(V6,4) + s(V8,5),
                    family=binomial(), data=pima.data)
summary(gamgam1)
outputTable = tidy(gamgam1)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")


library(boot)
?cv.glm
### linear predictor
glmPIMA <- glm(V9~., family = binomial(), data = pima.data)
cv.err.10 <- cv.glm(pima.data, glmPIMA, K = 10)$delta # 10-fold CV
cv.err.10
### quadratic 
glmPIMAq <- glm(V9~V1^2 + V2^2 + V3^2 + V4^2 + V5^2 + V6^2 
               + V7^2 + V8^2 
               , family = binomial(), data = pima.data)
cv.err.10 <- cv.glm(pima.data, glmPIMAq, K = 10)$delta # 10-fold CV
cv.err.10
### Fisher linear dis ana
library(MASS)
TenldaCV = function(data,k = 10){
  n = dim(data)[1]
  nums = round(n/k)
  wholeSet = c(1:n)
  misVec = c()
  for (i in c(1:k)){
    test <- c((1 + nums*(i-1)): (nums*i))
    train = setdiff(wholeSet, test)
    z <- lda(V9 ~ ., data, subset = train)
    pred <- predict(z, data[-train, ])$class
    cl.test <- data$V9[-train]
    # To get the misclassification rate
    misrate <- length(which(cl.test != pred))/length(cl.test)
    misVec = c(misrate, misVec)
  }
  return(mean(misVec))
}
cv.lda.10 = TenldaCV(pima.data)
cv.lda.10
### Q linear dis ana
TenqdaCV = function(data,k = 10){
  n = dim(data)[1]
  nums = round(n/k)
  wholeSet = c(1:n)
  misVec = c()
  for (i in c(1:k)){
    test <- c((1 + nums*(i-1)): (nums*i))
    train = setdiff(wholeSet, test)
    z <- qda(V9 ~ ., data, subset = train)
    pred <- predict(z, data[-train, ])$class
    cl.test <- data$V9[-train]
    # To get the misclassification rate
    misrate <- length(which(cl.test != pred))/length(cl.test)
    misVec = c(misrate, misVec)
  }
  return(mean(misVec))
}
cv.qda.10 = TenqdaCV(pima.data)
cv.qda.10



chemo.data = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW3\\chemo.txt")
count = chemo.data$V2 + chemo.data$V3 + chemo.data$V4 + chemo.data$V5
newData = data.frame(count = count, baseline = chemo.data$V7, age = chemo.data$V8,
                     treatment = as.factor(chemo.data$V6))
plot(x = c(1:59), y = count,pch=16, cex=0.6, xlab = "index", ylab = "count number", main = "Scatter Plot")
outindex = which(count>250)
cleanChemoData = newData[-49,]
head(cleanChemoData)
n = dim(cleanChemoData)[1]
poissonModel = glm(count~baseline + age + treatment, family = poisson(), data = cleanData)
outputTable = tidy(poissonModel)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")


nbModel <- glm.nb(count~baseline + age + treatment, data=cleanChemoData)
outputTable = tidy(nbModel)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")


library(pscl)
# Zero-Inflated Poisson Model
ziPoiss <- zeroinfl(count~baseline + age + treatment|baseline + age + treatment, 
                data = cleanChemoData, dist = 'poisson')

summary(ziPoiss)
# Zero-Inflated Negative Binomial Model
zinb <- zeroinfl(count~baseline + age + treatment|baseline + age + treatment,
                 data = cleanChemoData, dist = 'negbin')
summary(zinb)
vuong(poissonModel,ziPoiss)
vuong(nbModel,zinb)

poissonModel$deviance / (n-length(coef(poissonModel)))
nbModel$deviance / (n-length(coef(nbModel)))



