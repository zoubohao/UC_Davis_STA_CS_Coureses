library("MASS")
library(lawstat)
library(car)
oriData = read.csv("C:\\Users\\Admin\\Desktop\\BST 223\\Project1\\diabetes_data_upload.csv")
classNum = rep(0, dim(oriData)[1])
classNum[which(oriData$class == "Positive")] = 1
classNumData = oriData
classNumData$class = classNum

### Model Selection

fit1 = glm(class ~., data = classNumData, family = binomial())
summary(fit1)
Scope = list(upper = ~. + Age*(.), lower = ~1)

BICFit= stepAIC(fit1, trace = FALSE, scope = Scope,k = log(520))
summary(BICFit)


### Model Diagnostic

# We expect these two types of residuals have similar distributions.
# no lack-of-fit => similar boxplots
# similar boxplots -> next step: Residual Plots
par()
res.P = residuals(BICFit, type="pearson")
res.D = residuals(BICFit, type="deviance") #or residuals(fit), by default

par(mfrow=c(1,3))
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"), ylim=c(-0.5,0.5), 
        main="The boxplot of two residuals")

# * Residual Plots ----------------

# no lack-of-fit => no systematic pattern
# next step: Runs Test


plot(BICFit$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', 
     main="Residual plot for Pearson Residuals")
lines(smooth.spline(BICFit$fitted.values, res.P, spar=2.1), col=2)
abline(h=0, lty=2, col='grey')
plot(BICFit$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values',
     main="Residual plot for Deviance Residuals")
lines(smooth.spline(BICFit$fitted.values, res.D, spar=2.1), col=2)
abline(h=0, lty=2, col='grey')



runs.test(y = res.P, plot.it = TRUE)
title(main='Pearson Residual Runs Test')
runs.test(y = res.D, plot.it = TRUE)
title(main='Deviance Residual Runs Test')

### Add variable 
AICFit= stepAIC(BICFit, trace = FALSE, scope = Scope,k = 2,direction = "both")
summary(AICFit)

par(mfrow=c(1,3))
res.P = residuals(AICFit, type="pearson")
res.D = residuals(AICFit, type="deviance") #or residuals(fit), by default
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"), ylim=c(-0.5,0.5), main = "The boxplot of two residuals")


plot(AICFit$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', 
     main = "Residual plot for Pearson Residuals")
lines(smooth.spline(AICFit$fitted.values, res.P, spar=2), col=2)
abline(h=0, lty=2, col='grey')
plot(AICFit$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values',
     main = "Residual plot for Deviance Residuals")
lines(smooth.spline(AICFit$fitted.values, res.D, spar=2), col=2)
abline(h=0, lty=2, col='grey')

runs.test(y = res.P, plot.it = TRUE)
title(main='Pearson Residual Runs Test')
runs.test(y = res.D, plot.it = TRUE)
title(main='Deviance Residual Runs Test')


### vif aicFit
sort(vif(AICFit))
AICFit = glm(formula = class ~ Age + Gender + Polyuria + Polydipsia + 
               Polyphagia + Itching + Irritability + delayed.healing + partial.paresis + Age:partial.paresis + Age:Gender + 
               Age:Irritability + Age:Polydipsia, family = binomial(), data = classNumData)
sort(vif(AICFit))
AICFit = glm(formula = class ~ Age + Gender + Polyuria + Polydipsia + 
               Polyphagia + Itching + Irritability + delayed.healing + partial.paresis + Age:partial.paresis  + 
               Age:Irritability + Age:Polydipsia, family = binomial(), data = classNumData)
sort(vif(AICFit))
AICFit = glm(formula = class ~ Age + Gender + Polyuria + Polydipsia + 
               Polyphagia + Itching + Irritability + delayed.healing + partial.paresis   + 
               Age:Irritability + Age:Polydipsia, family = binomial(), data = classNumData)
sort(vif(AICFit))
AICFit = glm(formula = class ~ Age + Gender + Polyuria + Polydipsia + 
               Polyphagia + Itching + Irritability + delayed.healing + partial.paresis + 
               Age:Polydipsia, family = binomial(), data = classNumData)
sort(vif(AICFit))
AICFit = glm(formula = class ~ Age + Gender + Polyuria + Polydipsia + 
               Polyphagia + Itching + Irritability + delayed.healing + partial.paresis, family = binomial(), data = classNumData)
sort(vif(AICFit))

summary(AICFit)
### leverage
leverage = hatvalues(AICFit)
par(mfrow=c(1,2))
plot(names(leverage), leverage, xlab="Index", type="h", main="The leverage points")
points(names(leverage), leverage, pch=16, cex=0.6, col = 3)
p <- length(coef(AICFit))
n <- nrow(classNumData)
abline(h=2*p/n,col=2,lwd=2,lty=2)
infPts <- which(leverage>2*p/n)

### Cooks
cooks = cooks.distance(AICFit)
cooksTh = 4/(n-p)
plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6, main = "The cook's distance points")
abline(h=4/(n-p),col=2,lwd=2,lty=2)
infPtsCook = which(cooks>cooksTh)
interOutlier = intersect(infPts, infPtsCook)

removedData = classNumData[-interOutlier,]
finalFittedModel = glm(formula = class ~ Age + Gender + Polyuria + Polydipsia + 
               Polyphagia + Itching + Irritability + delayed.healing + partial.paresis, family = binomial(), data = removedData)

summary(finalFittedModel)
par(mfrow=c(1,3))
res.P = residuals(finalFittedModel, type="pearson")
res.D = residuals(finalFittedModel, type="deviance") #or residuals(fit), by default
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"), ylim=c(-0.5,0.5), main = "The boxplot of two residuals")


plot(finalFittedModel$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', 
     main = "Residual plot for Pearson Residuals")
lines(smooth.spline(finalFittedModel$fitted.values, res.P, spar=2.1), col=2)
abline(h=0, lty=2, col='grey')
plot(finalFittedModel$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values',
     main = "Residual plot for Deviance Residuals")
lines(smooth.spline(finalFittedModel$fitted.values, res.D, spar=2.1), col=2)
abline(h=0, lty=2, col='grey')

runs.test(y = res.P, plot.it = TRUE)
title(main='Pearson Residual Runs Test')
runs.test(y = res.D, plot.it = TRUE)
title(main='Deviance Residual Runs Test')



