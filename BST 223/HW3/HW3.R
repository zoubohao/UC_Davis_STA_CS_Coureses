
### 7.
chemo.data = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW3\\chemo.txt")
count = chemo.data$V2 + chemo.data$V3 + chemo.data$V4 + chemo.data$V5
newData = data.frame(count = count, baseline = chemo.data$V7, age = chemo.data$V8,
                     treatment = as.factor(chemo.data$V6))
plot(x = c(1:59), y = count,pch=16, cex=0.6, xlab = "index", ylab = "count number", main = "Scatter Plot")
outindex = which(count>250)
cleanData = newData[-49,]

model = glm(count~baseline + age + treatment, family = poisson(), data = cleanData)
res.D = residuals(model, type = "deviance")
res.P = residuals(model, type = "pearson")
### goodness of fit
P_stat = sum(res.P^2)
D_stat = sum(res.D^2)
par(mfrow=c(1,3))
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))
plot(model$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(model$fitted.values, res.P, spar=1.6), col=2)
abline(h=0, lty=2, col='grey')
plot(model$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(model$fitted.values, res.D, spar=1.6), col=2)
abline(h=0, lty=2, col='grey')

### 8
pima.data = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW3\\pima-indians-diabetes.txt",sep=",")
first = glm(V9~., family = binomial(), data = pima.data)
outputTable = tidy(first)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
res.D = residuals(first, type = "deviance")
res.P = residuals(first, type = "pearson")
### goodness of fit
P_stat = sum(res.P^2)
D_stat = sum(res.D^2)
par(mfrow=c(1,3))
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))
plot(first$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(first$fitted.values, res.P, spar=2), col=2)
abline(h=0, lty=2, col='grey')
plot(first$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(first$fitted.values, res.D, spar=2), col=2)
abline(h=0, lty=2, col='grey')
library(lawstat)
runs.test(y = res.P, plot.it = TRUE)
title(main='Pearson Residual Runs Test')
runs.test(y = res.D, plot.it = TRUE)
title(main='Deviance Residual Runs Test')
library(MASS)
aic = stepAIC(first, trace = FALSE)
outputTable = tidy(aic)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
bic = stepAIC(first, trace = F, k = log(dim(pima.data)[1]))
outputTable = tidy(bic)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
### 9
lungData = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW3\\lung.txt", header = T)
lungData$Dust = as.factor(lungData$Dust)
lungData$Race = as.factor(lungData$Race)
lungData$Smoker = as.factor(lungData$Smoker)
lungData$Sex = as.factor(lungData$Sex)
model = glm(cbind(Yes, No) ~ Dust + Race + Smoker + Sex + EmpLength, data = lungData,
            family = binomial())
aic = stepAIC(model, trace = F)
anova(model, test = "Chisq")
summary(aic)
outputTable = tidy(anova(model, test = "Chisq"))
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
outputTable = tidy(aic)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
res.D = residuals(aic, type = "deviance")
res.P = residuals(aic, type = "pearson")
### goodness of fit
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))

leverage = hatvalues(aic)

W = diag(aic$weights)
X = cbind(rep(1,nrow(bwt)), bwt[['lwt']], bwt[['race']]=='black',
          bwt[['race']]=='other', bwt[['smoke']], bwt[['ptd']])
Hat = sqrt(W) %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% sqrt(W)
all(abs(leverage - diag(Hat)) < 1e-15)

par(mfrow= c(1,2))
plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.6)
p <- length(coef(aic))
n <- nrow(lungData)
abline(h=2*p/n,col=2,lwd=2,lty=2)
infPts <- which(leverage>2*p/n)

# ** Cook's Distance ----------------

# high Cook's distance => influential points/outliers
# leverage points with high Cook's distance => suspicious influential points & outliers
#                    may need to be deleted -> check scatterplots

cooks = cooks.distance(aic)

plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6)
points(infPts, cooks[infPts], pch=17, cex=0.8, col=2)
infPts <- which ( leverage >2*p/n )
cooksTh = 4/(n - p )
infPtsCook = which (cooks > cooksTh )
interOutlier = intersect ( infPts , infPtsCook )

model = glm(cbind(Yes, No) ~ Dust + Smoker + EmpLength + Dust:EmpLength, data = lungData,
            family = binomial())
outputTable = tidy(model)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
