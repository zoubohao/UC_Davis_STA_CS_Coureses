### Data construction
oriData=read.table("C:\\Users\\Admin\\Desktop\\BST 223\\Project2\\student-por.csv",sep=";",header=TRUE)
n = dim(oriData)[1]
meanGrades = c()
for (i in c(1:n)){
  meanGrades = c(meanGrades, mean(c(oriData$G1[i], oriData$G2[i], oriData$G3[i])))
}
### 10.333333, 12.666667
data = cbind(oriData, meanGrades)
Y = rep("low", n)
mediumIndices = which((10.333333< data$meanGrades & data$meanGrades < 12.666667) | data$meanGrades == 12.666667)
highIndices = which(13.000000 < data$meanGrades | data$meanGrades == 13.000000)
Y[mediumIndices] = "medium"
Y[highIndices] = "high"
tempData = cbind(data, Y)
tempData = tempData[,-31]
tempData = tempData[,-31]
finalData = tempData[,-31]
finalData = finalData[,-31]
#################################
## model choise and selection. ##
#################################
a = factor(finalData$Y, levels = c("low", "medium", "high"))
finalData$Y = a
obslabel = c(1, 0, 0)
for (i in c(2:n)){
  label = Y[i]
  if (label == "low"){
    obslabel = rbind(obslabel, c(1, 0, 0))
  }
  else if (label == "medium"){
    obslabel = rbind(obslabel, c(0, 1, 0))
  }
  else{
    obslabel = rbind(obslabel, c(0, 0, 1))
  }
}
# * Proportional Odds Model ----------------
library(MASS)
plrModel <- polr(Y~., data = finalData, Hess = T)
summary(plrModel)
prd_prob_plr = fitted(plrModel)
resP.plr <- sapply(1:(ncol(obslabel)-1), function(m) {
  obs_m <- rowSums(as.matrix(obslabel[,1:m]))
  fit_m <- rowSums(as.matrix(prd_prob_plr[seq_len(nrow(finalData)), 1:m]))
  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
})
## low model
par(mfrow=c(1,2))
plot(prd_prob_plr[,1], resP.plr[,1], pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', main = "Fitted vs. Residual for low")
lines(smooth.spline(prd_prob_plr[,1], resP.plr[,1], spar=1.5), col=2)
abline(h=0, lty=2, col='grey')
## meidum model
plot(prd_prob_plr[,2], resP.plr[,2], pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values',main = "Fitted vs. Residual for medium")
lines(smooth.spline(prd_prob_plr[,2], resP.plr[,2], spar=1.5), col=2)
abline(h=0, lty=2, col='grey')
#library(lawstat)
#runs.test(y = resP.plr[,1])
#runs.test(y = resP.plr[,2])



# * Baseline Odds Model ----------------
library(nnet)
baseModel = multinom(Y~., data = finalData)
summary(baseModel)
prd_prob_base = fitted(baseModel)

residualGet = function(m) {
  # baseline is column 1 here 
  # otherwise you should replace "1" with the corresponding index and adjust the range of "m" accordingly
  obs_m <- obslabel[rowSums(obslabel[,c(1,m)]) > 0, m]
  fit_m <- prd_prob_base[rowSums(obslabel[,c(1,m)]) > 0, c(1,m)]
  fit_m <- fit_m[,2] / rowSums(fit_m)
  res = (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
  a = rowSums(obslabel[,c(1,m)]) > 0
  indices = which(a == T)
  b = list()
  b[[1]] = a
  b[[2]] = res
  return(b)
}
#resP.bo <- sapply(2:ncol(obslabel), function(m) {
#  # baseline is column 1 here 
#  # otherwise you should replace "1" with the corresponding index and adjust the range of "m" accordingly
#  obs_m <- obslabel[rowSums(obslabel[,c(1,m)]) > 0, m]
#  fit_m <- prd_prob_bo2[rowSums(obslabel[,c(1,m)]) > 0,c(1,m)]
#  fit_m <- fit_m[,2] / rowSums(fit_m)
#  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
#})
par(mfrow=c(1,2))
## medium model
mediumRes = residualGet(2)
mFitVal = prd_prob_base[mediumRes[[1]],2]
mRes = mediumRes[[2]]
par(mfrow=c(1,2))
plot(mFitVal, mRes, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', main = "Fitted vs. Residual for medium")
lines(smooth.spline(mFitVal, mRes, spar=1.5), col=2)
abline(h=0, lty=2, col='grey')
## high model
highRes = residualGet(3)
hFitVal = prd_prob_base[highRes[[1]],3]
hRes = highRes[[2]]
plot(hFitVal, hRes, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', main = "Fitted vs. Residual for high")
lines(smooth.spline(hFitVal, hRes, spar=1.5), col=2)
abline(h=0, lty=2, col='grey')

##### cross validation
TenPlrCV = function(data,k = 10){
  n = dim(data)[1]
  nums = round(n/k)
  wholeSet = c(1:n)
  misVec = c()
  for (i in c(1:k)){
    test <- c((1 + nums*(i-1)): (nums*i))
    train = setdiff(wholeSet, test)
    z <- polr(Y ~ ., data = data[train,])
    pred <- predict(z, data[-train, ])
    cl.test <- Y[-train]
    # To get the misclassification rate
    misrate <- length(which(cl.test == pred))/length(cl.test)
    misVec = c(misrate, misVec)
  }
  print(misVec)
  return(mean(misVec))
}

TenBaseCV = function(data,k = 10){
  n = dim(data)[1]
  nums = round(n/k)
  wholeSet = c(1:n)
  misVec = c()
  for (i in c(1:k)){
    test <- c((1 + nums*(i-1)): (nums*i))
    train = setdiff(wholeSet, test)
    z <- multinom(Y ~ ., data = data[train,])
    pred <- predict(z, data[-train, ])
    cl.test <- Y[-train]
    # To get the misclassification rate
    misrate <- length(which(cl.test == pred))/length(cl.test)
    misVec = c(misrate, misVec)
  }
  print(misVec)
  return(mean(misVec))
}

TenPlrCV(finalData, k = 10)
TenBaseCV(finalData, k = 10)

####### Model selection

# define a wider scope with more predictors in the upper model
Scope = list(upper = ~ ., lower = ~1)
selectedModelAIC <- stepAIC(baseModel, trace = FALSE, scope = Scope,k = 2 , direction = "both")
# Y ~ school + sex + Medu + Fedu + Fjob + reason +guardian + studytime + failures + schoolsup + famsup + activities + higher + Dalc + health + absences
summary(selectedModelAIC)
# z values
zvals <- coef(selectedModelAIC) / 
  summary(selectedModelAIC)$standard.errors
# two-sided p-values
pvals <- 2 * pnorm(abs(zvals), lower.tail=FALSE)
prd_prob_base = fitted(selectedModelAIC)
## medium model
mediumRes = residualGet(2)
mFitVal = prd_prob_base[mediumRes[[1]],2]
mRes = mediumRes[[2]]
par(mfrow=c(1,2))
plot(mFitVal, mRes, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', main = "Fitted vs. Residual for medium")
lines(smooth.spline(mFitVal, mRes, spar=1.5), col=2)
abline(h=0, lty=2, col='grey')
## high model
highRes = residualGet(3)
hFitVal = prd_prob_base[highRes[[1]],3]
hRes = highRes[[2]]
plot(hFitVal, hRes, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values', main = "Fitted vs. Residual for high")
lines(smooth.spline(hFitVal, hRes, spar=1.5), col=2)
abline(h=0, lty=2, col='grey')


nullModel <- multinom(Y ~ 1, data = finalData)
anova(nullModel, selectedModelAIC)

TenBaseAICCV = function(data,k = 10){
  n = dim(data)[1]
  nums = round(n/k)
  wholeSet = c(1:n)
  misVec = c()
  for (i in c(1:k)){
    test <- c((1 + nums*(i-1)): (nums*i))
    train = setdiff(wholeSet, test)
    z <- multinom(Y ~ school + sex + Medu + Fedu + Fjob + reason +guardian + studytime +
                    failures + schoolsup + famsup + activities + higher + Dalc + health + absences, data = data[train,])
    pred <- predict(z, data[-train, ])
    cl.test <- Y[-train]
    misrate <- length(which(cl.test == pred))/length(cl.test)
    misVec = c(misrate, misVec)
  }
  print(misVec)
  return(mean(misVec))
}

TenBaseAICCV(finalData)

