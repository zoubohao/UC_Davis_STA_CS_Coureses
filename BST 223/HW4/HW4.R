chemo.data = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW3\\chemo.txt")
count = chemo.data$V2 + chemo.data$V3 + chemo.data$V4 + chemo.data$V5
newData = data.frame(count = count, baseline = chemo.data$V7, age = chemo.data$V8,
                     treatment = as.factor(chemo.data$V6))
plot(x = c(1:59), y = count,pch=16, cex=0.6, xlab = "index", ylab = "count number", main = "Scatter Plot")
outindex = which(count>250)
cleanData = newData[-49,]

model = glm(count~baseline + age + treatment, family = poisson(), data = cleanData)

leverage = hatvalues(model)
par(mfrow=c(1,2))
plot(names(leverage), leverage, xlab="Index", type="h")
points(names(leverage), leverage, pch=16, cex=0.6)
p <- length(coef(model))
n <- nrow(cleanData)
abline(h=2*p/n,col=2,lwd=2,lty=2)
infPts <- which(leverage>2*p/n)

cooks = cooks.distance(model)

plot(cooks, ylab="Cook's Distance", pch=16, cex=0.6)
points(infPts, cooks[infPts], pch=17, cex=0.8, col=2)
susPts <- as.numeric(names(sort(cooks[infPts], decreasing=TRUE)[1:4]))
text(susPts, cooks[susPts], susPts, adj=c(-0.1,-0.1), cex=0.7, col=4)

sigma2 = model$deviance / (n - p)
#############
library(MASS)
nb1 <- glm.nb(count~baseline + age + treatment, data=cleanData)

par(mfrow=c(1,3))
res.P = residuals(nb1, type="pearson")
res.D = residuals(nb1, type="deviance") #or residuals(fit), by default
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))

plot(nb1$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(nb1$fitted.values, res.P, spar=1.6), col=2)
abline(h=0, lty=2, col='grey')
plot(nb1$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(nb1$fitted.values, res.D, spar=1.6), col=2)
abline(h=0, lty=2, col='grey')
nb1$deviance / (n - p)

quasip <- glm(count~baseline + age + treatment, data = cleanData, family = quasipoisson())

par(mfrow=c(1,3))
res.P = residuals(quasip, type="pearson")
res.D = residuals(quasip, type="deviance") #or residuals(fit), by default
boxplot(cbind(res.P, res.D), names = c("Pearson", "Deviance"))

plot(quasip$fitted.values, res.P, pch=16, cex=0.6, ylab='Pearson Residuals', xlab='Fitted Values')
lines(smooth.spline(quasip$fitted.values, res.P, spar=1.6), col=2)
abline(h=0, lty=2, col='grey')
plot(quasip$fitted.values, res.D, pch=16, cex=0.6, ylab='Deviance Residuals', xlab='Fitted Values')
lines(smooth.spline(quasip$fitted.values, res.D, spar=1.6), col=2)
abline(h=0, lty=2, col='grey')
quasip$deviance / (n - p)


##########################
wine = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW4\\wine.txt", header = T)
qualityCat = rep("low", dim(wine)[1])
qualityCat[which(wine$quality == 4)] = "mid"
qualityCat[which(wine$quality >= 5)] = "hig"
wine$quality = qualityCat
wine$quality = as.factor(wine$quality)
# Proportional Odds Model
wine_polr <- polr(quality ~., data =wine)
a = summary(wine_polr)
pval_polr = 2 * pt(abs(a$coefficients[,3]),df=n-2,lower.tail = F)
tableA = data.frame(a$coefficients, pvalue=pval_polr)
write.csv(tableA, "d:\\table.csv")
# ** Pearson residuals
prd_prob_po = fitted(wine_polr)
n = dim(wine)[1]
ms = 3
obslabel = matrix(data = rep(0, n * ms), nrow = n)

for (i in c(1:n)){
  if (wine$quality[i] == "low"){
    obslabel[i,2] = 1
  }
  else if (wine$quality[i] == "mid"){
    obslabel[i,3] = 1
  }
  else{
    obslabel[i,1] = 1
  }
}
resP.plr <- sapply(1:(ncol(obslabel) - 1), function(m) {
  obs_m <- rowSums(as.matrix(obslabel[,1:m]))
  fit_m <- rowSums(as.matrix(prd_prob_po[seq_len(nrow(wine)),1:m]))
  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
})

#library(broom)
#outputTable = tidy(wine_polr)
#for (i in c(2:dim(outputTable)[2])){
#  outputTable[,i] = round(outputTable[,i], 6)
#}
#write.csv(outputTable,"d:\\table.csv")

# baseline odd model
library(nnet)
wine_baseLine<- multinom(quality ~., data=wine)
summary(wine_baseLine, digit=3)
# z values
zval.bo <- coef(wine_baseLine) / summary(wine_baseLine)$standard.errors
# two-sided p-values
pval.bo <- 2 * pnorm(abs(zval.bo), lower.tail=FALSE)

wine = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW4\\wine.txt", header = T)
qualityCat = rep("low", dim(wine)[1])
qualityCat[which(wine$quality>=4)]="high"
wine$quality = as.factor(qualityCat)
model = glm(quality ~., data = wine, family = binomial())
summary(model)
library(broom)
outputTable = tidy(model)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")










