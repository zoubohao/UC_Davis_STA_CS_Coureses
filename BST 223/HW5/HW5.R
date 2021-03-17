wine = read.table("C:\\Users\\Admin\\Desktop\\BST 223\\HW4\\wine.txt", header = T)
qualityCat = rep("low", dim(wine)[1])
qualityCat[which(wine$quality>=4)]="high"
wine$quality = as.factor(qualityCat)
library(gam)
gamWineModel <- gam::gam(quality ~ s(fixed.acidity,5) + s(volatile.acidity,5) + s(citric.acid, 5) + 
                           s(residual.sugar,5) + s(chlorides, 5) + s(free.sulfur.dioxide, 5) + s(total.sulfur.dioxide,5) + 
                           s(density,5) + s(pH, 5)+ s(sulphates, 5) + s(alcohol,5),
                    family=binomial(), data=wine)
summary(gamWineModel)
library(broom)
outputTable = tidy(gamWineModel)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")




# Bootstrap ----------------

library(boot)
library(MASS)

# * confidence regions ----------------

B <- 2000
data <- read.table('C:\\Users\\Admin\\Desktop\\BST 223\\HW5\\melanoma.txt', header=TRUE)
baselineGLM = glm(totalincidence~.,data = data, family = poisson())
summary(baselineGLM)
outputTable = tidy(baselineGLM)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")

beta.est <- function(data, ind){
  res <- glm(totalincidence ~ ., data=data[ind,], family=poisson())
  coef(res)
}
boot.res <- boot(data, beta.est, R=B)

# ** confidence intervals for individual coefficients ----------------

alpha = 0.05
# 95% bootstrap confidence interval for beta_1
sort(boot.res$t[,2])[round(c((B+1)*alpha/2, (B+1)*(1-alpha/2)))]
# 95% bootstrap confidence interval for beta_2
sort(boot.res$t[,3])[round(c((B+1)*alpha/2, (B+1)*(1-alpha/2)))]

confint(baselineGLM)
##########################################
pimaData = read.table('C:\\Users\\Admin\\Desktop\\BST 223\\HW5\\pima-indians-diabetes.txt',sep=",")
pimaMLE = glm(V9~., family = binomial(), data = pimaData)
confint(pimaMLE)
outputTable = tidy(pimaMLE)
for (i in c(2:dim(outputTable)[2])){
  outputTable[,i] = round(outputTable[,i], 6)
}
write.csv(outputTable,"d:\\table.csv")
B = 2000
beta.est = function(data, ind){
  res = glm(V9~., data = data[ind,],family = binomial(),
            control = glm.control(epsilon = 1e-8, maxit = 10000))
  coef(res)
}
boot.res <- boot(pimaData, beta.est, R=B)

# ** confidence intervals for individual coefficients ----------------

alpha = 0.05
p = length(coef(pimaMLE))
lwb = c()
upb = c()
for (i in c(1:p)){
  # 95% bootstrap confidence interval for beta_1
  lwb = c(lwb,sort(boot.res$t[,i])[round(c((B+1)*alpha/2, (B+1)*(1-alpha/2)))][1])
  upb = c(upb,sort(boot.res$t[,i])[round(c((B+1)*alpha/2, (B+1)*(1-alpha/2)))][2])
}
result = data.frame(lwb, upb)
result




