#set your own path
setwd("~")
# Multinomial Regression ----------------

# * Proportional Odds Model ----------------

# performs logistic regression for the first m categories combined versus the rest, m = 1, ..., M-1.
# restricts the slope parameters to be the same for all submodels;
# only the intercept coefficient increases in submodel/category index m.

library(MASS)
?polr
# The response must be a factor!

## two formats of multinomial data
data(housing)
head(housing)

Sat <- matrix(housing$Freq, byrow=TRUE, ncol=3)
colnames(Sat) <- housing$Sat[1:3]
housing2 <- data.frame(Sat, housing[seq(1,nrow(housing),3),2:4])
head(housing2)
# if the dataset is in the same format as housing2, you need to turn it into the format as housing to be fitted by polr().

house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
summary(house.plr)
# n by M matrix of predicted prob:
prd_prob_po = predict(house.plr, type='prob')
# equivalently
prd_prob_po = fitted(house.plr)
# vector of predicted labels:
prd_labl_po = predict(house.plr)

head(data.frame(prd_labl_po, prd_prob_po))

# ** Pearson residuals ----------------

if(0){
  # wrong code:
  obslabel <- matrix(0, nrow = nrow(housing), ncol = length(levels(housing$Sat)))
  for (i in seq_len(nrow(housing))) obslabel[i,which(colnames(prd_prob_po)==housing$Sat[i])] = 1
  head(data.frame(obslabel,housing$Sat))
  resP.plr <- (obslabel - prd_prob_po) / sqrt(prd_prob_po * (1 - prd_prob_po))
}

obslabel <- t(apply(housing2[,1:3], 1, function(x) {
  res <- numeric(3)
  res[which.max(x)] <- 1
  res
}))
resP.plr <- sapply(1:(ncol(obslabel)-1), function(m) {
  obs_m <- rowSums(as.matrix(obslabel[,1:m]))
  fit_m <- rowSums(as.matrix(prd_prob_po[seq_len(nrow(housing2))*3,1:m]))
  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
})

# * Baseline Odds Model ----------------

library(nnet)
?multinom
# The response can be:
# either a factor vector
# or (grouped data format) a matrix of numbers of subjects falling in certain categories (aggregated format according to shared predictor levels).

housing.bo <- multinom(Sat ~ Infl + Type + Cont, data=housing, weights = Freq)

housing.bo2 <- multinom(cbind(Low, Medium, High) ~ ., data = housing2)

# The summary table only provides coefficient estimates and corresponding standard errors.
# You can utilize the Gaussian approximation to perform simple hypothesis testing and obtain approximate p-values.
summary(housing.bo, digit=3)
summary(housing.bo2, digit=3)
# z values
zval.bo <- coef(housing.bo) / summary(housing.bo)$standard.errors
# two-sided p-values
pval.bo <- 2 * pnorm(abs(zval.bo), lower.tail=FALSE)

prd_prob_bo = predict(housing.bo, type = 'prob')
prd_prob_bo = fitted(housing.bo)
head(prd_prob_bo)
prd_labl_bo = predict(housing.bo)
head(prd_labl_bo)

prd_prob_bo2 <- fitted(housing.bo2)

# ** Pearson residuals ----------------

# wrong code:
#resP.bo = (obslabel - prd_prob_bo) / sqrt(prd_prob_bo * (1 - prd_prob_bo))

# a list of (M-1) elements, each element contains the Pearson residuals for one submodel
resP.bo <- sapply(2:ncol(obslabel), function(m) {
  # baseline is column 1 here 
  # otherwise you should replace "1" with the corresponding index and adjust the range of "m" accordingly
  obs_m <- obslabel[rowSums(obslabel[,c(1,m)]) > 0, m]
  fit_m <- prd_prob_bo2[rowSums(obslabel[,c(1,m)]) > 0,c(1,m)]
  fit_m <- fit_m[,2] / rowSums(fit_m)
  (obs_m - fit_m) / sqrt(fit_m * (1 - fit_m))
})

# Don't use housing.bo$residuals!
head(resP.bo)
head(housing.bo2$residuals)
head(obslabel - prd_prob_bo2)

# Count data ----------------

# * Overdispersion in Poisson Model ----------------

# Estimate of the overdispersion parameter = Deviance / (n-p)

# ** Negative Binomial Model ----------------
library(MASS)
?glm.nb()

# glm(formula, family = negative.binomial(theta))
# must specify the overdispersion parameter theta

library(pscl) # containing the bioChemists data
data("bioChemists")
n <- nrow(bioChemists)
# compare Poisson and Negative Binomial Model
poisson <- glm(art ~ ., data = bioChemists, family = poisson())
sigma2 <- poisson$deviance/(n-length(coef(poisson)))
sigma2 # 1.798
# fitting nb without specifying theta
nb1 <- glm.nb(art ~ ., data=bioChemists)
summary(nb1)
nb1$deviance / (n-length(coef(nb1))) # 1.105

# fitting nb with a specified theta
nb2 <- glm(art ~ ., data = bioChemists, family=negative.binomial(nb1$theta))
summary(nb2)
nb2$deviance / (n-length(coef(nb2))) # 1.105

# *** Why are the AICs of nb1 and nb2 different? ----------------
# nb1 needs to estimate theta while theta is specified in nb2

# ** Quasi-Poisson Model ----------------

quasip <- glm(art ~ ., data = bioChemists, family = quasipoisson())
summary(quasip)
quasip$deviance / (n-length(coef(quasip))) # 1.798, same as Poisson model
summary(quasip)$dispersion # 1.829, not 1 as fixed in Poisson model

# * Zero-Inflation ----------------

# ** Zero-Inflated Poisson and Negative Binomial Model ----------------

library(pscl)
?zeroinfl

# Zero-Inflated Poisson Model
zip <- zeroinfl(art ~ . | ., data = bioChemists, dist = 'poisson')
summary(zip)
# Zero-Inflated Negative Binomial Model
zinb <- zeroinfl(art ~ . | ., data = bioChemists, dist = 'negbin')
summary(zinb)

# ** Test for the Existence of Zero-Inflation in R ----------------

# likelihood ratio test
# 2(l(full) - l(null)) converges in distribution to \chi^2_{df(full) - df(null)} under the null.

poi <- glm(art ~., data = bioChemists, family = poisson())

1 - pchisq(as.numeric(2*(logLik(zip) - logLik(poi))),
           df = length(coef(zip)) - length(coef(poi)))
