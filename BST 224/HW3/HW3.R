setwd("C:\\Users\\15302\\Desktop\\BST 224\\HW3")

#### Q2 (a)
ori_aheadld_data = read.csv("aheadld.csv")
ahead_data = ori_aheadld_data
ahead_data$realage = ahead_data$age + ahead_data$year
ahead_data$totword = ahead_data$immword + ahead_data$delword


#### (b)
library(nlme)
library(lme4)
ahead_data$realage = ahead_data$realage - 80
ahead_data$sex = as.factor(ahead_data$sex)
ahead_data$blks = as.factor(ahead_data$blks)
ahead_data$strs = as.factor(ahead_data$strs)
ahead_data$push = as.factor(ahead_data$push)
ahead_data$bag = as.factor(ahead_data$bag)
ahead_data$dime = as.factor(ahead_data$dime)
ahead_data$id = as.factor(ahead_data$id)
lme_model = lme(totword ~  realage + sex + sex : realage + blks + strs + push + bag + dime,
            random = ~1|id,data = ahead_data,correlation = corCompSymm(),method = "REML",na.action = na.omit)
## age CI 95%
upper_age = -0.161945 + qt(0.975,12192) * 0.0087369
lower_age = -0.161945  - qt(0.975,12192) * 0.0087369
## sex CI
upper_sex = 0.802681 + qt(0.975,12192) * 0.07647399
lower_sex = 0.802681 - qt(0.975,12192) * 0.07647399
## interaction CI
upper_in = -0.026594 + qt(0.975,12192) * 0.01082779
lower_in = -0.026594 - qt(0.975,12192) * 0.01082779
## blks CI
upper_blks = -0.135919 + qt(0.975,12192) * 0.0584691
lower_blks = -0.135919 - qt(0.975,12192) * 0.0584691

#### (c)
## var(beta1 - 10beta2) = var(beta1) + 100*var(beta2) - 2 * 10 * cov(beta1,beta2)
effect_70 = 0.802681 - 10 * -0.026594
variance_effect_70 = 0.07647399^2 + 100 * 0.01082779^2 - 2 * 10 * (0.098  * 0.07647399 * 0.01082779)
upper_effect = effect_70 + sqrt(variance_effect_70) * qt(0.975,12192)
lower_effect = effect_70 - sqrt(variance_effect_70) * qt(0.975,12192)

### (d)
L = matrix(0,nrow = 5,ncol = 9)
L[,4:8] = diag(5)
B = as.matrix(c(7.135427,-0.161945,0.802681,-0.135919,-0.281634,-0.217808,-0.184170,-0.009477,-0.026594),nrow = 9)
LB = L %*% B
CLB = L %*% vcov(lme_model) %*% t(L)
W2 = t(LB) %*% solve(CLB) %*% LB
(1-pchisq(W2,df = 5))/2

### (e)
library("geepack")
na_omit_data = na.omit(ahead_data)
gee_model = geeglm(totword ~  realage + sex + sex : realage + blks + strs + push + bag + dime,
                   id = id,data = na_omit_data)
## age
upper_e_age = -0.16132 + qt(0.975,12192) * 0.00925
lower_e_age = -0.16132 - qt(0.975,12192) * 0.00925
## sex 
upper_e_sex = 0.89901 + qt(0.975,12192) * 0.07418
lower_e_sex = 0.89901 - qt(0.975,12192) * 0.07418
## inter
upper_e_in = -0.04326 + qt(0.975,12192) * 0.01167
lower_e_in = -0.04326 - qt(0.975,12192) * 0.01167
### blks
upper_e_blks = -0.25612 + qt(0.975,12192) * 0.06858
lower_e_blks = -0.25612 - qt(0.975,12192) * 0.06858



####  Q3
ori_brithwt = read.table("birthwt.raw")
colnames(ori_brithwt) = c("id","birthorder","birthwt","momage","momage_avg","momage_dev")
birth = ori_brithwt
###(a)
length(unique(birth$id))
length(which(birth$birthorder == 1))
length(which(birth$birthorder == 2))
length(which(birth$birthorder == 3))
length(which(birth$birthorder == 4))
length(which(birth$birthorder == 5))
birth$momage = birth$momage / 10
birth$momage_avg = birth$momage_avg / 10
birth$momage_dev = birth$momage_dev / 10
first_birth = as.factor(as.numeric(birth$birthorder == 1))
birth$first_birth = first_birth

fac_birth = birth
fac_birth$id = as.factor(fac_birth$id)

#### (b)
## mix model
lme_mother_model = lme(fixed = birthwt ~ momage + first_birth , random = ~1|id,method = "ML",
                       data = fac_birth,correlation = corCompSymm())

## (c) fix model
ids = unique(birth$id)
resY = c()
for (oneId in ids){
  indexs = which(birth$id == oneId)
  meanYi = mean(birth[indexs,3])
  resY = c(resY,birth[indexs,3] - meanYi)
}
birth$resY = resY
lm_mother_model = lm(resY ~ -1 + momage_dev,data = birth)
Ui = c()
beta0_Ui = c()
for (oneId in ids){
  indexs = which(birth$id == oneId)
  meanYi = mean(birth[indexs,3])
  meanXi = mean(birth[indexs,4]) * 118
  beta0_Ui = c(beta0_Ui,meanYi - meanXi)
}
beta0 = mean(beta0_Ui)
Ui = c()
for (oneId in ids){
  indexs = which(birth$id == oneId)
  meanYi = mean(birth[indexs,3])
  meanXi = mean(birth[indexs,4]) * 118
  Ui = c(Ui, meanYi - meanXi - beta0 )
}
model = lm(birthwt ~ momage + factor(first_birth) + factor(id),data = fac_birth)





