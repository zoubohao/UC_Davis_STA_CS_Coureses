library(foreign)
fev1 <- read.dta("fev1.dta")
dim(fev1)
head(fev1)
fev1[fev1$id==197,]
fev1 <- subset(fev1, id != 197) 
fev1$logfev1
attach(fev1)
detach(fev1)
loght <- log(ht)           
logbht <- log(baseht)

library(nlme)
library(lme4)

model1 <- lme(logfev1 ~ age + loght + baseage + logbht, random= ~ age | id,method="ML",correlation=corAR1())
summary(model1)#p217
model1$coefficients
model1$sigma
model1$logLik
model1$method
#etc.

model2 <- lme(logfev1 ~ age + loght + baseage + logbht, random= ~ loght | id , correlation=corAR1())
summary(model2)

model3 <- lme(logfev1~ age + loght + baseage + logbht, random= ~ age + loght | id)
summary(model3)
#,method="ML"
#,correlation="corAR1"