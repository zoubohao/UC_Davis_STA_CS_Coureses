#hw4
#1
#(a)
data <- read.table("~/Desktop/HW4/exercise.raw", quote="\"", comment.char="")
head(data)
datalong <- reshape(data, idvar = "V1", direction = 'long', varying = c("V3","V4","V5","V6","V7","V8","V9"),
                    v.names = "y", timevar = "day", time = seq(0,12,2))
colnames(datalong) <- c("id", "treatment", "timedays", "measures")
head(datalong)
dim(data)
dim(datalong)
datalong[,4]<-as.numeric(datalong[,4])

#(b)
#(ii)
library(nlme)
model1 <- lme(measures ~ factor(treatment) * timedays, random = ~ timedays | id, data = na.omit(datalong), method = "REML")
summary(model1)
#remember to report your model clearly (especially for coefficients and interpretation)

#(c)
model2 <- lme(measures ~ factor(treatment) * timedays, random = ~ 1 | id, data = na.omit(datalong), method = "REML")
summary(model2)
#remember to do this hypoythesis test with complete steps. (not just say p-value...)

#(d)
predict(model1)[which(na.omit(datalong)[,1]==24)]
na.omit(datalong)[which(na.omit(datalong)[,1]==24),4]
#not complete here!
#should impute the other two time points.

########################

#how to fit multilevel mixed effects model in R
library(lme4)
HSBdata <- read.table("~/Desktop/hsbALL.txt", header=T, sep="")

attach(HSBdata)
head(HSBdata)
colnames(HSBdata) <- c("id", "minority", "female", "ses", "mathach", "size", "sector", "pracad", "disclim", "himinty", "meanses", "ses.centered")
attach(HSBdata)
HSBdata$meanses <- ave(ses, list(id))
HSBdata$centses <- ses - meanses
results1 <- lmer(mathach ~ 1 + (1 | id), data = HSBdata)
summary(results1)
results2 <- lmer(mathach ~ 1 + meanses + sector + (1 | id), data = HSBdata)
summary(results2)
results3 <- lmer(mathach ~ meanses + sector + centses + meanses*centses + sector*centses + (1 + centses|id), data = HSBdata)
summary(results3)
#The third model introduces SES variable and cross-level interaction terms.
#The centered SES slope is treated as random. (by the meaning of this dataset)
#Key part: a school's average SES and sector may interact with student-level SES,
#          thus accounting for some of the variance in the slope.