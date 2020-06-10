
library("AER")
data("Fatalities")

### calculate different fatal ratio
### fatal ratio day
Fatalities$FRD1517 = (Fatalities$fatal1517 - Fatalities$nfatal1517) / Fatalities$pop1517
### fatal ratio night
Fatalities$FRN1517 = Fatalities$nfatal1517 / Fatalities$pop1517
### 
Fatalities$FRD1820 = (Fatalities$fatal1820 - Fatalities$nfatal1820) / Fatalities$pop1820
###
Fatalities$FRN1820 = Fatalities$nfatal1820 / Fatalities$pop1820
###
Fatalities$FRD2124 = (Fatalities$fatal2124 - Fatalities$nfatal2124) / Fatalities$pop2124
###
Fatalities$FRN2124 = Fatalities$nfatal2124 / Fatalities$pop2124
### others situation
Fatalities$FRD_other = (Fatalities$fatal - Fatalities$nfatal - (Fatalities$fatal1517 - Fatalities$nfatal1517) - 
                          (Fatalities$fatal1820 - Fatalities$nfatal1820) - (Fatalities$fatal2124 - Fatalities$nfatal2124)) / (Fatalities$pop - Fatalities$pop1517
                                                                                                                              - Fatalities$pop1820 - Fatalities$pop2124)
Fatalities$FRN_other = (Fatalities$nfatal - Fatalities$nfatal1517 - Fatalities$nfatal1820 - Fatalities$nfatal2124) / (Fatalities$pop - Fatalities$pop1517
                                                                                                                        - Fatalities$pop1820 - Fatalities$pop2124)
factorDrinkAge = c()
drinkAge = Fatalities$drinkage
for (i in c(1:length(drinkAge))){
  if (18 <= drinkAge[i] && drinkAge[i] < 19){
    factorDrinkAge[i] = 1
  }
  else if (19 <= drinkAge[i] && drinkAge[i] < 20){
    factorDrinkAge[i] = 2
  }
  else if (20 <= drinkAge[i] && drinkAge[i] < 21){
    factorDrinkAge[i] = 3
  }
  else{
    factorDrinkAge[i] = 4
  }
}


changeableData = Fatalities
changeableData$drinkage = as.factor(factorDrinkAge)
for (i in c(1:14)){
  changeableData = changeableData[,-17]
}
### fatal involve alcohol
changeableData$FR_IA = Fatalities$afatal / Fatalities$pop
changeableData$FR_NOIA = (Fatalities$fatal - Fatalities$afatal) / Fatalities$pop
changeableData$FR = (Fatalities$fatal) / Fatalities$pop
### Normalization
normalization = function(x){
  return((x - mean(x)) / sd(x))
}
changeableData = changeableData[,-8]
normData = changeableData
### if the type of col is numeric, then do a normalization on it.
for (i in c(1:dim(changeableData)[2])){
  if (is.numeric(normData[,i])){
    normData[,i] = normalization(normData[,i])
  }
  else{
    normData[,i] = normData[,i]
  }
}


### analysis
library("plm")

#######################################
### we only do additive model. why?? ###
#######################################


library("MASS")
normColNames = colnames(normData)
x_variables = normColNames[1:19]
y_variables = normColNames[20:dim(changeableData)[2]]


### remove NA value 
normData = normData[-28,]


FindFinalModelForY = function(Y_Name,x_variables_Names,usedData){
  len = length(x_variables_Names)
  formulaString = paste(Y_Name,"~",sep = "")
  upperString = "~"
  for (i in c(1:(len))){
    if (i == 1){
      formulaString = paste(formulaString,normColNames[i],sep = "")
      upperString = paste(upperString,normColNames[i],sep = "")
    }
    else{
      formulaString = paste(formulaString,normColNames[i],sep = "+")
      upperString = paste(upperString,normColNames[i],sep = "+")
    }
  }
  formulaString = paste(formulaString,"-1",sep = "")
  upperString = paste(upperString,"-1",sep = "")
  fullModel = lm(as.formula(formulaString),data = usedData)
  finalStep = stepAIC(fullModel,scope = list(upper = as.formula(upperString),lower = as.formula(~state + year - 1)), direction = "both",k = 2)
  return(finalStep)
}
### This is the model with Time-Fixed and State-Fixed interaction
selectedModel = FindFinalModelForY("FR",x_variables,normData)
summary(selectedModel)


### The coefficient of dry and miles are not significant, so, we drop those variables.
### because we need to judge if there is relationship with jail, so , we add jail variable to this model.

######### 
## At this step, we need to test weather it needs to contain time-fixed effect
########

#########
### Final model  FR ~ spirits + unemp + income + beertax + jail
#########

state_time_fixed_lm = lm(FR ~  -1 + state + year + spirits + unemp + income + beertax + jail,data = normData)
anova(state_time_fixed_lm)

### model with Time-fixed 
state_time_fixed = plm(FR ~ spirits + unemp + income + beertax + jail,data = normData,index = c("state","year"),model = "within",effect = "twoway")
### model without Time-Fixed
stateFix_Model = plm(FR ~ spirits + unemp + income + beertax  + jail ,data = normData,index = c("state","year"),model = "within")
### Test which model is better.
# Testing time-fixed effects. The null is that no time-fixed effects are needed
pFtest(state_time_fixed, stateFix_Model)
###########
### we need time fixed model
###########



###########
### At this step, we need to test weather we need to use random model or fixed model.
###########

randomModel1 = plm(FR ~ spirits + unemp + income + beertax  + jail ,data = normData,index = c("state","year"),model = "random",effect = "twoway")
randomModel2 = plm(FR ~ spirits + unemp + income + beertax  + jail ,data = normData,index = c("state","year"),model = "random")
phtest(state_time_fixed, randomModel1)
phtest(state_time_fixed, randomModel2)
###########
### ### Fix model is better.
###########


###########
### At this step, we need to test weather we need to use ols model or fixed model.
###########

olsModel = lm(FR ~ spirits + unemp + income + beertax  + jail ,data = normData)
# Testing for fixed effects, null: OLS better than fixed
pFtest(state_time_fixed, olsModel)

###########
### So, we need use state and time fixed model
###########




###############
### The problem of cross-sectional dependence arises 
### if the n individuals in our sample are no longer independently drawn observations but affect each other’s outcomes. For example, 
### this can result from the fact that we look at a set of neighboring countries, which are usually highly interconnected.
#############
###########
### Cross-sectional dependence testing
###########
pcdtest(state_time_fixed, test = c("cd"))
###########
### we conclude that there is NO cross-sectional dependence
###########


###########
### Serial correlation tests apply to macro panels with long time series. Not a problem in micro panels (with very few years).
###########
pbgtest(state_time_fixed)
###########
### There are only 7 years in this micro panels. So, we do not need to do serial correlation test.
### serial correlation in idiosyncratic errors


### Unit root test
library("aTSA")
adf.test(normData$FR)
### It dose not have unit root, it is stationary




### Model dieognosic
################### 
### Normality test
###################
library("nortest")
lillie.test(residuals(state_time_fixed))
#### It is normal because p value is 0.09144


###################
### multicolinearlity 
###################
modelData = data.frame(normData$unemp,normData$spirits,normData$income,normData$beertax)
solve(cor(modelData,modelData))
#### The VIF is not bigger than 10, so, there is a less multicolinearilty between those variables.


###################
### same variance test
###################
fittedValues = fitted.values(state_time_fixed)
residualsS = residuals(state_time_fixed)
plot(x=as.vector(fittedValues),y = as.vector(residualsS))
### From the plot, we can see the variances are not same at all.

###########
### Heteroskedasticity testing,H0) The null hypothesis for the Breusch-Pagan test is homoskedasticity
###########
bptest(FR~ spirits + unemp + income + beertax  + year + state + jail - 1, data = normData, studentize=F)
###########
### Because p-value < 0.05, we detect hetersokedasticity
### If hetersokedasticity is detected we need to use a robust covariance matrix (Sandwich estimator) to account for it
########### arellano" - both heteroskedasticity and serial correlation. Recommended for fixed effects.
coeftest(state_time_fixed)
coeftest(state_time_fixed, vcovHC(state_time_fixed, method = "arellano"))

############
### We think the drink age may have relationship with the fatal ratio, so we add this variable to out final model
drinkModel = lm(FR ~  -1 + state + year + spirits + unemp + income + beertax + jail + drinkage,data = normData)
summary(drinkModel)
anova(drinkModel)
### it is not significant at all.

