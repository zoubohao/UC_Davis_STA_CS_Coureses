library(foreign)
library(MASS)
library(plyr)
library(dplyr)
library(car)
library(AER)

star = read.spss("E:\\R_workplace\\STAR_Students.sav", to.data.frame = TRUE)

Data<- data.frame(star[,'g1tmathss'],star[,'g1classtype'],star[,'g1schid'], star[,'g1tchid']  )
Data$star....g1schid..<- as.factor(Data$star....g1schid..)
Data$star....g1tchid..<- as.factor(Data$star....g1tchid..)


Data<- na.omit(Data)
colnames(Data)<- c("mathscore", "classtype", "schoolid", "teacherid")

Data.teacher<- data.frame(tAbility=tapply(Data[,1], Data[,4],mean))
class_size<- vector()
school_id<- vector()

for(i in 1:6598)
  class_size[Data[i,4]]=Data[i,2]


for(i in 1:6598)
  school_id[Data[i,4]]=Data[i,3]

Data.teacher<- cbind(Data.teacher, class_size = class_size,school_id )
Data.teacher$class_size <- as.factor(Data.teacher$class_size)
Data.teacher$school_id <- as.factor(Data.teacher$school_id)

boxTrans = function(x,lam){
  return((x^lam - 1) / lam)
}

### log trans 
newData = data.frame(tAbility = Data.teacher$tAbility, classSize = class_size,shcoolID = school_id)
tAbilityV = c()
classSizeV = c()
schoolIDV = c()
for (i in c(1:76)){
  unitTAV = c()
  unitCSV = c()
  unitSIV = c()
  for (j in c(1:3)){
    for (k in c(1:length(Data.teacher$tAbility))){
      if (class_size[k] == j && school_id[k] == i){
        unitTAV = c(unitTAV,newData$tAbility[k])
        unitSIV = c(unitSIV,school_id[k])
        unitCSV = c(unitCSV,class_size[k])
      }
    }
  }
  print(unitCSV)
  if (length(unitCSV)>=3 && 1 %in% unitCSV && 2 %in% unitCSV && 3 %in% unitCSV) {
    tAbilityV = c(tAbilityV,unitTAV)
    classSizeV = c(classSizeV,unitCSV)
    schoolIDV = c(schoolIDV,unitSIV)
  }
}

minMax = function(x){
  minN = min(x)
  maxN = max(x)
  return((x - minN) / (maxN - minN) + 0.0001)
}

deletedNewData = data.frame(tAbility = minMax(tAbilityV),class_size = factor(classSizeV), schoolID = factor(schoolIDV))

### test interaction term
interBox = boxcox(tAbility~class_size + schoolID + schoolID : class_size,data=deletedNewData)
transNumber = interBox$x[order(interBox$y)[length(interBox$y)]]
fullModel = lm(boxTrans(tAbility,transNumber)~class_size + schoolID + schoolID : class_size,data=deletedNewData)
anova(fullModel)

### test school id
schoolidBox = boxcox(tAbility~class_size + schoolID,data = deletedNewData)
transNumber = schoolidBox$x[order(schoolidBox$y)[length(schoolidBox$y)]]
reduce_schoolid = lm(boxTrans(tAbility,transNumber)~class_size + schoolID,data = deletedNewData)
anova(reduce_schoolid)

### test class size
reduce_classSize = lm(boxTrans(tAbility,transNumber)~schoolID + class_size,data = deletedNewData)
anova(reduce_classSize)

### use car package to check again
Anova(reduce_classSize)

### its all significant. so, the model has no interaction term and with those two factors.
### normality test
library("nortest")
lillie.test(boxTrans(deletedNewData$tAbility,transNumber))

### test equal variance levene
classSizeCh = as.character(deletedNewData$class_size)
schoolIDCh = as.character(deletedNewData$schoolID)
iterms = paste(classSizeCh,schoolIDCh,sep = ",")
newIterms = c()
newTAs = c()
for (thisG in unique(iterms)){
  values = c()
  currentIterms = c()
  for (k in c(1:length(iterms))){
    if(iterms[k] == thisG){
      values = c(values,deletedNewData$tAbility[k])
      currentIterms = c(currentIterms,thisG)
    }
  }
  ### the result of 2 data in one cell is significant.
  ### but the reuslt of 3 data in one cell is not significant.
  ### because of there is so small replication, only use 2 data
  ### is not presice at all
  if (length(values)>=3){
    newIterms = c(newIterms,currentIterms)
    newTAs = c(newTAs,values)
  }
}
bartlett.test(boxTrans(newTAs,transNumber) , factor(newIterms))

### Tukey test
alpha = 0.05
Ttest=TukeyHSD(aov(reduce_classSize),conf.level = 1-alpha)
par(mfrow=c(1,2))
plot(Ttest, las=1, col="brown", which=1)


### Test
classSizeCha = as.character(Data.teacher$class_size)
classOri = c()
for (c in classSizeCha){
  if (c == "1"){
    classOri = c(classOri,"Small Class")
  }
  else if (c=="2"){
    classOri = c(classOri,"Regular Class")
  }
  else{
    classOri = c(classOri,"Regular + Aide Class")
  }
}




