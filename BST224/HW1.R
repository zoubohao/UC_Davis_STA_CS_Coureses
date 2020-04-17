library("readr")
setwd("C:\\Users\\15302\\Desktop\\BST 224\\")
data = read.table("afcr.raw",header = F)
colnames(data) = c("id","time","AFCR","group","priorTI","age")

### cowswide <- reshape(cows, idvar="id", 
### v.names="prot", timevar="week", direction="wide", times=c(1:19))

### convert to wide data set and explore the x-covarites distribution
### Explore the x-covariates first.
wideData = reshape(data,idvar = "id", v.names = "AFCR" , 
                   timevar = "time",direction = "wide", times = c(1, 3, 6, 9, 12, 15,18))
library("cowplot")
library("ggplot2")
ageP = ggplot() + geom_histogram(data = wideData, aes(x = age) ,
                 binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("The distribution of age")

priorONum = length(which(wideData$priorTI == 0))
prior1Num = 150 - priorONum
priorDis = data.frame(group = c("No prior treatment", "Prior treatment"),
                      value = c(priorONum,prior1Num))
priorP = ggplot(priorDis,aes(x="",y=value,fill = group)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + ggtitle("The distribution of prior treatment") 
plot_grid(ageP,priorP,ncol = 2)
  
### At this step, we can explore the time variable.






