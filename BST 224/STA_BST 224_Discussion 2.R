#p11
setwd("C:\\Users\\15302\\Desktop\\BST 224")
library(readr)
cows <- read_csv("cows.csv")
cows
mode(cows)
dim(cows)
1337/19
table1 <- table(cows$id)
table1
table2 <- table(table1)
table2

table3 <- matrix(0,dim(table2),3)
table3[,1] <- sort(table2,decreasing = T)
table3[,2] <- table3[,1]/sum(table3[,1])
table3[,3] <- cumsum(table3[,2])
table3
# remember to add columns' names.
# Why different with note? See following useful transformation:

cows<-as.data.frame(as.matrix(cows))# The most difficult part

cowswide <- reshape(cows, idvar="id", v.names="prot", timevar="week", direction="wide", times=c(1:19))
cowswide
dim(cowswide)
head(cowswide)
# Now you can see the patterns.


#13
data1 <- cowswide[,-c(1,2)]
data1
apply(data1,2,mean)# NA remains
apply(na.omit(data1),2,mean)# wrong
apply(data1,2,mean,na.rm=TRUE)# doesn't work
data1p <- apply(data1,2,as.numeric)
meanvec <- apply(data1p,2,mean,na.rm=TRUE)
plot(meanvec)
plot(meanvec,ylim=c(3,4.5),xlim=c(0,20),type="o", xlab = "Week since calving", ylab = "Protein content")
#How to handle NA's
#apply(data1p,2,summary,na.rm=TRUE)
#upper <- unlist(apply(data1p,2,summary))[seq(2,110,6)]
#lower <- unlist(apply(data1p,2,summary))[seq(5,113,6)]
#arrows(1:19, lower, 1:19, upper, length=0.05, angle=90, code=3)

upper <- rep(0,19)
lower <- rep(0,19)
for(i in 1:19){
  upper[i]<-summary(data1p[,i],na.omit=T)[5]
  lower[i]<-summary(data1p[,i],na.omit=T)[2]
}
arrows(1:19, lower, 1:19, upper, length=0.05, angle=90, code=3)


#14
cowswide1 <- cowswide[cowswide$diet=="barley",]
cowswide2 <- cowswide[cowswide$diet=="mixed",]
cowswide3 <- cowswide[cowswide$diet=="lupins",]

data1 <- cowswide1[,-c(1,2)]
data1
apply(data1,2,mean)# NA remains
apply(na.omit(data1),2,mean)# wrong
apply(data1,2,mean,na.rm=TRUE)# doesn't work
data1p <- apply(data1,2,as.numeric)
meanvec <- apply(data1p,2,mean,na.rm=TRUE)
plot(meanvec,ylim=c(3,4.5),xlim=c(0,20),type="o", xlab = "Week since calving", ylab = "Protein content",col=2)

data1 <- cowswide2[,-c(1,2)]
data1
apply(data1,2,mean)# NA remains
apply(na.omit(data1),2,mean)# wrong
apply(data1,2,mean,na.rm=TRUE)# doesn't work
data1p <- apply(data1,2,as.numeric)
meanvec <- apply(data1p,2,mean,na.rm=TRUE)
points(meanvec,ylim=c(3,4.5),xlim=c(0,20),type="o", xlab = "Week since calving", ylab = "Protein content",col=3)

data1 <- cowswide3[,-c(1,2)]
data1
apply(data1,2,mean)# NA remains
apply(na.omit(data1),2,mean)# wrong
apply(data1,2,mean,na.rm=TRUE)# doesn't work
data1p <- apply(data1,2,as.numeric)
meanvec <- apply(data1p,2,mean,na.rm=TRUE)
points(meanvec, ylim = c(3,4.5), xlim = c(0,20),type = "o", xlab = "Week since calving", ylab = "Protein content",col=4)

legend("topright", legend = c("barley","mixed","lupins"), col = c(2,3,4), lty = c(1,1,1))


#15
seizure <- read_csv("seizure.csv")
seizure

seizure<-as.data.frame(as.matrix(seizure))# The most difficult part

seizurewide <- reshape(seizure, idvar="id", v.names="seiz", timevar="time", direction="wide", times=c(0:4))
seizurewide
data2 <- seizurewide[5:9]
data2
seizure$length
matrix(seizure$length,5,59)
data2[,1] <- sqrt(data2[,1]/8)
data2[,2:5] <- sqrt(data2[,2:5]/2)
data2
boxplot(data2)


#17
cd4 <- read_csv("cd4.csv")
cd4
plot(cd4[,3:4])
plot(cd4[,3:4], cex = 0.01)

library("KernSmooth")
par(mfrow=c(2,3))
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 10)
points(z$x,z$y, cex = 0.1)
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 50)
points(z$x,z$y, cex = 0.1)
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 100)
points(z$x,z$y, cex = 0.1)
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 200)
points(z$x,z$y, cex = 0.1)
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 500)
points(z$x,z$y, cex = 0.1)
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 1000)
points(z$x,z$y, cex = 0.1)
par(mfrow=c(1,1))


#21
nepal <- read_csv("nepal.csv")
nepal
data3 <- nepal[,c(4,6)]
data3 <- na.omit(data3)
data3
data3 <- as.matrix(data3)
plot(data3)
trnd <- loess(data3[,2]~data3[,1])
trnd$fitted
#points(data3[,1],trnd$fitted,type = "l") # Why does this not work?
data3[order(data3[,1]),1]
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l")
z <- locpoly(as.matrix(data3[,1]),as.matrix(data3[,2]),bandwidth = 0.1)
points(z$x,z$y,cex = 0.1)

#choose good span
par(mfrow=c(2,3))
plot(data3)
trnd <- loess(data3[,2]~data3[,1],span=0.01)
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l", lwd = 3, col = 2)
plot(data3)
trnd <- loess(data3[,2]~data3[,1],span=0.05)
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l", lwd = 3, col = 2)
plot(data3)
trnd <- loess(data3[,2]~data3[,1],span=0.1)
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l", lwd = 3, col = 2)
plot(data3)
trnd <- loess(data3[,2]~data3[,1],span=0.2)
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l", lwd = 3, col = 2)
plot(data3)
trnd <- loess(data3[,2]~data3[,1],span=0.5)
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l", lwd = 3, col = 2)
plot(data3)
trnd <- loess(data3[,2]~data3[,1],span=1)
points(data3[order(data3[,1]),1],trnd$fitted[order(data3[,1])],type = "l", lwd = 3, col = 2)
par(mfrow=c(1,1))


#22 & #24
#The same with before
#Loess or Kernel


#34
nepal
data4 <- nepal[nepal$sex==2,c(1,2,4)]
data4 <- na.omit(data4)
data4

data4<-as.data.frame(as.matrix(data4))
head(data4)

plot(data4[1:4,2:3],type="l", xlim = c(0,80), ylim = c(0,20), col = data4[1,1])
dim(data4)
head(data4,n=10)
lw <- 10
for(i in 10:422){
  if(data4[i,1]!=data4[i-1,1]){
    up <- i-1
    points(data4[lw:up,2:3],type="l", col = data4[lw,1])
    lw <-i
    }
}
tail(data4)
points(data4[420:422,2:3],type="l", col = data4[420,1])


#36

#Let's recall #17
plot(cd4[,3:4], cex = 0.01)
z <- locpoly(as.matrix(cd4[,3]),as.matrix(cd4[,4]),bandwidth = 500)
points(z$x,z$y, cex = 0.1)
data5 <- cd4[,c(1,3,4)]
data5

data5<-as.data.frame(as.matrix(data5))

points(data5[1:3,2:3],type="l", col = data5[1,1])
dim(data5)
head(data5,n=10)
lw <- 10
for(i in 10:2376){# may just choose a small subset to make figure clear
  if(data5[i,1]!=data5[i-1,1]){
    up <- i-1
    points(data5[lw:up,2:3],type="l", col = data5[lw,1])
    lw <-i
  }
}
tail(data5,n=10)
points(data5[2371:2376,2:3],type="l", col = data4[2371,1])
# I ploted everything here, you may just choose a subset as memntioned in notes.
