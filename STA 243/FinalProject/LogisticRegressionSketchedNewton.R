
### sketch newton algorithm for logistic regressian 
library("Matrix")

sketch_matrix<- function(m,n, if_sketch= TRUE){
  if (if_sketch){
    return(matrix(rnorm(m*n,0,1),m,n) / sqrt(m))
  }
  else{
    return(diag(1,n))
  }
}

##################### This is for gradient
### x : [k,1]
### beta : [k:1]
singleVecGradient = function(x,beta){
  result = exp(t(x) %*% beta) / (1 + exp(t(x) %*% beta))
  return(result)
}

### X [n,k]
### beta [k,1]
gradient = function(X, Y, beta){
  E = apply(X, 1, singleVecGradient,beta = beta)
  #print(E)
  return(t(X) %*% (-Y + E))
}

###################### This is for hessian

### x : [k,1]
### beta : [k:1]
singleVecDiag = function(x,beta){
  result = sqrt(exp(t(x) %*% beta)) / (1 + exp(t(x) %*% beta))
  return(result)
}


### X [n,k]
### beta [k,1]
squareRootHessian = function(X,beta){
  applyResult = apply(X,1,singleVecDiag,beta = beta)
  diagMatrix = diag(applyResult)
  return(diagMatrix %*% X)
}

### Return [m,k] sketched matrix
sketchedSquareRootHMatrix = function(X,beta ,m, if_sketch = T){
  n = dim(X)[1]
  k = dim(X)[2]
  skMatrix = sketch_matrix(m,n,if_sketch)
  return(skMatrix %*% squareRootHessian(X,beta))
}

### Distence
distence = function(X,beta,truthY){
  Y = X %*% beta
  proY = 1 / (1 + exp(-Y))
  return(sum((proY-truthY)^2))
}
library(MASS)
##### Logistic regression sketch newton method
## X : [n,k] data matrix
## Y : label
## ini_beta : [k,1]
## m : skteched dimansion
## lr: learning rate

sketch_Newton = function(X,Y, ini_beta,m,lr,if_sketch = T,displayTimes = 50, max_iter = 100000){
  currentBeta = ini_beta
  disVec = c()
  for (i in 0:max_iter){
    di = distence(X,currentBeta,Y)
    if (i %% displayTimes == 0){
      print(di)
      disVec = c(disVec,di)
    }
    s_HM = sketchedSquareRootHMatrix(X,beta = currentBeta,m = m,if_sketch = if_sketch)
    #print(s_HM)
    inv = ginv(t(s_HM) %*% s_HM)
    #print(inv)
    currentBeta = currentBeta - lr * (inv %*% gradient(X,Y,currentBeta))
    #print(currentBeta)
  }
  return(disVec)
}
X = matrix(rnorm(1000*100),1000)
Y = matrix(sample(c(0,1),size = 1000,replace = T))
iniBeta = matrix(rnorm(100))
m = 100
## Sktech
sketchBeginTime = date()
sktechedDis = sketch_Newton(X,Y,iniBeta,m,lr = 1e-7,if_sketch = T,max_iter = 25000)
sketchEndTime = date()
## No sktech
noSketchBeginTime = date()
noSktechedDis = sketch_Newton(X,Y,iniBeta,m,lr = 1e-6,if_sketch = F,max_iter = 25000)
noSketchEndTime = date()

library("cowplot")
library("ggplot2")
len = length(sktechedDis)
group = c(rep("Sketched Newton",len),rep("Standard Newton",len))
data = c(sktechedDis,noSktechedDis) 
data = data / 1000
x = c(c(1:len),c(1:len))
dataFram = data.frame(x=x,y=data,group = group)
p1 = ggplot(data = dataFram, mapping = aes(x = x, y = y, color = group)) + geom_line(size = 2) + 
  xlab("Iteration Time") + ylab("Mean MSE Loss") + ggtitle("Compare the Sketched Newton with Standard Newton")
p1

### sketch time : 46 , 56
sketchEndTime
sketchBeginTime
### standard time : 1,10,53
noSketchEndTime
noSketchBeginTime


####### appendix
X = matrix(rnorm(500*50),500)
Y = matrix(sample(c(0,1),size = 500,replace = T))
iniBeta = matrix(rnorm(50))
ms = c(5, 25, 45, 65, 85, 105)
disList = list()
## Sktech
k = 1
for (m in ms){
  disList[[k]] = sketch_Newton(X,Y,iniBeta,m,lr = 1e-3,if_sketch = T,max_iter = 25000)
  k = k + 1
}
y = c()
for (i in 1:length(disList)){
  y = c(y, disList[[i]])
}
y = y / 500
len = length(y) / length(disList)
x= c()
for (i in 1:length(disList)){
  x = c(x,c(1:len))
}
group = c()
for (i in 1:length(disList)){
  group = c(group,rep(paste("m = ", ms[i],sep = ""),len))
}
dataFram = data.frame(x=x,y=y,group = group)
p2 = ggplot(data = dataFram, mapping = aes(x = x, y = y, color = group)) + geom_line(size = 2) + 
  xlab("Iteration Time") + ylab("Mean MSE Loss") + ggtitle("Compare the Sketched Newton with different reduced dimension")
p2







