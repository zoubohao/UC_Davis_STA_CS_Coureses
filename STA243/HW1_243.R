setwd("C:\\Users\\15302\\Desktop\\STA 243\\")

### 4.(a)
RandomMatrixMultiplication = function(A,B,r){
  prob = c()
  l = dim(A)[2]
  for (i in 1:l){
    prob = c(prob, norm(matrix(A[,i]),type = c("2")) * norm(matrix(B[i,]),type = c("2") ))
  }
  prob = prob / sum(prob)
  
  S = 0
  for (i in 1:r){
    selected = sample(1:l,size = 1, replace = T, prob = prob)
    S = S + outer(A[,selected], B[selected,]) / prob[selected]
  }
  return(S / r)
}

### 4.(b)
A = read.csv("matrix_A.csv")
B = read.csv("matrix_B.csv")
A = as.matrix(A)
B = t(as.matrix(B))
Approx20 = RandomMatrixMultiplication(A,B,20)
Approx50 = RandomMatrixMultiplication(A,B,50)
Approx100 = RandomMatrixMultiplication(A,B,100)
Approx200 = RandomMatrixMultiplication(A,B,200)

### 4.(c)
ApproximateError = function(A,B,E){
  return(norm(E - A%*%B,c("F")) / (norm(A,c("F")) * norm(B,c("F"))))
}
error20 = ApproximateError(A,B,Approx20)
error50 = ApproximateError(A,B,Approx50)
error100 = ApproximateError(A,B,Approx100)
error200 = ApproximateError(A,B,Approx200)

### 4. (d)
par(mfrow=c(2,2))
image(z = abs(Approx20 - A %*% B) * 10000,main = "Estimates of r = 20")
image(z = abs(Approx50 - A %*% B) * 10000,main = "Estimates of r = 50")
image(z = abs(Approx100 - A %*% B) * 10000,main = "Estimates of r = 100")
image(z = abs(Approx200 - A %*% B) * 10000,main = "Estimates of r = 200")


### 5 
power_iteration = function(A, v0, eps = 1e-6, maxiter=100) {
  # Please implement the function power_iteration that takes 
  # in the matrix X and initial vector v0 and returns the eigenvector.
  for (i in 1:maxiter){
    c_t = A %*% v0
    c_t = c_t / (norm(as.matrix(c_t),c("2")) + eps)
  }
  return(c_t)
}

### 6 (1) hadamard(10)
library("pracma")
library("phangorn")
library("Matrix")
generate_canonical = function(i_th,size){
  c = rep(0,size)
  if (0 < i_th && i_th <= size){
    c[i_th] = 1
    return(c)
  }
  else{
    return(NA)
  }
}

### X is a n * d matrix and y is n vector, error is a scalar parameter which is between 0 and 1
sketched_OLS = function(X, y, error){
  X = as.matrix(X)
  y = as.matrix(y)
  n = dim(X)[1]
  rN = 2^floor(log2(n))
  resamplingX = X[1:rN,]
  resamplingY = y[1:rN,]
  d = dim(X)[2]
  r = round(d * log(rN) / error)
  

  diaX = zeros(r,d)
  diaY = zeros(r,1)
  sampleS = sample(c(1:rN),size = r,replace = T,prob = rep(1/rN,rN))
  sampleD = sample(c(1,-1),size = rN,replace = T,prob = c(0.5,0.5))
  for (i in 1:r){
    print(i)
    rD = fhm(generate_canonical(sampleS[i],rN) * sqrt(rN / r)) * sampleD
    diaX[i,] = rD %*% resamplingX
    diaY[i,] = rD %*% resamplingY
  }
  
  print("Calculating Beta.")
  beta = solve(t(diaX) %*% diaX) %*% t(diaX) %*% diaY
  result = list()
  result$beta = beta
  result$diaX = diaX
  result$diaY = diaY
  return(result)
}

DesignMatrix = rand(1048576,20)
Y = rand(1048576,1)
error = 0.1
testResult = sketched_OLS(DesignMatrix,Y,error)

Xstar = testResult$diaX
Ystar = testResult$diaY


timeStar = system.time(solve(t(Xstar) %*% Xstar) %*% t(Xstar) %*% Ystar)
timeStar
timeOri = system.time(solve(t(DesignMatrix) %*% DesignMatrix) %*% t(DesignMatrix) %*% Y)
timeOri
