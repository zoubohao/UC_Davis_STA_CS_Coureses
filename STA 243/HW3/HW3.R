setwd("C:/Users/15302/Desktop/STA 243/HW3")


############## Q1
## load MNIST image data
load_image_file = function(filename) {
  ret = list()
  f = file(filename,'rb')
  readBin(f, integer() ,n=1, endian='big') #  Magic number
  ret$n = readBin(f,integer(),n=1,endian='big')
  nrow = readBin(f,integer(),n=1,endian='big')
  ncol = readBin(f,integer(),n=1,endian='big')
  x = readBin(f,integer(),n=ret$n*nrow*ncol,size=1,signed=F)
  ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
  close(f)
  ret
}

## load MNIST label data
load_label_file <- function(filename) {
  f = file(filename,'rb')
  readBin(f, integer() ,n=1 ,endian='big') # Magic number
  n = readBin(f, integer(),n=1,size=4,endian='big')
  y = readBin(f, integer(),n=n,size=1,signed=F)
  close(f)
  y
}

train_total = load_image_file("train-images.idx3-ubyte")
train_labels = load_label_file("train-labels.idx1-ubyte")

train_sub = train_total$x[train_labels %in% c(0,1,2,3,4),]
train_sub_labels = train_labels[train_labels %in% c(0,1,2,3,4)]

test_total = load_image_file("t10k-images.idx3-ubyte")
test_labels = load_label_file("t10k-labels.idx1-ubyte")

test_sub = test_total$x[test_labels %in% c(0,1,2,3,4),]
test_sub_labels = test_labels[test_labels %in% c(0,1,2,3,4)]

pooling = function(im){
  ret = vector(length = 14 * 14)
  for (i in 1:14){
    for (j in 1:14){
      ret[(i - 1) * 14 + j] = ( im[(2*i - 2)*28 + (2 * j-1)] + im[(2*i - 2)*28 + (2 * j)] + 
                                  im[(2*i - 1)*28 + 2 * j-1]+ im[(2*i - 1)*28 + (2 * j)]) / 4
    }
  }
  return(ret)
}

### Normalization the data set to 0 - 1.
xTrainMatrix = t(apply(train_sub, 1, pooling)) / 255.
train_labels = train_sub_labels

xTestMatrix = t(apply(test_sub, 1, pooling)) / 255
test_labels = test_sub_labels


############# Q3
# ajs : [k]
# bjs : [k]
log_sum_exp_trick = function(ajs,bjs){
  sum_t = 0
  A = max(ajs)
  k = length(ajs)
  for (i in 1:k){
    sum_t = sum_t + bjs[i] * exp(ajs[i] - A)
  }
  return(A + log(sum_t))
}


############## 
# xMatrix : shape [n,d]
# piVectors : [k]
# muMatrix : [k,d]
# diagMatrix :[k,d]

# n : samples number
# k : clusters number 
# d : the dimensions of one sample.

### This will return a list, the first element is the value of loglikehood
### the second value is Fij matrix. Fij matrix : [n, k]
logLikelihood = function(xMatrix,piVectors,muMatrix,diagMatrix){
  n = dim(xMatrix)[1]
  k = length(piVectors)
  d = dim(xMatrix)[2]
  log_t = 0
  FMatrix = matrix(rep(0,n * k),n,k)
  for (i in c(1:n)){
    ajs = c()
    bjs = c()
    Fis = c()
    if (i %% 10000 == 0){
      print(i)
    }
    for (j in c(1:k)){
      ajs = c(ajs,-0.5 * sum( (1 / diagMatrix[j,]) * (xMatrix[i,] - muMatrix[j,])^2) )
      bjs = c(bjs,piVectors[j] / exp(0.5 * sum(log(diagMatrix[j,]))))
    }
    inner_Log_sum = log_sum_exp_trick(ajs,bjs)
    for (j in 1:k){
      Fis = c(Fis,exp(log(bjs[j]) + ajs[j] - inner_Log_sum))
    }
    log_t = log_t + inner_Log_sum
    FMatrix[i,] = Fis
  }
  result = list()
  result[[1]] = log_t
  result[[2]] = FMatrix
  return(result)
}

### 
max_frequency = function(x_vectors){
  uniqueX = unique(x_vectors)
  frequencyNum = c()
  for (i in uniqueX){
    frequencyNum = c(frequencyNum, length(which(x_vectors == i)))
  }
  return(max(frequencyNum))
}


### Because the GM is a cluster algorithm, the label of each data may not match
### the true labels. This is because GM random assign label to one cluster.
# Fmatrix, [n, k]
# labels [n]
# clustersNum : a number which is k
acc_compute = function(Fmatrix,labels,clustersNum){
  clusters = c(1:clustersNum)
  indexesC = list()
  if (0 %in% labels){
    labels = labels + 1
  }
  for (i in clusters){
    indexesC[[i]] = which(labels == i)
  }
  predictLables = c()
  n = dim(Fmatrix)[1]
  for (i in 1:n){
    predictLables = c(predictLables,which.max(Fmatrix[i,]))
  }
  acc = 0
  for (i in clusters){
    thisPredictLables = predictLables[indexesC[[i]]]
    currentACC = max_frequency(thisPredictLables) / length(thisPredictLables)
    acc = acc + currentACC
  }
  return(acc / clustersNum)
}


############## 
# xMatrix : shape [n,d]
# pi_t : [k]
# muMatrix_t : [k,d]
# diagMatrix_t :[k,d]

## Return a list, the first is pi_t, the second is the mean matrix which shape is [k,d]
## The third is the diaganal elements of one diagianl matrix, so, the shap is [k,d]
EM_SGM = function(xMatrix,pi_t, muMatrix_t,diagMatrix_t,eps = 0.0001){
  k = length(pi_t)
  n = dim(xMatrix)[1]
  d = dim(xMatrix)[2]
  parameters = list()
  # Step 1 , compute loglikelihood 
  t_result = logLikelihood(xMatrix,pi_t,muMatrix_t,diagMatrix_t)
  FMatrix_t = t_result[[2]] # n * k
  # Step 2 , update the parameters 
  for (j in 1:k){
    # pi update
    sum_Fj = sum(FMatrix_t[,j])
    pi_t[j] = sum_Fj / n
    
    # mu update
    sum_Fij_xi = 0
    for (i in 1:n){
      sum_Fij_xi = sum_Fij_xi + FMatrix_t[i,j] * xMatrix[i,]
    }
    muMatrix_t[j,] = sum_Fij_xi / sum_Fj
    
    # sigma update
    sum_Fij_xi_muj = 0
    for (i in 1:n){
      sum_Fij_xi_muj = sum_Fij_xi_muj + FMatrix_t[i,j] * sum((xMatrix[i,] - muMatrix_t[j,])^2)
    }
    sigmaj = sum_Fij_xi_muj / (d * sum_Fj) + 0.05
    diagMatrix_t[j,] = rep(sigmaj,d)
    
  }
  parameters[[1]] = pi_t
  parameters[[2]] = muMatrix_t
  parameters[[3]] = diagMatrix_t
  
  tA1_result = logLikelihood(xMatrix,pi_t,muMatrix_t,diagMatrix_t)
  log_likeli_t = t_result[[1]]
  log_likeli_tA1 = tA1_result[[1]]
  print(paste("Loglikehood of t :",log_likeli_t))
  print(paste("Loglikehood of t + 1 :",log_likeli_tA1))
  changeValue = abs(log_likeli_t - log_likeli_tA1)
  print(paste("Changed value : ",changeValue))
  while(changeValue > eps){
    t_result = tA1_result
    FMatrix_t = t_result[[2]] # n * k
    # Step 2 , update the parameters 
    for (j in 1:k){
      sum_Fj = sum(FMatrix_t[,j])
      pi_t[j] = sum_Fj / n
      
      
      sum_Fij_xi = 0
      for (i in 1:n){
        sum_Fij_xi = sum_Fij_xi + FMatrix_t[i,j] * xMatrix[i,]
      }
      muMatrix_t[j,] = sum_Fij_xi / sum_Fj
      
      sum_Fij_xi_muj = 0
      for (i in 1:n){
        sum_Fij_xi_muj = sum_Fij_xi_muj + FMatrix_t[i,j] * sum((xMatrix[i,] - muMatrix_t[j,])^2)
      }
      sigmaj = sum_Fij_xi_muj / (d * sum_Fj) + 0.05
      diagMatrix_t[j,] = rep(sigmaj,d)
      
    }
    parameters[[1]] = pi_t
    parameters[[2]] = muMatrix_t
    parameters[[3]] = diagMatrix_t
    tA1_result = logLikelihood(xMatrix,pi_t,muMatrix_t,diagMatrix_t)
    log_likeli_t = t_result[[1]]
    log_likeli_tA1 = tA1_result[[1]]
    print(paste("Loglikehood of t :",log_likeli_t))
    print(paste("Loglikehood of t + 1 :",log_likeli_tA1))
    changeValue = abs(log_likeli_t - log_likeli_tA1)
    print(paste("Changed value : ",changeValue))
  }
  return(parameters)
}

### SGM ini 1
iniPiVec = c(0.2,0.2,0.2,0.2,0.2)
iniMuMatrix = matrix(rnorm(5 * 14 * 14,mean = 0),nrow = 5, ncol = 14 * 14)
iniDiagMatrix = matrix(rep(abs(rnorm(1,mean = 0)),5 * 14 * 14) + 1,nrow = 5) ## The ini must be positive value

resultP_SGM1 = EM_SGM(xTrainMatrix,iniPiVec,iniMuMatrix,iniDiagMatrix)
sgm_Fmatrix1Train = logLikelihood(xTrainMatrix,resultP_SGM1[[1]],resultP_SGM1[[2]],resultP_SGM1[[3]])[[2]]
acc_of_sgm1Trian = acc_compute(sgm_Fmatrix1Train,train_labels, 5)
acc_of_sgm1Trian

sgm_Fmatrix1Test = logLikelihood(xTestMatrix,resultP_SGM1[[1]],resultP_SGM1[[2]],resultP_SGM1[[3]])[[2]]
acc_of_sgm1Test = acc_compute(sgm_Fmatrix1Test,test_labels, 5)
acc_of_sgm1Test


### SGM ini 2
iniPiVec = c(0.1,0.2,0.4,0.2,0.1)
iniMuMatrix = matrix(rnorm(5 * 14 * 14,mean = 1),nrow = 5, ncol = 14 * 14)
iniDiagMatrix = matrix(rep(abs(rnorm(1,mean = 1)),5 * 14 * 14) + 1,nrow = 5) ## The ini must be positive value

resultP_SGM2 = EM_SGM(xTrainMatrix,iniPiVec,iniMuMatrix,iniDiagMatrix)
sgm_Fmatrix2Train = logLikelihood(xTrainMatrix,resultP_SGM2[[1]],resultP_SGM2[[2]],resultP_SGM2[[3]])[[2]]
acc_of_sgm2Trian = acc_compute(sgm_Fmatrix2Train,train_labels, 5)
acc_of_sgm2Trian

sgm_Fmatrix2Test = logLikelihood(xTestMatrix,resultP_SGM2[[1]],resultP_SGM2[[2]],resultP_SGM2[[3]])[[2]]
acc_of_sgm2Test = acc_compute(sgm_Fmatrix2Test,test_labels, 5)
acc_of_sgm2Test


### SGM ini 3
iniPiVec = c(0.2,0.1,0.4,0.1,0.2)
iniMuMatrix = matrix(rnorm(5 * 14 * 14,mean = 2),nrow = 5, ncol = 14 * 14)
iniDiagMatrix = matrix(rep(abs(rnorm(1,mean = 2)),5 * 14 * 14) + 1,nrow = 5) ## The ini must be positive value

resultP_SGM3 = EM_SGM(xTrainMatrix,iniPiVec,iniMuMatrix,iniDiagMatrix)
sgm_Fmatrix3Train = logLikelihood(xTrainMatrix,resultP_SGM3[[1]],resultP_SGM3[[2]],resultP_SGM3[[3]])[[2]]
acc_of_sgm3Trian = acc_compute(sgm_Fmatrix3Train,train_labels, 5)
acc_of_sgm3Trian

sgm_Fmatrix3Test = logLikelihood(xTestMatrix,resultP_SGM3[[1]],resultP_SGM3[[2]],resultP_SGM3[[3]])[[2]]
acc_of_sgm3Test = acc_compute(sgm_Fmatrix3Test,test_labels, 5)
acc_of_sgm3Test

### DGM 
## Return a list, the first is pi_t, the second is the mean matrix which shape is [k,d]
## The third is the diaganal elements of one diagianl matrix, so, the shap is [k,d]
EM_DGM = function(xMatrix,pi_t, muMatrix_t,diagMatrix_t,eps = 0.0001){
  k = length(pi_t)
  n = dim(xMatrix)[1]
  d = dim(xMatrix)[2]
  parameters = list()
  # Step 1 , compute loglikelihood 
  t_result = logLikelihood(xMatrix,pi_t,muMatrix_t,diagMatrix_t)
  FMatrix_t = t_result[[2]] # n * k
  # Step 2 , update the parameters 
  for (j in 1:k){
    ### pi update
    sum_Fj = sum(FMatrix_t[,j])
    pi_t[j] = sum_Fj / n
    
    ### mu update
    sum_Fij_xi = 0
    for (i in 1:n){
      sum_Fij_xi = sum_Fij_xi + FMatrix_t[i,j] * xMatrix[i,]
    }
    muMatrix_t[j,] = sum_Fij_xi / sum_Fj
    
    ### sigma update
    sum_Fij_xi_muj = 0
    for (i in 1:n){
      sum_Fij_xi_muj = sum_Fij_xi_muj + FMatrix_t[i,j] * (xMatrix[i,] - muMatrix_t[j,])^2
    }
    sigmajs = sum_Fij_xi_muj / sum_Fj + 0.05
    diagMatrix_t[j,] = sigmajs
    
  }
  parameters[[1]] = pi_t
  parameters[[2]] = muMatrix_t
  parameters[[3]] = diagMatrix_t
  tA1_result = logLikelihood(xMatrix,pi_t,muMatrix_t,diagMatrix_t)
  log_likeli_t = t_result[[1]]
  log_likeli_tA1 = tA1_result[[1]]
  print(paste("Loglikehood of t :",log_likeli_t))
  print(paste("Loglikehood of t + 1 :",log_likeli_tA1))
  changeValue = abs(log_likeli_t - log_likeli_tA1)
  print(paste("Changed value : ",changeValue))
  while(changeValue > eps){
    t_result = tA1_result
    FMatrix_t = t_result[[2]] # n * k
    # Step 2 , update the parameters 
    for (j in 1:k){
      sum_Fj = sum(FMatrix_t[,j])
      pi_t[j] = sum_Fj / n
      
      
      sum_Fij_xi = 0
      for (i in 1:n){
        sum_Fij_xi = sum_Fij_xi + FMatrix_t[i,j] * xMatrix[i,]
      }
      muMatrix_t[j,] = sum_Fij_xi / sum_Fj
      
      ### sigma update
      sum_Fij_xi_muj = 0
      for (i in 1:n){
        sum_Fij_xi_muj = sum_Fij_xi_muj + FMatrix_t[i,j] * (xMatrix[i,] - muMatrix_t[j,])^2
      }
      sigmajs = sum_Fij_xi_muj / sum_Fj + 0.05
      diagMatrix_t[j,] = sigmajs
      
    }
    parameters[[1]] = pi_t
    parameters[[2]] = muMatrix_t
    parameters[[3]] = diagMatrix_t
    tA1_result = logLikelihood(xMatrix,pi_t,muMatrix_t,diagMatrix_t)
    log_likeli_t = t_result[[1]]
    log_likeli_tA1 = tA1_result[[1]]
    print(paste("Loglikehood of t :",log_likeli_t))
    print(paste("Loglikehood of t + 1 :",log_likeli_tA1))
    changeValue = abs(log_likeli_t - log_likeli_tA1)
    print(paste("Changed value : ",changeValue))
  }
  return(parameters)
}

### DGM ini 1
iniPiVec = c(0.2,0.2,0.2,0.2,0.2)
iniMuMatrix = matrix(rnorm(5 * 14 * 14,mean = 0),nrow = 5, ncol = 14 * 14)
iniDiagMatrix = matrix(abs(rnorm(5 *14 * 14,mean = 0)) + 1,nrow = 5) ## The ini must be positive value

resultP_DGM1 = EM_DGM(xTrainMatrix,iniPiVec,iniMuMatrix,iniDiagMatrix)
dgm_Fmatrix1Train = logLikelihood(xTrainMatrix,resultP_DGM1[[1]],resultP_DGM1[[2]],resultP_DGM1[[3]])[[2]]
acc_of_dgm1Train = acc_compute(dgm_Fmatrix1Train,train_labels, 5)
acc_of_dgm1Train

dgm_Fmatrix1Test = logLikelihood(xTestMatrix,resultP_DGM1[[1]],resultP_DGM1[[2]],resultP_DGM1[[3]])[[2]]
acc_of_dgm1Test = acc_compute(dgm_Fmatrix1Test,test_labels,5)
acc_of_dgm1Test

### DGM ini 2
iniPiVec = c(0.1,0.2,0.4,0.2,0.1)
iniMuMatrix = matrix(rnorm(5 * 14 * 14,mean = 1),nrow = 5, ncol = 14 * 14)
iniDiagMatrix = matrix(abs(rnorm(5 *14 * 14,mean = 1)) + 1,nrow = 5) ## The ini must be positive value

resultP_DGM2 = EM_DGM(xTrainMatrix,iniPiVec,iniMuMatrix,iniDiagMatrix)
dgm_Fmatrix2Train = logLikelihood(xTrainMatrix,resultP_DGM2[[1]],resultP_DGM2[[2]],resultP_DGM2[[3]])[[2]]
acc_of_dgm2Train = acc_compute(dgm_Fmatrix2Train,train_labels, 5)
acc_of_dgm2Train

dgm_Fmatrix2Test = logLikelihood(xTestMatrix,resultP_DGM2[[1]],resultP_DGM2[[2]],resultP_DGM2[[3]])[[2]]
acc_of_dgm2Test = acc_compute(dgm_Fmatrix2Test,test_labels,5)
acc_of_dgm2Test


### DGM ini 3
iniPiVec = c(0.2,0.1,0.4,0.1,0.2)
iniMuMatrix = matrix(rnorm(5 * 14 * 14,mean = 2),nrow = 5, ncol = 14 * 14)
iniDiagMatrix = matrix(abs(rnorm(5 *14 * 14,mean = 0)) + 1,nrow = 5) ## The ini must be positive value

resultP_DGM3 = EM_DGM(xTrainMatrix,iniPiVec,iniMuMatrix,iniDiagMatrix)
dgm_Fmatrix3Train = logLikelihood(xTrainMatrix,resultP_DGM3[[1]],resultP_DGM3[[2]],resultP_DGM3[[3]])[[2]]
acc_of_dgm3Train = acc_compute(dgm_Fmatrix3Train,train_labels, 5)
acc_of_dgm3Train

dgm_Fmatrix3Test = logLikelihood(xTestMatrix,resultP_DGM3[[1]],resultP_DGM3[[2]],resultP_DGM3[[3]])[[2]]
acc_of_dgm3Test = acc_compute(dgm_Fmatrix3Test,test_labels,5)
acc_of_dgm3Test

#######################################################################


