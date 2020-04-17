



### data is n x p data frame
### information ratio is the number between [0,1]
PCA = function(data,informationSaveRatio = 0.9,manualSelectFeature = FALSE, selectedNumber = 2){
  sampleCovariance = cov(data)
  eigInfor = eigen(sampleCovariance)
  eigVectors = eigInfor$vectors
  eigValues = eigInfor$values
  if (manualSelectFeature == FALSE){
    totalVariance = sum(eigValues)
    add = 0
    k = 0
    for (i in c(1:length(eigValues))){
      add = add + eigValues[i]
      if (add / totalVariance >= informationSaveRatio){
        k = i
        break()
      }
    }
    savedEigVectors = eigVectors[,1:k]
    return(data %*% savedEigVectors)
  }
  else{
    savedEigVectors = eigVectors[,1:selectedNumber]
    return(data %*% savedEigVectors)
  }
}

normalization = function(data){
  sampleNumber = dim(data)[1]
  features = dim(data)[2]
  matrixR = c(1:sampleNumber)
  for (i in c(1:features)){
    minz = min(data[,i])
    maxz = max(data[,i])
    matrixR = cbind(matrixR,(data[,i] - minz) / (maxz - minz))
  }
  return(matrixR[,-1])
}

imagePath = "F:\\Dog.jpg"
PCA_Save_Path = "F:\\Dog_PCA.jpg"
scaleR = 2
library("jpeg")
img = readJPEG(imagePath)
RMatrix = img[,,1]
GMatrix = img[,,2]
BMatrix = img[,,3]
sampleNumber = dim(RMatrix)[1]
featureNumber = dim(RMatrix)[2]
RPCA = normalization(PCA(RMatrix,manualSelectFeature = T,selectedNumber = round(featureNumber  / scaleR)))
GPCA = normalization(PCA(GMatrix,manualSelectFeature = T,selectedNumber = round(featureNumber  / scaleR)))
BPCA = normalization(PCA(BMatrix,manualSelectFeature = T,selectedNumber = round(featureNumber  / scaleR)))
imgPCA = array(c(RPCA,GPCA,BPCA),dim = c(sampleNumber,round(featureNumber / scaleR),3)) * 255.0
writeJPEG(imgPCA,target = PCA_Save_Path)


