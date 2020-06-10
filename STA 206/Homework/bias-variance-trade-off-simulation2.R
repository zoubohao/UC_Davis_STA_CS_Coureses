#### A simulation to illustrate bias-variance trade-off in regression analysis

## True regression function (mean response): f(x)
## Predictor values (design points): n points equally spaced on [-3,3]
n=30  ##sample size
X=seq(-3,3,length.out=n) ## design points: fixed throughout the simulation 
f.X=sin(X)+sin(2*X)  ## the values of the true regression function on the design points.

par(lwd=2, cex.lab=1.5, cex.main=1.5) ##customize features of the graph in this R session
plot(X, f.X, type='l',  xlab="x", ylab="f(x)", col=1,  main="true regression function") ## look at the true regression function


## Observations: Y_i=f(x_i)+e_i, e_i ~ i.i.d. N(0, sigma.e), i=1,..., n
sigma.e=0.5 ## error standard deviation: consider 0.5, 2, 5
rep=1000 ## number of independent data sets (replicates) to be generated 

Y=matrix (0, n, rep)  ## matrix to record the observations; each column corresponds to one replicate

for (k in 1:rep){
  set.seed(1234+k*56)    ##set seed for the random number generator; for reproducibility of the result 
  e.c=rnorm(n,0,sigma.e) ##generate the random errors
  Y.c=f.X+e.c   ## generate observations for kth replicate: true mean response + error 
  Y[,k]=Y.c
}


## plot the true regression function with the observations for several replicates
## notice the observations are different from replicate to replicate: this is sampling variability 
par(mfrow=c(3,3)) ## create a plot with 3 by 3 panels
for (k in 1:9){
  plot(X, f.X, type='l', xlab="x", ylab="f(x)", ylim=range(Y), col=1, main=paste("rep",k)) ## true regression function; same across replicates
  Y.c=Y[,k]  
  points(X, Y.c)  ## observations of the kth replicate
}

par(mfrow=c(1,1))

## fit polynomial regression models of order l for each replicate; 
## consider l=1 (linear), 2 (quadratic), 3 (cubic), 5, 8,11
## record the fitted values for each replicate
l.order=c(1,2,3,5, 7,9) ## order of the polynomial models to be fitted
Y.fit=array(0, dim=c(n,rep,length(l.order))) ## record the fitted values; 1st index corresponds to cases; 2nd index corresponds to replicates, 3rd index corresponds to models


for (k in 1:rep){
  Y.c=Y[,k] ##observations of the kth replicate
  
  for (l in 1:length(l.order)){
  fit.c=lm(Y.c ~ poly(X, l.order[l], raw=TRUE)) ## fit a polynomial model with order l.order[l]; raw=TRUE means raw polynomial is used; raw= FALSE mean orthogonal polynomial is used
  Y.fit[,k,l]=fitted(fit.c)
  } ## end of l loop
  
}## end of k loop


## plot  the fitted regression curves with observations for several replicates
## notice the fitted response curves are changing from replicate to replicate: this is due to sampling variability
## notice the 8th  and 11th order models tend to overfit the data, while linear and qudratic models tend to underfit. 

label.m=paste(l.order,"order") ## label for each model 
par(mfrow=c(2,2)) ## create a plot with 2 by 2 panels
for (k in 1:4){
  plot(X, f.X, type='l', xlab="x", ylab="f(x)", lwd=2.5, ylim=range(Y[,1:4]),main=paste("rep",k)) ##true regression function (true mean response curve)
  
  Y.c=Y[,k]  
  points(X, Y.c)  ## observations of the kth replicate
  
   for (l in 1:length(l.order)){
    points(X, Y.fit[,k,l], type='l', col=l+1, lty=l+1, lwd=1.5) ## fitted regression function (fitted mean response curve)
   }## end of l loop
  
  legend(x=-1, y=37,legend=c("true", label.m), col=1:(length(l.order)+1), lty=1:(length(l.order)+1), cex=0.5) ## legend for the plot
  
}## end of k loop

par(mfrow=c(1,1))

## examine model bias:  
## compare the average (across replicates) of the fitted response  curves with the true regression function (true mean response)
## notice the 1st order model and 2nd order model both have large biases; but the higher order models have little bias

## examine model variance:
## overlay the fitted response curves  over the true mean response curve 
## notice that the higher order models have larger sampling variability 

Y.fit.ave=apply(Y.fit, c(1,3), mean) ## average across  replicates (2nd index)

par(mfrow=c(3,2))

for (l in 1:length(l.order)){
plot(X, f.X, type='n', xlab="x", ylab="f(x)", ylim=range(Y.fit), main=paste(l.order[l],"order poly model")) ## set plot axis label/limit, title, etc.

 for (k in 1:rep){
    points(X, Y.fit[,k,l], type='l', lwd=1, col=grey(0.6)) ## fitted response curves of lth model: grey
  }## end of k loop

points(X, f.X, type='l',  col=1) ## true mean response: solid black
points(X, Y.fit.ave[,l], type='l', col=2, lty=2) ## averaged (across replicates) fitted mean reponse of the lth model: broken red

legend(x=-0.5,y=40, legend=c("true", "ave.fit"), col=c(1,2), lty=c(1,2)) ## legend of the plot

}##end l loop
par(mfrow=c(1,1))

## compare SSE; variance, bias^2 and mean-squared-estimation-error = variance+bias^2 across models
SSE=matrix(0, rep, length(l.order)) ## record SSE for each model on each replicate
resi=array(0, dim=c(n,rep, length(l.order))) ## record residuals : residual := obs-fitted
error.fit=array(0, dim=c(n,rep, length(l.order))) ## record estimation errors in the fitted values: error := fitted value - true mean response

for (l in 1:length(l.order)){
  temp=Y-Y.fit[,,l]
  resi[,,l]=temp ## residuals
  SSE[,l]=apply(temp^2,2, sum) ## SSE=sum of squared residuals across cases
  error.fit[,,l]=Y.fit[,,l]-matrix(f.X, n, rep, byrow=FALSE) ## estimation error = fitted value - true mean response
}

### in a simulation study, taking average across replicates (i.e., taking empirical mean) is the counterpart of taking mean/expectation of a random variable
### the larger the number of replicates, the closer the empirical mean would be to the actual mean.
SSE.mean=apply(SSE,2,mean) ## mean SSE (averaged over the replicates); this is the empirical version of E(SSE)
bias=apply(error.fit, c(1,3), mean)  ## bias= mean (averaged across replicates) errors in the fitted values
variance=apply(Y.fit, c(1,3), var) ## variance (across replicates) of the fitted values
err2.mean=apply(error.fit^2,c(1,3), mean) ## mean-squared-estimation errors: squared estimation errors of the fitted values averaged across replicates

### compare SSE.mean with (n-l.order-1)*sigma.e^2; What do you find?  
### note: l.order+1 is the number of regression coefficients p in the lth moder
### for a correct model (models with all important X variables included), E(SSE)=(n-p)*sigma^2
### does this hold for an underfit model (models with some important X variables omitted)?
cbind(SSE.mean, (n-l.order-1)*sigma.e^2)



### check err2.mean=bias^2+variance; this only holds approximately, because 1/(rep-1) is used in sample variance calculation
summary(as.vector(abs(err2.mean-(bias^2+variance)))) ## the discrepancy is reasonably small
summary(as.vector(abs(err2.mean-(bias^2+variance*(rep-1)/rep)))) ## no discrepancy 

### bias, variance, err2.mean are calculated on each design point/case for each model
### to facilitate comparison among models, we sum them across the design points/cases to produce an overall quantity (each) for each model
bias2.ave=apply(bias^2, 2, mean) ## average bias^2 across design points  for each model: overall in-sample bias
variance.ave=apply(variance, 2,mean) ## average variance across design points for each model: overall in-sample variance
err2.mean.ave=apply(err2.mean,2, mean) ## average mean-squared-estimation-error across design points for each model: over-all in-sample msee

### compare variance.ave*n/sigma.e^2 with l.order+1. What do you observe? Can you explain it? 
cbind(variance.ave*n/sigma.e^2, l.order+1)

### plot E(SSE), E(MSE), bias^2, variance, mean-squared-estimation-error against the model order to examine bias-variance trade-off
par(mfrow=c(3,2))
plot(l.order, SSE.mean, type='o',xlab="order of model", ylab="E(SSE)", main="E(SSE)")  
points(l.order, sigma.e^2*(n-l.order-1), type='l',lty=2,col=2)


plot(l.order, SSE.mean/(n-l.order-1), type='o',xlab="order of model", ylab="E(MSE)", main="E(MSE)") 
abline(h= sigma.e^2,  lty=2,col=2)
plot(l.order, bias2.ave, type='o',xlab="order of model", ylab="bias^2", main="squared model bias")

plot(l.order, variance.ave, type='o',xlab="order of model", ylab="variance", main="model variance") 
points(l.order, (l.order+1)*sigma.e^2/n, type='l', lty=2,col=2)

plot(l.order, err2.mean.ave, type='o',xlab="order of model", ylab="mean-squared-estimation-error", main="mean-squared-estimation-error")

par(mfrow=c(1,1))







