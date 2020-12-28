###################################################################### 
# simulation if cox PH model is assumed, with some continuous covariates; 
# baseline event time is assumed to follow Weibull/exponential distribution
# indepedent (uniform) censoring and Right censoring
###################################################################### 
library(MASS)

sim_cox<- function(N,lambda0, beta, censor.right)
{
  # N = Total sample size 
  # beta = PH coefficients
  # lambda0 = rate parameter of the exponential distribution for baseline
 
  # randomization to treatment or control  
  A <- sample(x=c(0, 1), size=N, replace=TRUE, prob=c(0.5, 0.5))
  
  # generate continuous covariates, mutually indepedent
  X = mvrnorm(N,mu=c(0,0),Sigma=matrix(c(1,0,0,1),2,2))
  X1=X[,1]
  X2=X[,2]
  
  # generate underlying event time
  T <- rweibull(n=N, shape=1, scale = lambda0*exp(beta[1]*A+beta[2]*X1+beta[3]*X2))
  #mean(X)
  #rexp(n=N, rate=lambda0*exp(beta*A))

  # censoring times
  ctime = runif(N, min=0, max=censor.right)
   
  # follow-up times and event indicators
  time <- pmin(T, ctime, censor.right)
  censor <- as.numeric(T>ctime | T>censor.right)
  # data set
  data.frame(id=1:N,
             group=A,
             x1=X1,
             x2=X2,
             time=time,
             censor=censor)
}
mydata=sim_cox(N=1000, lambda0=8, beta=c(0.8,0.5,0.2), censor.right=16)
write.csv(mydata,
          file="/Users/xinerzhou/Dropbox/STABST222 Survival Analysis/SASUniversityEdition/myfolders/Lab4/mydata.csv")

 