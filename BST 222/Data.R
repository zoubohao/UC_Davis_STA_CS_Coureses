
data = read.csv("C:\\Users\\Admin\\Desktop\\BST 222\\BST 222, HW3\\pharmacoSmoking.csv")
library(ggplot2)
ttr_patchOnly = data[which(data$grp == "patchOnly"),]$ttr
qplot(ttr_patchOnly,xlab = "Time in days until relapse", ylab = "Frequency", main = "Patch Only Distribution",bins=7)
ttr_combine = data[which(data$grp == "combination"),]$ttr
qplot(ttr_combine,xlab = "Time in days until relapse", ylab = "Frequency", main = "Combination Distribution",bins = 7)



logLlikelihood = function(beta){
  a = log((exp(4*beta) / (exp(3*beta) + exp(4*beta) + exp(5*beta) + exp(6*beta))) * 
            (exp(3*beta) / (exp(3*beta) + exp(5*beta) + exp(6*beta)) ))
  
  return(a)
}

x = seq(-8,3,0.01)
y = logLlikelihood(x)
plot(x,y,xlab = 'Beta value', ylab = "Log likelihood value")

library(ggplot2)
data = data.frame(x = x, y = y)
p = ggplot(data = data, mapping = aes(x,y)) + geom_line(color = "#69b3a2", size = 2) + 
  xlab("Beta Value") + ylab("Log likelihood value")
p

b = function(x){
  a = - ((4*exp(x)+10*exp(2*x)+18*exp(3*x))*(1+exp(x)+exp(2*x)+exp(3*x)) - 
          (exp(x)+2*exp(2*x)+3*exp(3*x))*(3+4*exp(x)+5*exp(2*x)+6*exp(3*x)) / 
        (1+exp(x)+exp(2*x)+exp(3*x))^2) -
    
        (((10*exp(2*x)+18*exp(3*x))*(1+exp(2*x)+exp(3*x)) - 
            (2*exp(2*x)+3*exp(3*x))*(3+5*exp(2*x))+6*exp(3*x))/
        (1+exp(2*x)+exp(3*x))^2)
    
  return(a)
}
b(-1)

par(
  mfrow=c(1,2),
  mar=c(4,4,1,0)
)
data = read.csv("C:\\Users\\Admin\\Desktop\\BST 222\\BST 222, HW3\\Book1.csv")
nocensor = data[which(data$censor == 0),]
A = nocensor[which(nocensor$type == "A"),1]
D = nocensor[which(nocensor$type == "D"),1]
hist(A, breaks = 7, col = rgb(1,0,0,0.5), xlab = "Time", ylab = "Frequency",
     main = "Distribution of time of Aneuploid tumor type",xlim=c(0,200))
hist(D, breaks = 7, col = rgb(0,0,1,0.5), xlab = "Time", ylab = "Frequency",
     main = "Distribution of time of Diploid tumor type",xlim=c(0,200))
mean(A)
mean(D)










