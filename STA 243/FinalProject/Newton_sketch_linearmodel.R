## For linear regression model


## Creat the data

x<- matrix(rnorm(2^12*2^7,0,2),2^12,2^7)
x<- cbind(matrix(rnorm(2^12,1,0),2^12,1),x)
beta_true <- matrix(rnorm(2^7+1,1,2),2^7+1,1)
error<- rnorm(2^12,0,1)
y = x%*%beta_true+ error

beta_sq<- solve(t(x)%*%x)%*%t(x)%*%y

sum((beta_true-beta_sq)^2)

#### Then we consider the Newton sketch
Gradient_beta<- function(x,y,beta_0){
  -2*t(x)%*%y+2*t(x)%*%x%*%beta_0
}

Hessian_beta<- function(x){
  sqrt(2)*x
}

Sketch_matrix<- function(m,n){
  matrix(rnorm(m*n,0,1),m,n)/sqrt(m)
}

Newton_sketch<- function(x,y,m,max_iterate,tolrance){
  beta_0<- matrix(rnorm(dim(x)[2],0,0),dim(x)[2],1)
  for(i in 1:max_iterate){
    grad_beta<- Gradient_beta(x,y,beta_0)
    hessian_squareroot<- Hessian_beta(x)
    sketch_matrix<- Sketch_matrix(m,dim(x)[1])
    S_t<- sketch_matrix%*%hessian_squareroot
    beta<- solve(t(S_t)%*%S_t)%*%(2*t(S_t)%*%S_t%*%beta_0-grad_beta)/2
    beta_0<- beta
    if( ( sum(Gradient_beta(x,y,beta_0)^2) ) < tolrance ){
      break
    }
  }
  return(beta_0)
}
beta_newton_sketch<- Newton_sketch(x,y,500,1000,0.0001)
sum((beta_true-beta_newton_sketch)^2)/sum((beta_true)^2)
