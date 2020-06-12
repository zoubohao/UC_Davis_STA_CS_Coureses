

setwd("C:\\Users\\Administrator\\Desktop\\HW2")

train_data = read.csv("train.data.csv")
test_data = read.csv("test.data.csv")
attach(train_data)
############### (a)
model = lm(price ~bedrooms + bathrooms + sqft_living + sqft_lot)
su_model = summary(model)
train_R_Squared = su_model$r.squared
predict_value = predict(model,test_data)
mean_test = mean(test_data$price)
SSTO = sum((test_data$price - mean_test)^2)
SSE = sum((test_data$price - predict_value)^2)
test_R_Squared = 1 - SSE / SSTO

#################(b)
feature_of_house = read.csv("fancyhouse.csv")
guess_price = predict(model,feature_of_house)
# The model say that the price is 15436770$, i think it is reasonable.

################# (c)
model_2 = lm(price ~ bedrooms + bathrooms + sqft_living + sqft_lot + bedrooms * bathrooms)
new_train_R = summary(model_2)$r.squared
new_precit = predict(model_2,test_data)
mean_test = mean(test_data$price)
SSTO = sum((test_data$price - mean_test)^2)
SSE = sum((test_data$price - new_precit)^2)
new_test_R = 1 - SSE / SSTO


library("Matrix")
library("ggplot2")
library("cowplot")
################# (d)
## X : data design matrix
## Y : data label
## ini_beta : the initial beta number
## tau : jump out the while loop. If the norm of gradient smaller than tau, it wll stop while loop.
## yita : learning rate 
## max_iter : the maxinum iteration.
gradient_descent_OLS = function(X,Y,ini_beta,truth_beta,tau,yita,max_iter = 200000){
  beta =   as.matrix(ini_beta)
  k = 0
  norm_vector = c()
  XX = t(X) %*% X
  XY = t(X) %*% Y
  v = norm(beta - truth_beta,"2") / norm(truth_beta,"2")
  while (v >= tau) {
    g = XX %*% beta - XY
    beta = beta - 2 *  yita * g
    v = norm(beta - truth_beta,"2")/ norm(truth_beta,"2")
    if (k >= max_iter){
      break()
    }
    if (k %% 1000 == 0){
      print("#######")
      print("Iteration : ")
      print(k)
      print("Norm of truth beta and current beta: ")
      print(v)
      norm_vector = c(norm_vector,v)
    }
    k = k + 1
  }
  result = list()
  result[[1]] = beta
  result[[2]] = norm_vector
  return(result)
}
R_Squared = function(Y_true,Y_hat){
  mean_Y = mean(Y_true)
  SSTO = sum((Y_true - mean_Y)^2)
  SSE = sum((Y_true - Y_hat)^2)
  return(1 - SSE / SSTO)
}
standerdization = function(x){
  x_s = 1 / sqrt(length(x) - 1) * ((x - mean(x)) / sd(x))
}
### for soving the model of a 

### calculate truth beta for model a
model_a_data_frame = as.data.frame(cbind(standerdization(train_data$price),
                                         standerdization(train_data$bedrooms),
                                         standerdization(train_data$bathrooms),
                                         standerdization(train_data$sqft_living),
                                         standerdization(train_data$sqft_lot)))
model_a = lm(V1 ~ V2 + V3 + V4 + V5,data=model_a_data_frame)
model_a_truth_beta = model_a$coefficients
### GD for calculating beta of model a 
initial_beta = c(0,0,0,0,0)
Y_train = standerdization(train_data$price)
design_a_matrix = cbind(rep(1,dim(train_data)[1]),
                        standerdization(train_data$bedrooms),
                        standerdization(train_data$bathrooms),
                        standerdization(train_data$sqft_living),
                        standerdization(train_data$sqft_lot))
gd_a_result = gradient_descent_OLS(design_a_matrix,
                                  as.matrix(Y_train),
                                  ini_beta = initial_beta,
                                  truth_beta = model_a_truth_beta,
                                  tau = 0.01,yita = 1e-5,max_iter = 8000000)
beta_hat_a = gd_a_result[[1]]
norm_v = gd_a_result[[2]]
plot_data_frame = data.frame(x = c(1:length(norm_v)) , y = norm_v)
p_gd_model_a = ggplot(data = plot_data_frame,aes(x = x,y = y)) + 
  geom_point() + xlab("Iteration") + ylab("Norm of truth beta subtracts estimated beta.") + ggtitle("Gradient descent algorithm for model (a)")
p_gd_model_a


train_a_GD_R_Squared = R_Squared(Y_train,design_a_matrix %*% beta_hat_a)
design_a_test_matrix = cbind(rep(1,dim(test_data)[1]),
                             standerdization(test_data$bedrooms),
                             standerdization(test_data$bathrooms),
                             standerdization(test_data$sqft_living),
                             standerdization(test_data$sqft_lot))
Y_test = standerdization(test_data$price)
test_a_GD_R_Squared = R_Squared(Y_test,design_a_test_matrix %*% beta_hat_a)
### for solving the model of c
multiplication_variable = standerdization(train_data$bedrooms * train_data$bathrooms)
design_c_matrix = cbind(rep(1,dim(train_data)[1]),
                        standerdization(train_data$bedrooms),
                        standerdization(train_data$bathrooms),
                        standerdization(train_data$sqft_living),
                        standerdization(train_data$sqft_lot),
                        multiplication_variable)

### calculate truth beta for model c
model_c_data_frame = as.data.frame(cbind(standerdization(train_data$price),
                                         standerdization(train_data$bedrooms),
                                         standerdization(train_data$bathrooms),
                                         standerdization(train_data$sqft_living),
                                         standerdization(train_data$sqft_lot),
                                         multiplication_variable))
model_c = lm(V1 ~ V2 + V3 + V4 + V5 + multiplication_variable, data=model_c_data_frame)
model_c_truth_beta = model_c$coefficients
####
initial_beta = c(0,0,0,0,0,0)
gd_c_result = gradient_descent_OLS(design_c_matrix,
                                  as.matrix(Y_train),
                                  ini_beta = initial_beta,
                                  truth_beta = model_c_truth_beta,
                                  tau = 0.01,yita = 1e-5,max_iter = 8000000)
beta_hat_c = gd_c_result[[1]]
norm_v = gd_c_result[[2]]
plot_data_frame = data.frame(x = c(1:length(norm_v)) , y = norm_v)
p_gd_model_c = ggplot(data = plot_data_frame,aes(x = x,y = y)) + 
  geom_point() + xlab("Iteration") + ylab("Norm of truth beta subtracts estimated beta.") + ggtitle("Gradient descent algorithm for model (c)")
p_gd_model_c


train_c_GD_R_Squared = R_Squared(Y_train,design_c_matrix %*% beta_hat_c)
design_c_test_matrix = cbind(rep(1,dim(test_data)[1]),
                             standerdization(test_data$bedrooms),
                             standerdization(test_data$bathrooms),
                             standerdization(test_data$sqft_living),
                             standerdization(test_data$sqft_lot),
                             standerdization(test_data$bedrooms * test_data$bathrooms))
test_c_GD_R_Squared = R_Squared(Y_test,design_c_test_matrix %*% beta_hat_c)

plot_grid(p_gd_model_a,p_gd_model_c,ncol = 2)

############ (e)
## X : data design matrix
## Y : data label
## ini_beta : the initial beta number
## yita : learning rate 

SGD_OLS = function(X,Y,ini_beta,truth_beta,yita,tau){
  norm_vector = c()
  samples_num = dim(X)[1]
  beta = as.matrix(ini_beta)
  k = 0
  e = 0
  v = norm(beta - truth_beta,"2") / norm(truth_beta,"2")
  stop_sigrn = T
  while (stop_sigrn) {
    for (j in c(1 : samples_num)){
      thisSample = as.matrix(X[j,])
      beta = beta -  yita * (2 * (sum(thisSample * beta) - Y[j]) * thisSample)
      v = norm(beta - truth_beta,"2") / norm(truth_beta,"2")
      if (k %% 1000 == 0){
        print("#######")
        print("Iteration : ")
        print(k)
        print("Norm of truth beta and current beta: ")
        print(v)
        norm_vector = c(norm_vector,v)
      }
      k = k + 1
      if (v <= tau){
        stop_sigrn = F
        break()
      }
    }
    e = e + 1
    if (e %% 120 == 0){
      yita = yita / 1.5
    }
  }
  result = list()
  result[[1]] = beta
  result[[2]] = norm_vector
  return(result)
}
### for soving the model of a 
initial_beta = c(0,0,0,0,0)
sgd_a_result = SGD_OLS(design_a_matrix,
                       Y_train,
                       initial_beta,
                       model_a_truth_beta,
                       5e-1,
                       0.02)
beta_hat_e_a = sgd_a_result[[1]]
norm_v = sgd_a_result[[2]]
plot_data_frame = data.frame(x = c(1:length(norm_v)) , y = norm_v)
p_sgd_model_a = ggplot(data = plot_data_frame,aes(x = x,y = y)) + 
  geom_point() + xlab("Iteration") + ylab("Norm of truth beta subtracts estimated beta.") + ggtitle("Stochastic gradient descent algorithm for model (a)")
p_sgd_model_a

train_sgd_a_R_Squared = R_Squared(Y_train,design_a_matrix %*% beta_hat_e_a)
test_sgd_a_R_Squared = R_Squared(Y_test,design_a_test_matrix %*% beta_hat_e_a)
### for solving the model of c 
initial_beta = c(0,0,0,0,0,0)
sgd_c_result = SGD_OLS(design_c_matrix,
                       Y_train,
                       initial_beta,
                       model_c_truth_beta,
                       5e-1,
                       0.02)
beta_hat_e_c = sgd_c_result[[1]]
norm_v = sgd_c_result[[2]]
plot_data_frame = data.frame(x = c(1:length(norm_v)) , y = norm_v)
p_sgd_model_c = ggplot(data = plot_data_frame,aes(x = x,y = y)) + 
  geom_point() + xlab("Iteration") + ylab("Norm of truth beta subtracts estimated beta.") + ggtitle("Stochastic gradient descent algorithm for model (c)")
p_sgd_model_c

train_sgd_c_R_Squared = R_Squared(Y_train,design_c_matrix %*% beta_hat_e_c)
test_sgd_c_R_Squared = R_Squared(Y_test,design_c_test_matrix %*% beta_hat_e_c)

plot_grid(p_sgd_model_a,p_sgd_model_c,ncol = 2)











