
library(C50)
library(descr)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(MLmetrics)
library(ROCR)
persentage_to_train = 0.8
row_data = read.table("C:\\Users\\15302\\Desktop\\207\\P4\\bank-additional-full.txt",sep = ";",header = T)


### There is no NA data in this data set.
###is.na(row_data)

number_of_data = dim(row_data)[1]

training_number = round(number_of_data * persentage_to_train)

training_data = row_data[1:training_number,]
testing_data = row_data[(training_number + 1) : number_of_data,]

### construct the balanced training set for model selection.
yes_samples_in_training_data = training_data[which(training_data$y == "yes"),]
number_of_no_sample = length(which(training_data$y == "no"))
ratio = round(number_of_no_sample / length(which(training_data$y == "yes")))
new_training = training_data
for( i in c(1:ratio) ){
  new_training = rbind(new_training,yes_samples_in_training_data)
}


### We only consider the addtive model. But why?
### Model selection ###
### As in simple linear regression, we can use AIC for model comparison or in a stepwise model 
### selection routine.The same cautions and pros and cons apply
library("MASS")
finalModel = stepAIC(glm(y~.,data = new_training,family = binomial(link = "logit")),
                     scope = list(upper = as.formula("~."),lower = as.formula("~1")),
                     direction = "both",k=2,steps = 2500)


### Splitting Data
BMdata = row_data
set.seed(123)
index <- createDataPartition(BMdata$y, p = 0.7, list = FALSE)
set.seed(42)
train_data <- BMdata[index, ]
test_data  <- BMdata[-index, ]
set.seed(42)
control <- trainControl(method = "cv",
                        number = 10,
                        classProbs = TRUE,
                        summaryFunction = multiClassSummary)
model_glm <- train(y~age+marital+education+housing+duration+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m,data = train_data,                               method = "glm",family = "binomial", trControl = control)
pred_glm_raw <- predict.train(model_glm,
                              newdata = test_data,
                              type = "raw") # use actual predictions
pred_glm_prob <- predict.train(model_glm,
                               newdata = test_data,
                               type = "prob") # use the probabilities
cm_logistic <- confusionMatrix(data = pred_glm_raw,
                               factor(test_data$y),
                               positive = "yes")


### there are 4640 yes in data and 36548 no in data
### Hosmer-Lemeshow goodness of fit test
### The Hosmer-Lemeshow goodness-of-fit test compares the observed and 
### expected frequencies of events and non-events to assess how well the model fits the data.
### H0: fitted well , H1 fitted not well
fi1 = pred_glm_prob <- predict.train(model_glm,
                                     newdata = train_data,
                                     type = "prob")[,2]
labels = as.numeric(train_data$y)
labels[which(labels==1)] = 0
labels[which(labels==2)] = 1
fi1c=cut(fi1,br=c(0,quantile(fi1,p=seq(.1,.9,.1)),1))
fi1c=cut(fi1,br=c(0,quantile(fi1,p=seq(.1,.9,.1)),1),labels=F)
E=matrix(0,nrow=10,ncol=2)
O=matrix(0,nrow=10,ncol=2)
for(j in 1:10){
  E[j,2]=sum(fi1[fi1c==j])
  E[j,1]=sum((1-fi1)[fi1c==j])
  O[j,2]=sum(labels[fi1c==j])
  O[j,1]=sum((1-labels)[fi1c==j]) }
1-pchisq(sum((O-E)^2/E),8)










