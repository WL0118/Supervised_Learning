rm(list = ls())
set.seed(777)
library(randomForest)
library(trainControl)
library(caret)
SPAM = read.csv("Data/SPAM.csv",header = FALSE,sep=' ')


train_ind = sample(1:nrow(SPAM), nrow(SPAM)*0.60)

train = SPAM[train_ind,]
test = SPAM[-train_ind,]
error_store_test <- c()
error_store <- c()
for(i in 4:8){
  
  model = randomForest(x=train[1:57], y=as.factor(train[,58]), ntree=500, mtry = i, na.action = na.omit )
  y_hat <- predict(model, newdata = test[1:57], type = "response")
  predicted_y<-y_hat
  yy <- as.factor(test$V58)
  conf_mat<-confusionMatrix(y_hat, yy)
  acc_RF<-conf_mat$overall[1]
  error_store_test <- c(error_store_test, unname(acc_RF))
  
  print(model$err.rate[,1])
  error_store <- c(error_store, sum(model$err.rate[,1])/500)
}

model <- randomForest(x=train[1:57], y=as.factor(train[,58]), ntree=500, mtry = 30, importance=TRUE )
print(sum(model$err.rate[,1])/500)

metric <- "Accuracy"
mtry<- sqrt(57)
control <- trainControl(method="boot", search="random")

bestmtry<-tuneRF(train[1:57],as.factor(train[,58]), stepFactor = 1.3,mtryStart =mtry,improve = 1e-5,  ntree=500)

mtry_num<- 4:8
plot(x=mtry_num,y=error_store_test, main = "test error by mtry", ylab = "Accuracy")
plot(y=error_store,x= mtry_num, main = "train error by mtry", ylab = "Accuracy")


error_store_test_ntree <- c()
error_store_ntree <- c()
tree_num<- 1:10*100
for(i in 1:10){
  
  model = randomForest(x=train[1:57], y=as.factor(train[,58]), ntree=i*100, mtry = 5, na.action = na.omit )
  y_hat <- predict(model, newdata = test[1:57], type = "response")
  predicted_y<-y_hat
  yy <- as.factor(test$V58)
  conf_mat<-confusionMatrix(y_hat, yy)
  acc_RF<-conf_mat$overall[1]
  error_store_test_ntree <- c(error_store_test, unname(acc_RF))
  
  print(model$err.rate[,1])
  error_store_ntree <- c(error_store, sum(model$err.rate[,1]))
}
plot(error_store_test_ntree, main = "test error by mtry", ylab = "Accuracy")

