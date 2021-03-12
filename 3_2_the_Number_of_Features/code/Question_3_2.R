library(ggplot2)
library(dplyr)
library(leaps)
library(caret)

rm(list = ls())
test_2 <-read.csv('Test_2.csv')
train_2 <-read.csv('Train_2.csv')


best_sub = regsubsets(P20~., data=train_2, nvmax = 20, method = "exhaustive")
summary(best_sub)
train_pred_matrix = model.matrix(P20 ~., data = train_2)
test_pred_matrix = model.matrix(P20 ~., data = test_2)
train_errors = rep(NA,20)
test_errors = rep(NA,20)



for (i in 1:20) {
  coefi = coef(best_sub, id = i)
  pred_train <- train_pred_matrix[,names(coefi)] %*% coefi
  train_errors[i] = mean((train_2$P20 - pred_train)^2)
  pred_test <- test_pred_matrix[,names(coefi)] %*% coefi
  test_errors[i] = mean((test_2$P20 - pred_test)^2)
}
x <- seq (1, 20 , 1)
x11()
plot(x,test_errors,xlab = "Number of Variables", ylab = "Test_data_MSE",type ="o",col='red')
lines(x,train_errors,xlab = "Number of Variables", ylab = "MSE",type ="o",col='green')

plot(x,train_errors,xlab = "Number of Variables", ylab = "Train_data_MSE",type ="o",col='green')

train_errors = data.frame(train_errors)
fix(train_errors)
