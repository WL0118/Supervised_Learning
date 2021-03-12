rm(list = ls())
set.seed(777)
load("/Users/WS_LEE/Desktop/Fall_2020/DataMining/Homework_7/Data/Cleveland.RData")

library(rpart)
library(rpart.plot)

train_ind = sample(1:nrow(cleveland), nrow(cleveland)*0.70)

train = cleveland[train_ind,1:14]
test = cleveland[-train_ind,1:14]

#######################CHART#####################################


model.control <- rpart.control(minsplit = 4, xval = 10, cp = 0)
tree_cleveland<- rpart(diag1~., data = train, method = "class", control = model.control)
x11()
plot(tree_cleveland$cptable[,4], main = "Cp for model selection", ylab = "Cp")

min_cp = which.min(tree_cleveland$cptable[,4])

x11()
plot(tree_cleveland, branch = .3, compress=T, main = "Full Tree")
text(tree_cleveland, cex = .5)

pruned_tree <- prune(tree_cleveland, cp = tree_cleveland$cptable[min_cp,1])

x11()
plot(pruned_tree, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_tree, cex = .5)


pred_full <- predict(tree_cleveland, newdata = test,type="class")
pred_pruned <- predict(pruned_tree, newdata = test,type="class")
Y_result=test$diag1

#pred_train_df = as.data.frame(t(pred_train))
#pred_train_df_full = as.data.frame(t(pred_train_full))


accuracy_full = sum(pred_full == Y_result)/length(Y_result)
accuracy_pruned = sum(pred_pruned == Y_result)/length(Y_result)

############################RF#####################################
rf.fit <- randomForest(diag1~., data = train, n.tree=10000, importance=TRUE)

library(randomForest)
x11()
varImpPlot(rf.fit)

importance(rf.fit)


y_hat_RF <- predict(rf.fit, newdata = test, type = "class")
RF_test_accuracy=sum(y_hat_RF == Y_result)/length(Y_result)
RF_test_accuracy
#################################NNet##############################
library(neuralnet)
library(nnet)
#one nod for my single layer hidden =1
graphics.off()

#normalize <- function(x) {(x - min(x)) / (max(x)-min(x))}
#normalized_train$한국 <- normalize(as.integer(train$한국))
#https://scikit-learn.org/stable/modules/generated/sklearn.neural_network.MLPClassifier.html#sklearn.neural_network.MLPClassifier
library(caret)
train_nn<-read.csv("/Users/WS_LEE/Desktop/Fall_2020/DataMining/Homework_7/Data/train_1_nnet.csv")
test_nn<-read.csv("/Users/WS_LEE/Desktop/Fall_2020/DataMining/Homework_7/Data/test_1_nnet.csv")

#dummy <- dummyVars(" ~.", fullRank = TRUE, data=cleveland) #effectively does what model matrix does

#newdata <- data.frame(predict(dummy, newdata = cleveland))

#head(newdata)
#train_nn = newdata[train_ind,1:14]
#test_nn = newdata[-train_ind,1:14]
library(quartz)
train

nn0 <- neuralnet(diag1~., data = train_nn, hidden = 1, err.fct = "ce", linear.output = FALSE)
quartz()
c11()
plot(nn0)




train_err_store <- c()
test_err_store <- c()

for (i in 1:4){
  
  # fit neural network with "i" neurons
  nn1 <- neuralnet(diag1~., data = train_nn, hidden = i,  err.fct = "ce", linear.output = FALSE)
  
  # calculate the train error
  pred <- predict(nn1, newdata = train_nn)
  y_hat_train <- round(pred)
  train_err <- length(which(train_nn$diag1 != y_hat_train))/length(y_hat_train)
  train_err_store <- c(train_err_store, train_err) #store the error at each iteration
  
  pred <- predict(nn1, newdata = test_nn)
  y_hat_test <- round(pred)
  test_err <- length(which(test_nn$diag1 != y_hat_test))/length(y_hat_test)
  test_err_store <- c(test_err_store, test_err) #store the error at each iteration	
}
train_err_store
test_err_store
