rm(list = ls())
library(ISLR)
library(bootstrap)
library(boot)  #install.packages("bootstrap")
library(leaps)
library(gbm)
library(rpart)
library(rpart.plot)
library(tidyverse)
set.seed(777)
load("Data/pima.RData")





train_ind = sample(1:nrow(pima), nrow(pima)*0.70)

train = pima[train_ind,0:8]
test = pima[-train_ind,0:8]

X_4_train = train
X_4_test = test


X_modi <- train[,1:8]
X <- train[,1:7]
Y <- train[,8]
X_4_train<-X_4_train %>% select(1, 2,5,6,8)
X_4_test<-X_4_test %>% select(1, 2,5,6,8)

#########################subset#######################

fit <- regsubsets(x=X, y=Y, method = "exhaustive", nvmax = 7)
my_summary <- summary(fit)
select <- my_summary$outmat

names(my_summary)
my_summary$cp
my_summary$bic
plot(my_summary$cp, type = "o", lty = 2, col = "blue", xlab = "number of variables", ylab = "CP", main = "CP by the number of variables")
plot(my_summary$bic, type = "o", lty = 2, col = "red", xlab = "number of variables", ylab = "BIC", main = "BIC by the number of variables")


######################boosting###########################
boost.train = train
boost.train$classdigit <- as.numeric(boost.train$classdigit)-1
#boost.train$classdigit <- as.factor(boost.train$classdigit)

boost.test = test
boost.test$classdigit <- as.numeric(boost.test$classdigit)-1

boost.fit <- gbm(classdigit ~., data=boost.train, distribution="adaboost", 
          n.trees=1000, shrinkage = .1, interaction.depth =3 )
boost.fit2 <- gbm(classdigit ~., data=boost.train, distribution="adaboost", 
                 n.trees=1000, shrinkage = .6, interaction.depth =3 )

summary(boost.fit)



Y_hat<-predict(boost.fit,newdata=test,n.trees=1000, type="response")
Y_hat <- Y_hat>0.5
Y_hat <- as.numeric(Y_hat)
Y_result<-c(test[8])
Y_result<-as.numeric(as.character(unlist(Y_result)))
misclass_boost <- sum(abs(Y_hat - Y_result))/length(Y_result)
accuracy_boost <- 1-misclass_boost
######################boosting with 4 variables###########################
boost.X_4_train = X_4_train
boost.X_4_train$classdigit <- as.numeric(boost.X_4_train$classdigit)-1
#boost.train$classdigit <- as.factor(boost.train$classdigit)

X_4_test = X_4_test
X_4_test$classdigit <- as.numeric(X_4_test$classdigit)-1

boost_x4.fit <- gbm(classdigit ~., data=boost.X_4_train, distribution="adaboost", 
                 n.trees=1000, shrinkage = .1, interaction.depth =3 )
boost_x4.fit2 <- gbm(classdigit ~., data=boost.X_4_train, distribution="adaboost", 
                  n.trees=1000, shrinkage = .6, interaction.depth =3 )

summary(boost_x4.fit)



Y_hat_4<-predict(boost_x4.fit2,newdata=X_4_test[1:4],n.trees=1000, type="response")
Y_hat_4 <- Y_hat_4>0.5
Y_hat_4 <- as.numeric(Y_hat_4)
Y_result<-c(test[8])
Y_result<-as.numeric(as.character(unlist(Y_result)))
misclass_boost_4 <- sum(abs(Y_hat_4 - Y_result))/length(Y_result)
accuracy_boost_4 <- 1-misclass_boost_4

Y_hat_4<-predict(boost_x4.fit,newdata=X_4_test[1:4],n.trees=1000, type="response")
Y_hat_4 <- Y_hat_4>0.5
Y_hat_4 <- as.numeric(Y_hat_4)
Y_result<-c(test[8])
Y_result<-as.numeric(as.character(unlist(Y_result)))
misclass_boost_4 <- sum(abs(Y_hat_4 - Y_result))/length(Y_result)
accuracy_boost_4 <- 1-misclass_boost
##########################boosting (srk)##################################
shrink <- c(.1, .4, .6, .8)
max_iter <- 1000
store_error_boost <- c()

for ( i in 1:length(shrink)){
  boost.fit <- gbm(classdigit ~., data=boost.train, distribution="adaboost", 
                   n.trees=1000, shrinkage = i, interaction.depth =3 )
  temp<-c()
  for(j in 1:max_iter){
    
    Y_hat<-predict(boost.fit,newdata=test,n.trees=j, type="response")
    Y_hat <- Y_hat>0.5
    Y_hat <- as.numeric(Y_hat)
    Y_result<-c(test[8])
    Y_result<-as.numeric(as.character(unlist(Y_result)))
    misclass_boost <- sum(abs(Y_hat - Y_result))/length(Y_result)
    print(misclass_boost)
    accuracy_boost <- 1-misclass_boost
    temp <- c(temp, misclass_boost)
    
  }
  store_error_boost <-cbind(store_error_boost,temp)
  
}
colnames(store_error_boost) <- paste("shrinkage", shrink,sep=":")
x11()
plot()
plot(store_error_boost[,1], main="Error Profiles", ylab="error",xlab="boosting iterations",ylim=c(0.1,0.5))
lines(store_error_boost[,2], col = "red")
lines(store_error_boost[,3], col = "blue")
lines(store_error_boost[,4], col = "green")

##########################################Tree#########################################


model.control <- rpart.control(minsplit = 4, xval = 10, cp = 0)
tree_pima<- rpart(classdigit~., data = train, method = "class", control = model.control)
x11()
plot(tree_pima$cptable[,4], main = "Cp for model selection", ylab = "Cp")

min_cp = which.min(tree_pima$cptable[,4])

x11()
plot(tree_pima, branch = .3, compress=T, main = "Full Tree")
text(tree_pima, cex = .5)

pruned_tree <- prune(tree_pima, cp = tree_pima$cptable[min_cp,1])

x11()
plot(pruned_tree, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_tree, cex = .5)


pred_full <- predict(tree_pima, newdata = test,type="class")
pred_pruned <- predict(pruned_tree, newdata = test,type="class")
pred_train_df = as.data.frame(t(pred_train))
pred_train_df_full = as.data.frame(t(pred_train_full))


accuracy_full = sum(pred_full == Y_result)/length(Y_result)

accuracy_pruned = sum(pred_pruned == Y_result)/length(Y_result)



misclass_tree_full <- sum(abs(Y_hat_full - Y_result))/length(Y_result)

#######################bagging#############
bagg.fit <- randomForest(classdigit~., data = train, n.tree=10000, importance=TRUE,mtry=7)

x11()
varImpPlot(bagg.fit)
importance(bagg.fit)

y_hat_bagg <- predict(bagg.fit, newdata = test, type = "class")
Bagg_test_accuracy=sum(y_hat_bagg == Y_result)/length(Y_result)
Bagg_test_accuracy
#######################bagging4#############
bagg.fit <- randomForest(classdigit~., data = X_4_train, n.tree=10000, importance=TRUE,mtry=4)

x11()
varImpPlot(bagg.fit)
importance(bagg.fit)

y_hat_bagg <- predict(bagg.fit, newdata = X_4_test[1:4], type = "class")
Bagg_test_accuracy_4=sum(y_hat_bagg == Y_result)/length(Y_result)
Bagg_test_accuracy_4


#######################RF#############
rf.fit <- randomForest(classdigit~., data = train, n.tree=10000, importance=TRUE)

library(randomForest)
x11()
varImpPlot(rf.fit)

importance(rf.fit)


y_hat_RF <- predict(rf.fit, newdata = test, type = "class")
RF_test_accuracy=sum(y_hat_RF == Y_result)/length(Y_result)
RF_test_accuracy
#######################RF4#################################
x4_rf_train<-train %>% select( 2,5,6,7,8)
x4_rf_test<-test %>% select( 2,5,6,7,8)

rf4.fit <- randomForest(classdigit~., data = x4_rf_train, n.tree=10000, importance=TRUE)

library(randomForest)
x11()
varImpPlot(rf4.fit)

importance(rf4.fit)


y_hat_RF4 <- predict(rf4.fit, newdata = x4_rf_test, type = "class")
RF_test_accuracy4=sum(y_hat_RF4 == Y_result)/length(Y_result)
RF_test_accuracy4
###############################KNN################################
library(class) 
library(dplyr)
knn_error <- c()

for (i in 1:15){
  knn.fit <- knn(train = train, test = test, cl = train$classdigit, k = i)
  error = 1- mean(knn.fit == test$classdigit)
  knn_error = c(knn_error,error)
}
which.min(knn_error)
acc_knn_11<-1-knn_error[which.min(knn_error)]

###############################KNN_4################################

knn_error_4 <- c()

for (i in 1:15){
  knn.fit <- knn(train = X_4_train, test = X_4_test, cl = X_4_train$classdigit, k = i)
  error = 1- mean(knn.fit == X_4_test$classdigit)
  knn_error_4 = c(knn_error_4,error)
}
which.min(knn_error_4)
acc_knn4_11<-1-knn_error_4[which.min(knn_error_4)]


################################pdp#########################################


library(pdp)
plotPartial(partial(rf.fit, pred.var=c("glucose","bmi"), pred.train=train),
            main='Random Forest')
plotPartial(partial(rf.fit, pred.var=c("bmi"), train=train),
            main='Random Forest')
plotPartial(partial(rf.fit, pred.var=c("age"), train=train),
            main='Random Forest')
plotPartial(partial(rf.fit, pred.var=c("pedigree"), train=train),
            main='Random Forest')

plotPartial(partial(fitGBM, pred.var='x1', train=train,
                    n.trees=1000), main='GBM')
plotPartial(partial(fitNN10, pred.var='x1', train=train),
            main='NN with 10 hidden nodes')
plotPartial(partial(fitNN160, pred.var='x1', train=dat),
            main='NN with 160 hidden nodes')


partialPlot(partial(rf.fit, pred.var=c("glucose","bmi"), pred.data = train) , x.var = "glucose")
partialPlot(rf.fit, pred.data = train , x.var = "age")
partialPlot(rf.fit, pred.data = train , x.var = "bmi")
partialPlot(rf.fit, pred.data = train , x.var = "pedigree")

partialPlot(rf.fit, pred.data = train , x.var = "glucose")

partialPlot(boston_rf, pred.data = boston, x.var = "lstat")
p1 <- partial(rf.fit, pred.var = "lstat", plot = TRUE, rug = TRUE)


rf.fit %>%
  partial(pred.var = "glucose") %>%
  plotPartial(rug = TRUE, train = train)

rf.fit %>%
  partial(pred.var = c("glucose", "age"), chull = TRUE, progress = "text") %>%
  plotPartial(contour = TRUE, legend.title = "legend")



rf.fit %>%
  partial(pred.var = c("glucose", "age"), chull = TRUE, progress = "text") %>%
  plotPartial(contour = TRUE, legend.title = "legend")
