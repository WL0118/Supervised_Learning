rm(list = ls())
set.seed(44)
wine = read.csv("./Data/conv_wine.csv")
library(rpart)
library(rpart.plot)

train_ind = sample(1:nrow(wine), nrow(wine)*0.70)

train = wine[train_ind,]
test = wine[-train_ind,]



model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0)
fit.wine <- rpart(Y~., data = train, method = "class", control = model.control)

x11()
plot(fit.wine, uniform = T, compress = T)
text(fit.wine, cex = 1)

x11()
plot(fit.wine, uniform = T, compress = T)
text(fit.wine, use.n = T, all = T, cex = 1)

x11()
plot(fit.wine, branch = .4, uniform = T, compress = T)
text(fit.wine, use.n = T, all = T, cex = 1)

##############################################################################

model.control <- rpart.control(minsplit = 10, xval = 10, cp = 0)
fit.wine_2 <- rpart(Y~., data = train, method = "class", control = model.control)
x11()
plot(fit.wine_2$cptable[,4], main = "Cp for model selection", ylab = "Cp")

min_cp = which.min(fit.wine_2$cptable[,4])
pruned_fit_wine <- prune(fit.wine_2, cp = fit.wine_2$cptable[min_cp,1])

x11()
plot(pruned_fit_wine, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_wine, cex = .5)

x11()
plot(fit.wine_2, branch = .3, compress=T, main = "Full Tree")
text(fit.wine_2, cex = .5)

pred_train_pruned <- predict(pruned_fit_wine, newdata = test)

pred_train_pruned

pred_train_full <-predict(fit.wine_2, newdata = test)

##################################################
#Random Forest
###########################################3
rf.fit <- randomForest(Y~., data = train, n.tree=10000)

library(randomForest)
x11()
varImpPlot(rf.fit)

importance(rf.fit)


y_hat <- predict(rf.fit, newdata = test, type = "response")
sum(round(y_hat)-test$Y)/length(y_hat)
round(y_hat)
df_y=data.frame(y_hat,test$Y)

write.csv(df_y,"./Data/Q_3_RF_result.csv", row.names = FALSE)

write.csv(train,"./Data/Q_3_train.csv", row.names = FALSE)

write.csv(test,"./Data/Q_3_test.csv", row.names = FALSE)
write.csv(pred_train_pruned,"./Data/Q_3_pred_result.csv", row.names = FALSE)
write.csv(pred_train_full,"./Data/Q_3_pred_result_full.csv", row.names = FALSE)




regfit.forward <- regsubsets(Y~., data = train,nvmax = 10,method = "forward")

my_forward <- summary(regfit.forward)
plot(my_forward$cp, xlab = "Number of Variables", ylab = "cp",type ="l")
plot(my_forward$bic, xlab = "Number of Variables", ylab = "BIC",type ="l")
which.min(my_forward$cp)
which.min(my_forward$bic)

which(my_forward$outmat[7,]=="*")


