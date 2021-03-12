rm(list = ls())


load("~/Data/vehicle.RData")


train_ind = sample(1:nrow(vehicle), nrow(vehicle)*0.70)

train = vehicle[train_ind,0:19]
test = vehicle[-train_ind,0:19]


model.control <- rpart.control(minsplit = 4, xval = 10, cp = 0)
fit.vehicle <- rpart(classdigit~., data = train, method = "class", control = model.control)
x11()
plot(fit.vehicle$cptable[,4], main = "Cp for model selection", ylab = "Cp")

min_cp = which.min(fit.vehicle$cptable[,4])
pruned_fit.vehicle <- prune(fit.vehicle, cp = fit.vehicle$cptable[min_cp,1])

x11()
plot(pruned_fit.vehicle, branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit.vehicle, cex = .5)

x11()
plot(fit.vehicle, branch = .3, compress=T, main = "Full Tree")
text(fit.vehicle, cex = .5)

pred_train_full <- predict(fit.vehicle, newdata = test)
pred_train <- predict(pruned_fit.vehicle, newdata = test)
pred_train_df = as.data.frame(t(pred_train))
pred_train_df_full = as.data.frame(t(pred_train_full))
write.csv(test,"./Data/Q_1_test.csv", row.names = FALSE)
write.csv(pred_train,"./Data/Q_1_pred_result.csv", row.names = FALSE)
write.csv(pred_train_df_full,"./Data/Q_1_pred_result_full.csv", row.names = FALSE)
