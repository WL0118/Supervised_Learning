
iris<-data(iris)
write.csv(iris,"./iris.csv")

train_3 <-read.csv('Train_3.csv')
train_exhaustive_subset <- regsubsets(Y~., data = train_3,nvmax = 4,method = "exhaustive")
my_ex <- summary(train_exhaustive_subset)
my_ex
train_fwd_subset <- regsubsets(Y~., data = train_3,nvmax = 4,method = "forward")
my_fw <- summary(train_fwd_subset)
my_fw
train_bwd_subset <- regsubsets(Y~., data = train_3,nvmax = 4,method = "backward")
my_bw <- summary(train_bwd_subset)
my_bw


###################################################################


