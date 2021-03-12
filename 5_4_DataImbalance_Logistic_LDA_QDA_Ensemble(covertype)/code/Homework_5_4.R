rm(list = ls())
load(".Data/covertype.RData")


write.csv(covertype,"./Data/covertype_name.csv", row.names = FALSE)


train<-read.csv('./Data/Q_4_train_conv.csv')

#regfit.forward <- regsubsets(V55~., data = covertype,nvmax = 54,method = "exhusted")
regfit.forward <- regsubsets(V55~., data = train,nvmax = 54,method = "exhaustive")

my_forward <- summary(regfit.forward)
plot(my_forward$cp, xlab = "Number of Variables", ylab = "cp",type ="l")
plot(my_forward$bic, xlab = "Number of Variables", ylab = "BIC",type ="l")
which.min(my_forward$cp)
which.min(my_forward$bic)

which(my_forward$outmat[20,]=="*")
which(my_forward$outmat[10,]=="*")

test.mat = model.matrix(rating ~., data=test)
val.errors = rep(NA,14)

for (i in 1:14){
  coefi = coef(regfit.full, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((test$rating-pred)^2)
}
which.min(val.errors)
min(val.errors)
val.errors
x11()
plot(val.errors,type ="l")
#######################Examine the best variable models
summary(regfit.full)$outmat[9,]
summary(regfit.forward)$outmat[11,]