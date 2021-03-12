#######################################
#
#
#Woosung Lee
#
#
#
#
###########################################
#Q no 1. a
#a) 
###########################################


rm(list = ls())
#Read data
cereal = read.csv("./cereal_conv.csv")
#remove categorical data
cereal <- cereal[,-c(0:1)]
#extract same random numbers and separate the data into the train set (80%) and the test set (20%)
set.seed(1)
indi = sample(1:length(cereal[,1]),4/5*length(cereal[,1]))
train = cereal[indi,]
test = cereal[-indi,]
#Fitting Linear model with the data 
model = lm(rating ~   ., data = train)
summary(model)

# Finding MSE of the Linear model. 
tr_pred = predict(model, newdata=test)
tr_pred-test$rating
MSE = mean((test$rating-tr_pred)^2)
print(MSE)


########################################
#Q no 1. b
################################
library(leaps)
####################FORWARD###########################################################
regfit.forward <- regsubsets(rating~., data = train,nvmax = 14,method = "forward")
my_forward <- summary(regfit.forward)
x11()
par(mfrow = c(2,2))
plot(my_forward$rss, xlab = "Number of Variables", ylab = "RSS",type ="l")
plot(my_forward$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2 ",type ="l")
plot(my_forward$cp, xlab = "Number of Variables", ylab = "cp",type ="l")
plot(my_forward$bic, xlab = "Number of Variables", ylab = "BIC",type ="l")



########################################
#Q no 1. c
#################exhaustive###############

regfit.full <- regsubsets(rating~., data = train,nvmax = 14,method = "exhaustive")
my_sum <- summary(regfit.full)
my_sum
x11()
par(mfrow = c(2,2))
plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS",type ="l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2 ",type ="l")
plot(my_sum$cp, xlab = "Number of Variables", ylab = "cp",type ="l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC",type ="l")
###########################BACK#################################################


test.mat = model.matrix(rating ~., data=test)
val.errors = rep(NA,14)

for (i in 1:14){
  coefi = coef(regfit.forward, id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean((test$rating-pred)^2)
}
which.min(val.errors)
min(val.errors)
val.errors
x11()
plot(val.errors,type ="l")
##################################################################

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
summary(regfit.forward)$outmat[11,]#best 11


