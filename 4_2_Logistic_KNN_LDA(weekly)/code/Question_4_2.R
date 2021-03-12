df_b <-read.csv('./data/df_B.csv')
library(ISLR)
WK<-data(Weekly)
attach(Weekly)
#################################B####################################
LR_6<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly,family=binomial)
summary(LR_6)

LR_7<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume+Today, data=Weekly,family=binomial)
summary(LR_7)
#################################C####################################


df_test_lag2 <-read.csv('./data/Test_Lag2.csv')
df_train_lag2 <-read.csv('./data/Train_Lag2.csv')

Lag2<-glm(Direction~Lag2, data=df_train_lag2,family=binomial)
summary(Lag2)

library(MASS)
fit.lda <- lda(Direction ~ Lag2, data = df_train_lag2)
summary(fit.lda)
fit.lda


knn.fit <- knn(train = as.matrix(df_train_lag2$Lag2), test = as.matrix(df_test_lag2$Lag2), cl = df_train_lag2$Direction, k = 1)
library(class)
#error[i] = 1- mean(knn.fit == iris.test$Species)


knn.fit
summary(knn.fit)

