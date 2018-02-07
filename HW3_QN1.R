#################################################
#LOGISTIC REGRESSION
#################################################

rm(list = ls())
library(ISLR)
library(MASS)
#install.packages("caret");
library(caret)
#library(ModelMetrics)
attach(Boston)
#install.packages("heuristica")
library(heuristica)

names(Boston)

#########################################
# Create a training and test set
#########################################

set.seed(123)
len <- length(crim)
train_data <- 0
for(l in 1:(len - 1)){
  
  train_data <- c(train_data, 0); 
  
}
train_data[crim > median(crim)] <- 1
Boston <- data.frame(Boston, train_data)

names(Boston)
train <- sample(1:(length(crim)/2))
Boston_train <- Boston[train,]
Boston_test <- Boston[-train,]

y_true_train <- Boston_train$crim
y_true_test <- Boston_test$crim


########################################
#Finding correlation between values
########################################
#install.packages("corrplot")
library(corrplot)
corrplot::corrplot.mixed(cor(Boston[, -15]), upper="circle")


par(mfrow=c(2,3))
boxplot(Boston$zn ~ Boston$train_data, data = Boston, main = "zn vs crim>median(crim)")
boxplot(Boston$indus ~ Boston$train_data, data = Boston, main = "indus vs crim>median(crim)")
boxplot(Boston$chas ~ Boston$train_data, data = Boston, main = "chas vs crim>median(crim)")
boxplot(Boston$nox ~ Boston$train_data, data = Boston, main = "nox vs crim>median(crim)")
boxplot(Boston$rm ~ Boston$train_data, data = Boston, main = "rm vs crim>median(crim)")
boxplot(Boston$age ~ Boston$train_data, data = Boston, main = "age vs crim>median(crim)")
boxplot(Boston$dis ~ Boston$train_data, data = Boston, main = "dis vs crim>median(crim)")
boxplot(Boston$rad ~ Boston$train_data, data = Boston, main = "rad vs crim>median(crim)")
boxplot(Boston$tax ~ Boston$train_data, data = Boston, main = "tax vs crim>median(crim)")
boxplot(Boston$ptratio ~ Boston$train_data, data = Boston, main = "ptratio vs crim>median(crim)")
boxplot(Boston$black ~ Boston$train_data, data = Boston, main = "black vs crim>median(crim)")
boxplot(Boston$lstat ~ Boston$train_data, data = Boston, main = "lstat vs crim>median(crim)")
boxplot(Boston$medv ~ Boston$train_data, data = Boston, main = "black vs crim>median(crim)")

#########################################
# Logistic Regression
#########################################
glm.fit <- glm(train_data ~ . - train_data - crim, data = Boston, family = binomial, subset = train,maxit = 100)
summary(glm.fit)
names(glm.fit)


glm.fit.1 <- glm(train_data ~ . - train_data - crim - chas -zn +lstat +ptratio +tax, data = Boston, family = binomial, subset = train,maxit = 100)

# Predict
glm.probs.train <- predict(glm.fit, newdata = Boston_train, type = "response")
y_hat_train <- round(glm.probs.train)
table(y_hat_train,y_true_train)
glm.probs.test <- predict(glm.fit, newdata = Boston_test, type = "response")
y_hat_test <- round(glm.probs.test)
table(y_hat_test,y_hat_train)


#Predict
glm.probs.train <- predict(glm.fit.1, newdata = Boston_train, type = "response")
y_hat_train <- round(glm.probs.train)
table(y_hat_train,y_true_train)
glm.probs.test <- predict(glm.fit.1, newdata = Boston_test, type = "response")
y_hat_test <- round(glm.probs.test)
table(y_hat_test,y_hat_train)


#########################################
#  Calculate the error rates
########################################
train_err <- mean(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- mean(abs(y_hat_test- y_true_test))/length(y_true_test)

train_err
test_err
accuracy_train <- (1 - train_err)*100
accuracy_test <- (1 - test_err)*100

#error for glm.fit.1

train_err <- mean(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- mean(abs(y_hat_test- y_true_test))/length(y_true_test)

train_err
test_err
accuracy_train <- (1 - train_err)*100
accuracy_test <- (1 - test_err)*100




####################################
## Linear Discriminant Analysis (LDA)
##
####################################
lda.fit <- lda(train_data~.- train_data - crim, data = Boston_train)
lda.pred.train <- predict(lda.fit, newdata = Boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit, newdata = Boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1

#model 1 with different predictors
lda.fit1 <- lda(train_data~.- train_data - crim - chas -zn +lstat +ptratio +tax, data = Boston_train)
lda.pred.train <- predict(lda.fit1, newdata = Boston_train)
y_hat_train <- as.numeric(lda.pred.train$class)-1
lda.pred.test <- predict(lda.fit1, newdata = Boston_test)
y_hat_test <- as.numeric(lda.pred.test$class)-1

# Compute the error
lda_train_error <- mean(abs(y_true_train - y_hat_train))/length(y_true_train) 
lda_test_error <- mean(abs(y_true_test - y_hat_test))/length(y_true_test)  
lda_train_error
lda_test_error

accuracy_lda_test <- (1 - lda_test_error)*100
accuracy_lda_train <- (1 - lda_train_error)*100

#########################################
# KNN
#########################################
library("ggplot2")
library("ElemStatLearn")
#install.packages("class")
library("class")
#errr_test<- c(0,0,0,0,0,0,0,0)
#errr_train <- c(0,0,0,0,0,0,0,0)
errr_train <- 0
errr_test <- 0
for (i in c(1,3,5,7,9,11, 13,15,100)){
  #quartz()
  test_predict <- knn(Boston_train,Boston_test,y_true_test,i)
  train_predict <- knn(Boston_train,Boston_train,y_true_train, i)
  
  #converting to data frame
  test_predict_df <- as.data.frame(test_predict)
  train_predict_df <- as.data.frame(train_predict)
  errr_test <- c(errr_test,0)
  errr_train <- c(errr_train,0)
  errr_test[i] <- mean(y_true_test!=test_predict_df)/nrow(Boston_test)
  errr_train[i] <- mean(y_true_train!=train_predict_df)/nrow(Boston_train)
  
}
errr_test
errr_train

accuracy_knn_train <- c(0,0,0,0,0,0,0,0)
accuracy_knn_test <- c(0,0,0,0,0,0,0,0)
for(i in c(1,3,5,7,9,11,13,15,100))
{
  accuracy_knn_train[i] <- (1 - errr_train[i])*100
  accuracy_knn_test[i] <- (1 - errr_test[i])*100 
}

print("Accuracy of Train Data")
for(i in c(1,3,5,7,9,11, 13,15,100))
{
  print(accuracy_knn_train[i])
}
print("Accuracy of Test Data")
for(i in c(1,3,5,7,9,11, 13,15,100))
{
  print(accuracy_knn_test[i])
}


for(i in c(1,3,5,7,9,11, 13,15,100))
{
  print(errr_train[i])
}


for(i in c(1,3,5,7,9,11, 13,15,100))
{
  print(errr_test[i])
}



  