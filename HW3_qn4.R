#perform cross-validation on a simulated data set
library(boot)
#Generate Dataset
rm(list= ls())
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

plot(x, y)

#############################################################################
#Compute the LOOCV errors that result from fitting the following four models
#using least squares
#############################################################################

#Model 1: Y = β0 + β1X + ε
mse_loocv <- c(0,0,0,0)
set.seed(1)
data <- data.frame(x, y)
glm.fit.model1 <- glm(y ~ x)
mse_loocv <- cv.glm(data, glm.fit.model1)
mse_loocv1 <- mse_loocv$delta[1]
mse_loocv1
summary(glm.fit.model1)

#Model 2: Y = β0 + β1X + β2X2 + ε
set.seed(1)
data <- data.frame(x, y)
glm.fit.model2 <- glm(y ~ poly(x,2))
mse_loocv <- cv.glm(data, glm.fit.model2,K = 2)
mse_loocv2 <- mse_loocv$delta[1]
mse_loocv2 
summary(glm.fit.model2)

#Model 3: Y = β0 + β1X + β2X2 + β3X3 + ε
set.seed(1)
data <- data.frame(x, y)
glm.fit.model3 <- glm(y ~ poly(x,3))
mse_loocv <- cv.glm(data, glm.fit.model3, K = 2)
mse_loocv3 <- mse_loocv$delta[1]
mse_loocv3
summary(glm.fit.model3)

#Model 4: Y = β0 + β1X + β2X2 + β3X3 + β4X4 + ε
set.seed(1)
data <- data.frame(x, y)
glm.fit.model4 <- glm(y ~ poly(x,4))
mse_loocv <- cv.glm(data, glm.fit.model4)
mse_loocv4 <- mse_loocv$delta[1]
mse_loocv4
summary(glm.fit.model4)





