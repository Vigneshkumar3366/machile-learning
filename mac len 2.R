rm(list = ls(all=TRUE))

getwd()
cars_data = read.csv(file = "toyota-SimpleReg.csv", header = TRUE)
summary(cars_data)
cars_data
cars_data =cars_data[,-c(1,2)]
str(cars_data)
summary(cars_data)
cov(cars_data)
cov(cars_data$Age_06_15,cars_data$Price)
plot(cars_data$Age_06_15,cars_data$Price)
plot(cars_data$Age_06_15,cars_data$Price,xlab = "age",ylab = "price",pch=12,col = "blue")
cor(cars_data)
cor(cars_data$Age_06_15,cars_data$Price)
#70:30
rows = seq(1,nrow(cars_data),1)
set.seed(123)
trainRows = sample(rows,(70*nrow(cars_data))/100)
cars_train= cars_data[trainRows,]
cars_test = cars_data[-trainRows,]

trainRows1 = sample(rows,(80*nrow(cars_data))/100)
cars_train1= cars_data[trainRows1,]
cars_test1 = cars_data[-trainRows1,]

trainRows2 = sample(rows,(90*nrow(cars_data))/100)
cars_train2= cars_data[trainRows2,]
cars_test2 = cars_data[-trainRows2,]

linreg = lm(Price ~ Age_06_15, data = cars_train)
coefficients(linreg)


linreg1 = lm(Price ~ Age_06_15, data = cars_train1)
coefficients(linreg1)


linreg2 = lm(Price ~ Age_06_15, data = cars_train2)
coefficients(linreg2)

plot(linreg$residuals)
summary(linreg)
#7
par(mfrow=c(2,2))
plot(linreg)
par(mflow=c(1,1))
#8
test_pre =predict(linreg,cars_test)
test_pre
plot(test_pre)
test_actual = cars_test$Price
plot(test_actual)
##9
library(DMwR)
regr.eval(cars_test$Price,linreg$fitted.values)
plot(regr.eval(cars_test$Price,linreg$fitted.values))
regr.eval(test_actual,test_pre)
#10
conf_pred =data.frame(predict(linreg,cars_test,interval = "confidence",level = 0.95))
pred_pred =data.frame(predict(linreg,cars_test,interval = "confidence",level = 0.95))
str(conf_pred)
plot(conf_pred)
summary(conf_pred)
#
plot(cars_test$Age_06_15,cars_test$Price,xlab = "age",ylab = "price")
points(cars_test$Age_06_15,conf_pred$fit,type = "l",col="green",lwd=2)
