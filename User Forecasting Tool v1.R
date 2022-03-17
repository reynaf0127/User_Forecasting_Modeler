#Predict DAU by N/C/R retention rate and ARPU by year, month and weekday
#Import data
data<-read.csv('C:/Users/sfeng/Documents/Analysis/User Analysis/user analysis.csv')
data$month<-as.factor(data$month)
data$weekday<-as.factor(data$weekday)
data<-data[,-1]
str(data)
summary(data)


#Check assumption
require("dplyr")
require("car")
#Normality
qqnorm(data$new_user)
qqnorm(log(data$new_user))
shapiro.test(data$new_user)
fit <- lm(new_user ~ year_num, data=data)
library("ggplot2")
ggplot(fit, aes(year_num, new_user)) + 
  geom_point() + 
  geom_smooth(method=lm)
#Linearity
plot(fit,1)
#Homoscedasticity
plot(fit,3)
#Normality
plot(fit,2)
#Outliers
plot(fit,5)
plot(fit,4)


#Regression model
#Create dummy
library("caret")
library("lattice")

#Separate train and test data
set.seed(127)
assignment <- sample(0:1, size= nrow(data), prob = c(0.75,0.25), replace = TRUE)
train <-data[assignment == 0, ]
test <- data[assignment == 1, ]
t.test(train$new_user,test$new_user)

#Linear model
require("MASS")
require("rpart")
require("mgcv")
#LM model
model1 <- lm(new_user ~ year_num+month+weekday, data = train)
summary(model1)
#Stepwise method to select the variables
model3 <- stepAIC(model1, trace = 0)
summary(model3)
#VIF to check multicollinearity among variables
library("car")
vif(model3)
-----------------------------------------
model2 <- lm(current_user ~ log(year_num)+month+weekday, data = train)
summary(model2)
#Stepwise method to select the variables
model4 <- stepAIC(model2, trace = 0)
summary(model4)
#VIF to check multicollinearity among variables
library("car")
vif(model4)

#Model fit
#mse, standard error and r square
d3=model4$fitted-train$dau
#standard error
print(sqrt(mean(d3^2)))
#r square
print(1-(sum((d3)^2)/sum((train$dau-mean(train$dau))^2)))
shapiro.test(model4$residuals)
qqnorm(model4$residuals)
t.test(model4$residuals,mu=0)

#Cross validation
predict(model4, newdata = test, type = "response") -> test_prob
plot(test_prob,type="l",col="red")
lines(test$dau,col="blue")
legend("bottomleft",legend=c("Pred", "Act"),
       col=c("red", "blue"),lty=1:4, cex=1)
t.test(test_prob,test$dau)
wilcox.test(test_prob,test$dau)
mse <- sqrt(mean((test_prob-test$dau)^2,na.rm=T))
mse

#Get result
library("xlsx")
write.xlsx(model4$coefficients,"C:/Users/sfeng/Documents/Analysis/User Analysis/pred_dau_log.xlsx")


