#Loading the library and the dataset

library(tidyverse)
data=read.csv('HR_Analytics.csv')

#Explore the dataset
head(data)
glimpse(data)
length(data)
summary(data)

#Missing value
colSums(is.na(data))

#There are no missing values

#Density plots for data most likely to be normal

ggplot(data=data,aes(Age))+geom_density()
#right skewed

ggplot(data=data,aes(DistanceFromHome))+geom_density()
#right skewed

ggplot(data=data,aes(TotalWorkingYears))+geom_density()
#right skewed


#Simple linear Regression


#Trying to predict Hourly Rate using Age (investigation 1)
names(data)
data_new=data[c(1,13)]
glimpse(data_new)

#Splitting the data into training set and test set

#install.packages('caTools')
library(caTools)

#Random state set to any value to get same result 
set.seed(50)

#Splitting the data
#Split depends on dependent variable 
split=sample.split(data$HourlyRate, SplitRatio = 1/5)
train=subset(data,split=TRUE)
test=subset(data,split=FALSE)

#Fitting the simple linear regressor on Training set (investigation 1)
regressor=lm(formula = HourlyRate~Age,train)
summary(regressor)

# The formula is 63.896 + .0541*Age 
# The R-squared value is .06%, this is not a good prediction model

#Prediction model (investigation 1)
new=data.frame(Age=45)
paste("If an employee is 45, the model predicts their hourly rate as:",predict(regressor,newdata = new))

#Mean square error (investigation 1)

summ=summary(regressor)

paste("The MSE for investigation 1 is:", mean(summ$residuals^2))

#Visualization of the plot (investigation 1)

y_pred=predict(regressor,newdata = test)

ggplot()+geom_point(aes(x=test$Age,y=test$HourlyRate),color='red')+
  geom_line(aes(x=test$Age,y=y_pred),color='blue')+
  xlab('Age')+ylab('HourlyRate')

#Trying to predict Hourly Rate using TotalWorkingYears (investigation 2)
names(data)
data_new2=data[c(13,29)]
glimpse(data_new2)

#Fitting the simple linear regressor on Training set (investigation 2)
regressor2=lm(formula = HourlyRate~TotalWorkingYears,train)
summary(regressor2)

# The formula is 65.9599 - .0061 * TotalWorkingYears
# The R-squared value is 5.446e-06, the model is not good

#Prediction model (investigation 2)
new2=data.frame(TotalWorkingYears=9)
paste("If an employee has 9 years in their career, the model predicts their hourly rate as:",predict(regressor2,newdata = new2))

#Mean square error (investigation 2)

summ2=summary(regressor2)

paste("The MSE for investigation 2 is:", mean(summ2$residuals^2))

#Visualization of the plot (investigation 2)

y_pred2=predict(regressor2,newdata = test)

ggplot()+geom_point(aes(x=test$TotalWorkingYears,y=test$HourlyRate),color='red')+
  geom_line(aes(x=test$TotalWorkingYears,y=y_pred2),color='blue')+
  xlab('TotalWorkingYears')+ylab('HourlyRate')

#Trying to predict Hourly Rate using Distance from Home (investigation 3)
names(data)
data_new3=data[c(13,6)]
glimpse(data_new3)

#Fitting the simple linear regressor on Training set (investigation 3)
regressor3=lm(formula = HourlyRate~DistanceFromHome,train)
summary(regressor3)

# The formula is 65.1735 + .078*DistanceFromHome
# The R-squared value is .097%, this is not a good model

#Prediction model (investigation 3)
new3=data.frame(DistanceFromHome=15)
paste("If an employee has a 15 mile commute, the model predicts their hourly rate as:",predict(regressor3,newdata = new3))

#Mean square error (investigation 3)

summ3=summary(regressor3)

paste("The MSE for investigation 3 is:", mean(summ3$residuals^2))

#Visualization of the plot ()

y_pred3=predict(regressor3,newdata = test)

ggplot()+geom_point(aes(x=test$DistanceFromHome,y=test$HourlyRate),color='red')+
  geom_line(aes(x=test$DistanceFromHome,y=y_pred3),color='blue')+
  xlab('DistanceFromHome')+ylab('HourlyRate')

# None of these models are good for predicting Hourly Rate

