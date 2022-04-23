#Calling the required library
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#Setting the woorking directory
setwd("C:/Users/TANIA DAS/Downloads")
getwd()

#Reding the csv file
data<-read.csv("elantra.csv")

#Exploratory Data Analysis
dim(data)
colnames(data)
str(data)
summary(data)
head(data)

#Checking the missing values
colSums(is.na(data))

There are no outliers and missing values in the entire dataset.

#Making seperate data frames for train and test data
train<-filter(data,Year==2010|Year==2011|Year==2012)
dim(train)

test<-filter(data,Year==2013|Year==2014)
dim(test)

###Testing the model on train data 

#Iteration1
linearm1<-lm(ElantraSales~Unemployment+Queries+CPI_energy+CPI_all,data=train)
summary(linearm1)

#Iteration2
linearm2<-lm(ElantraSales~Month+Unemployment+Queries+CPI_all+CPI_energy,data=train)
summary(linearm2)

#Creating a new column and storing Month as a factor variable
data$New_Month<-as.factor(data$Month)
class(data$New_Month)

New_Train<-filter(data,Year==2010|Year==2011|Year==2012)
New_Test<-filter(data,Year==2013|Year==2014)

#Iteration3
linearm3<-lm(ElantraSales~New_Month+Unemployment+Queries+CPI_energy+CPI_all,data=New_Train)
summary(linearm3)

#Getting the correlation between the continuous variables
cor(select(New_Train,c("Unemployment","Month","Queries","CPI_energy","CPI_all")))

#Iteration4 
linearm4<-lm(ElantraSales~New_Month+Unemployment+CPI_energy+CPI_all,data=New_Train) #Dropping "Queries" at 90% level of significance
summary(linearm4)

#Getting the predicted values
prediction<-predict(linearm4,newdata = New_Test)

#Calculating the largest error
error<-max(abs(New_Test$ElantraSales-prediction))

#Calculating sum of squared error
SSE<-sum((New_Test$ElantraSales-prediction)^2)

#Calculating total sum of squares
SST<-sum((mean(New_Train$ElantraSales)-New_Test$ElantraSales)^2)

#R-squared
R2<-1-(SSE/SST)
