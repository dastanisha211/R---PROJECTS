#-----------------------------------------Time Series Forecasting---------------------------------------#



#Analytical Problem: To forecast the Sales (in thoushand units) of a Automobile company for the next 36 months

#----------------------------------Installing the required packages--------------------------------------------#

list.of.packages <- c("forecast", "ggplot2","MASS","caTools","sqldf","tseries")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(forecast)
library(tseries)
kpss.test


#-----------------------------Setting the working directory-------------------------------------------#

Path<-"C:/Users/TANIA DAS/OneDrive/Desktop/Rivy"
setwd(Path)
getwd()

data<-read.csv("Sales.csv",header = TRUE)
data1=data#To create a backup of original data


#---------------------------------Exploring the data-------------------------------------------------------#

head(data1)# Displaying the first 6 rows of the data
dim(data1)#We have 144 time series points (at a date level) and 2 variables(Date, Sales)
str(data1)#Displaying the structure of the data for the two variables.Date variable is in character and sales is a integer variable.
summary(data1)#Displaying the summary of the variable
colSums(is.na(data1))# Checking whether there is missing values or not
names(data1)[c(1:2)]=c("Date","Sales")#Changing the variable names
class(data1)#This will give the class of the sales data.As of for now the class of the data will be a dataframe but in order to make arima 
work we need to convert it into timeseries data#


#---------------------Transformation of the date data into time series------------------------------------#

data1=ts(data1[,2],start=c(2003,1),frequency=12)
start(data1)
end(data1)
frequency(data1)
str(data1)
data1

#2003,1 is the start date and 12 is the frequency of the time series (monthly series)



#--------------------->plotting the sales 
plot(data1,ylab="Sales", xlab="Year",main="Sales between 2003-2014",col="red")
abline(reg = lm(data1~time(data1)))
cycle(data1)
plot(aggregate(data1,FUN=mean))
#This plot displays the year on year trend in the sales from 2003 

#Data has both a trend and drift, i.e. time invariant mean and variance 



#--------------->Differencing the data to remove trend and drift


plot(log10(data1),ylab="log(Sales)",xlab="Year",main="log(Sales) between 2003-2014",col="red")
##Differencing the data to remove trend
plot(diff(data1,differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Sales) between 2003-2014",col="red")
#The differenced data continues to have unequal variance 


#--------------->Differencing+Log transformation the data to remove trend and unequal variance


plot(diff(log10(data1),differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales)) between 2003-2014",col="red")

#So, with Log10 and 1 order of differencing makes the series stationary

#----------------->Checking the stationarity of transformed data using the Augmented Dickey-Fuller Test(ADF)

LDTSdata=diff(log10(data1),differences = 1)
require(forecast)
adf.test(LDTSdata,alternative="stationary")

#Since, the p-value <0.05, hence, we reject the Ho: Series is Non-Stationary 

kpss.test(LDTSdata)
#creating the ACF and PACF plot
par(mfrow=c(1,2))
acf(diff(log10(data1)),main="ACF plot")#ACF PLOT -- Moving Average or q
pacf(diff(log10(data1)),main="PACF plot")#PACF PLOT -- Auto Regressive or p
#Running the ARIMA model
ARIMAFit=arima((log10(data1)),c(0,1,1))# p=0,d=1,q=1
ARIMAFit1=arima((log10(data1)),c(1,1,1))#p=1,d=1,q=1
ARIMAFit2=arima((log10(data1)),c(0,1,0))#p=0,d=1,q=0
ARIMAFit3=arima((log10(data1)),c(1,1,0))#p=1,d=1,q=0
summary(ARIMAFit)
summary(ARIMAFit1)
summary(ARIMAFit2)
summary(ARIMAFit3)

#Running the ARIMA model-R, gives the best model fit 
require(forecast)
AutoARIMAFit1=auto.arima(log10(data1),approximation=TRUE,trace=TRUE)
summary(AutoARIMAFit1)
AutoARIMAFit1$residuals
#Predicting the future values
pred=predict(AutoARIMAFit1,n.ahead=36)
pred

#n.ahead is the no. of time series, we want to predict

##########################################
##Ploting the observed data and forecasted data together
par(mfrow=c(1,1))
plot(data1,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab="Year",ylab="Sales")
lines(10^(pred$pred),col="red")

###############################################
#plotting the +-2 standard error to range of expected error
plot(data1,type="l",xlim=c(2004,2018),ylim=c(1,1600),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue")
lines(10^(pred$pred-2*pred$se),col="violet")


## then do the exponential since you had used log earlier.
normal_result=10^pred$pred ## you get the desired result.
normal_result_df<-as.data.frame(normal_result)
Date_pred_seq<-as.data.frame(seq(as.Date("2015/01/01"),as.Date("2017/12/01"),by="month"))
final_result_df<-cbind(Date_pred_seq,normal_result_df)
colnames(final_result_df)<-c("Date","predicted_Sales")
write.csv(final_result_df,"finalpredict_ARIMA.csv", row.names = FALSE)
plot(normal_result)

#-------------------------------End of the model-----------------------------------#


