#calling the necessary library

library(caret)# LOGISTIC MODEL
library(ggplot2)# VISUALIZATION
library(MASS)# VIF CALCULATION
library(car)# VIF CALCULATION
library(mlogit)# LOGISTIC MODEL
library(sqldf)#WOE & IV
library(Hmisc)#WOE & IV
library(aod)#WALD TEST
library(BaylorEdPsych)#R-SQUARE
library(DescTools)#R-SQUARE
library(ResourceSelection)#HOSMER LEMESHOW TEST
library(pROC)#ROC CURVE
library(ROCR)#ROC CURVE
library(caTools)#TRAIN AND TEST SPLIT

#--------------------------------Setting the Working Directory-----------------------------------------#
setwd("C:/Users/TANIA DAS/Downloads")

data<-read.csv("Data_for_Logistic_Regression.csv",header=TRUE)
data1=data#To create a backup of original data
head(data1)

#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(data1)
summary(data1)
dim(data1)
data1$Default_On_Payment<-as.factor(data1$Default_On_Payment)
cols_cat<-c("Status_Checking_Acc","Credit_History","Purposre_Credit_Taken","Savings_Acc","Years_At_Present_Employment","Inst_Rt_Income","Marital_Status_Gender","Current_Address_Yrs","Other_Debtors_Guarantors","Property","Other_Inst_Plans","Housing","Job","Telephone","Foreign_Worker")
data1[cols_cat]<-lapply(data1[cols_cat],factor)
str(data1)
table(data$Default_On_Payment)
summary(data1)
sum<-summary(data1)
write.csv(sum,"summary.csv")
colnames(data1)

#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
data.frame(colSums(is.na(data1)))
#There are no missing values in the dataset

#--------------------------------Information Value Calculation (A variable reduction technique)----------------------------------#
colnames(data1)
#Creating separate datasets for categorical and numeric values
## Data set with numeric variable
numeric <- data1[,-c(2,4:5,7:13,15:16,18,20:21)]#Numerical Data Frame
categorical <- data1[,c(2,4:5,7:13,15:16,18,20:21,22)]#Categorical Data Frame

#---------------------------------------IV for numeric data-------------------------------------------------------#
colnames(numeric)

IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

a1<-IVCal("Customer_ID","Default_On_Payment",numeric,groups=10)
a2<- IVCal("Duration_in_Months","Default_On_Payment",numeric,groups=10)
a3<- IVCal("Credit_Amount","Default_On_Payment",numeric,groups=10)
a4<- IVCal("Age","Default_On_Payment",numeric,groups=10)
a5<- IVCal("Num_CC","Default_On_Payment",numeric,groups=10)
a6<- IVCal("Dependents","Default_On_Payment",numeric,groups=10)
a7<- IVCal("Count","Default_On_Payment",numeric,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3,a4,a5,a6,a7))
IV_num

#-------------------------------------Information Value for categorical data----------------------------------------------------------#
colnames(categorical)

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
A1<- CA("Default_On_Payment","Status_Checking_Acc",categorical)
B1<- CA("Default_On_Payment","Credit_History",categorical)
C1<- CA("Default_On_Payment","Purposre_Credit_Taken",categorical)
D1<- CA("Default_On_Payment","Savings_Acc",categorical)
E1<- CA("Default_On_Payment","Years_At_Present_Employment",categorical)
F1<- CA("Default_On_Payment","Marital_Status_Gender",categorical)
G1<- CA("Default_On_Payment","Other_Debtors_Guarantors",categorical)
H1<- CA("Default_On_Payment","Property",categorical)
I1<- CA("Default_On_Payment","Other_Inst_Plans",categorical)
J1<- CA("Default_On_Payment","Housing",categorical)
K1<- CA("Default_On_Payment","Job",categorical)
L1<- CA("Default_On_Payment","Telephone",categorical)
M1<- CA("Default_On_Payment","Foreign_Worker",categorical)
N1<- CA("Default_On_Payment","Inst_Rt_Income",categorical)
O1<- CA("Default_On_Payment","Current_Address_Yrs",categorical)

IV_cat<- data.frame(rbind(A1,B1,C1,D1,E1,F1,G1,H1,I1,J1,K1,L1,M1,N1,O1))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

write.csv(Final_IV,"Final_IV.csv")

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(144)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data1$Default_On_Payment, 0.7)
data.train = subset(data1, spl == TRUE)
str(data.train)
dim(data.train)

data.test = subset(data1, spl == FALSE)
str(data.test)
dim(data.test)

#-------------------------------------Logistic Regression Model Building------------------------------------------#
model <- glm(Default_On_Payment~., data=data.train, family=binomial())
summary(model)


## Remove the insignificant variable
model1 <- glm(Default_On_Payment~Status_Checking_Acc + Duration_in_Months + Credit_History + Purposre_Credit_Taken +	Credit_Amount + Savings_Acc
              + Years_At_Present_Employment + Inst_Rt_Income + Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs + Property
              + Age + I(Other_Inst_Plans=="A143") + Housing + Num_CC +	Job + Dependents + Telephone
              + Foreign_Worker, data=data.train, family=binomial())
summary(model1)

## Remove the insignificant variable
model2 <- glm(Default_On_Payment~Status_Checking_Acc + Duration_in_Months + Credit_History + Purposre_Credit_Taken +	Credit_Amount + Savings_Acc
              + Years_At_Present_Employment + Inst_Rt_Income + Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs + I(Property=="A123")+I(Property=="A124")
              + Age + I(Other_Inst_Plans=="A143") + Housing + Num_CC +	Job + Dependents + Telephone
              + Foreign_Worker, data=data.train, family=binomial())
summary(model2)

## Remove the insignificant variable
model3 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + Credit_History + I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")+I(Purposre_Credit_Taken=="A46")+I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")++	Credit_Amount + Savings_Acc
              + Years_At_Present_Employment + Inst_Rt_Income + Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs + I(Property=="A123")+I(Property=="A124")
              + Age + I(Other_Inst_Plans=="A143") + Housing + Num_CC +	Job + Dependents + Telephone
              + Foreign_Worker, data=data.train, family=binomial())
summary(model3)

## Remove the insignificant variable
model4 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + I(Credit_History=="A32") + I(Credit_History=="A33") + I(Credit_History=="A34") + I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A44")+I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")++	Credit_Amount + Savings_Acc
              + Years_At_Present_Employment + Inst_Rt_Income + I(Marital_Status_Gender=="A93") + Other_Debtors_Guarantors + I(Property=="A123")+I(Property=="A124")
              + Age + I(Other_Inst_Plans=="A143") + Housing + Num_CC +	Job + Dependents + Telephone
              + Foreign_Worker, data=data.train, family=binomial())
summary(model4)


## Remove the insignificant variable
model5 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + I(Credit_History=="A32") + I(Credit_History=="A33") + I(Credit_History=="A34") + I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+I(Purposre_Credit_Taken=="A48")+ I(Purposre_Credit_Taken=="A49")++	Credit_Amount 
              + I(Savings_Acc=="A62")+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ 
                + I(Years_At_Present_Employment=="A74") + I(Inst_Rt_Income==3) +I(Inst_Rt_Income==4)+ I(Marital_Status_Gender=="A93") + Other_Debtors_Guarantors+I(Property=="A124")
              + Age + I(Other_Inst_Plans=="A143") + Housing + Num_CC + Dependents + Telephone
              + Foreign_Worker, data=data.train, family=binomial())
summary(model5)

vif(model5)

## Remove the insignificant variable based on vif value
model6 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + I(Credit_History=="A33") + I(Credit_History=="A34") + I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A49")++	Credit_Amount 
              + I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ 
                + I(Years_At_Present_Employment=="A74") + I(Inst_Rt_Income==3) +I(Inst_Rt_Income==4)+ + I(Marital_Status_Gender=="A93") + Other_Debtors_Guarantors+I(Property=="A124")
              + I(Other_Inst_Plans=="A143") + Housing + Num_CC + Telephone
              + Foreign_Worker, data=data.train, family=binomial())
summary(model6)

## Remove the insignificant variable
model7 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + I(Credit_History=="A33") + I(Credit_History=="A34") + I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A49")++	Credit_Amount 
              + I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ 
                + I(Years_At_Present_Employment=="A74") + I(Inst_Rt_Income==3) +I(Inst_Rt_Income==4)+ + I(Marital_Status_Gender=="A93") + I(Other_Debtors_Guarantors=="A103")+I(Property=="A124")
              + I(Other_Inst_Plans=="A143") + Housing + Num_CC
              + Foreign_Worker, data=data.train, family=binomial())
summary(model7)

vif(model7)

#------------------------------Validating the model on test data------------------------------#
modeltest <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + I(Credit_History=="A33") + I(Credit_History=="A34") + I(Purposre_Credit_Taken=="A41")+ I(Purposre_Credit_Taken=="A410")+ I(Purposre_Credit_Taken=="A42")+I(Purposre_Credit_Taken=="A43")+ I(Purposre_Credit_Taken=="A49")++	Credit_Amount 
                 + I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+ 
                   + I(Years_At_Present_Employment=="A74") + I(Inst_Rt_Income==3) +I(Inst_Rt_Income==4)+ + I(Marital_Status_Gender=="A93") + I(Other_Debtors_Guarantors=="A103")+I(Property=="A124")
                 + I(Other_Inst_Plans=="A143") + Housing + Num_CC
                 + Foreign_Worker, data=data.test, family=binomial())
summary(modeltest)

vif(modeltest)

#------------------------------Checking the overall fitness of the model----------------------------#

#--------------->using Wald Test
wald.test(b=coef(modeltest), Sigma= vcov(modeltest), Terms=1:26)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0

#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)

# Difference between null deviance and deviance
modelChi <- modeltest$null.deviance - modeltest$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modeltest$df.null - modeltest$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(modeltest)
#Expected good range of R2 is between 0.2-0.4

# Hosmer and Lemeshow given by the McFadden 6R square
R2.hl<-modelChi/model$null.deviance
R2.hl

# Cox and Snell R Square (the last number; 
R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(data.test))
R.cs

# Max rescaled R square (Nagelkarke) 
R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(data.test))))))
R.n


#--------------------Lackfit Deviance for assessing whether the model where
#Ho: Observed Frequencies/probabilities =Expected Frequencies/probabilities ----------------------------------------#
residuals(modeltest) # deviance residuals
residuals(modeltest, "pearson") # pearson residuals

sum(residuals(modeltest, type = "pearson")^2)
deviance(modeltest)

#########Larger p value indicate good model fit
1-pchisq(deviance(modeltest), df.residual(modeltest))
#Thus, we accept the Null Hypothesis Ho that Observed Frequencies = Expected Frequencies


################################################################################################################
# How to use the function. Data.train is the name of the data set. model is name of the glm output
## This is not a very useful test. Some authors have suggested that sometimes it produces wrong result##
## High p value indicates the model fits well

# Hosmer and Lemeshow test 
options (scipen = 999)
hl <- hoslem.test(as.integer(data.test$Default_On_Payment), fitted(modeltest), g=10)
hl

#####################################################################################################################
# Coefficients (Odds)
modeltest$coefficients
# Coefficients (Odds Ratio)
exp(modeltest$coefficients)#Interpret 

# Variable Importance of the model
varImp(modeltest)

# Predicted Probabilities
prediction <- predict(modeltest,newdata = data.test,type="response")
write.csv(prediction,"pred.csv")

rocCurve   <- roc(response = data.test$Default_On_Payment, predictor = factor(prediction, ordered =TRUE), 
                  levels = rev(levels(data.test$Default_On_Payment)))
data.test$Default_On_Payment <- as.factor(data.test$Default_On_Payment)

#Metrics - Fit Statistics

threshold<-as.numeric(coords(rocCurve,"best")[1])
predclass <-ifelse(prediction>threshold,1,0)

Confusion <- table(Predicted = predclass,Actual = data.test$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose = TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)

data.test$m1.yhat <- predict(modeltest, data.test, type = "response")

m1.scores <- prediction(data.test$m1.yhat, data.test$Default_On_Payment)
### KS statistics calculation
m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

