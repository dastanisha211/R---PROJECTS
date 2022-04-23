library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)

#-------------------------Setting the working directory and Reading the dataset--------------------------------------------#

Path<-"C:/Users/TANIA DAS/OneDrive/Desktop/Rivy"
setwd(Path)
getwd()

letters=read.csv("letters_ABPR.csv",stringsAsFactors = TRUE)

#-----------------------Basic Exploration of the Data Set----------------------------------------------------------#
head(letters)
colnames(letters)
dim(letters)
str(letters)
summary(letters)
sum<-summary(letters)
write.csv(sum,"Summaryletter.csv")
colSums(is.na(letters))
letters$letter<-as.factor(letters$letter)
str(letters)

#------------------------------Splitting the dataset into train and test data-----------------------#

set.seed(1000)
spl<-sample.split(letters$letter,0.7)
train<-subset(letters,spl==TRUE)
dim(train)

test<-subset(letters,spl==FALSE)
dim(test)


#-------------------------------------------Building the CART model----------------------------------------------#
CART1<-rpart(letter~.,data=train, method = "class")
prp(CART1)
CART1


#-------------------------Checking the accuracy of the model in the train data------------------------------#
predictCART1train<-predict(CART1, newdata=train, type = "class")#To predict the probabilities for the observations in the train data set
table(train$letter,predictCART1train)

confusionMatrix(predictCART1train,train$letter)


#-------------------------Checking the accuracy of the model in the test data------------------------------#
predictCART1<-predict(CART1, newdata=test, type = "class")#To predict the probabilities for the observations in the test data set
table(test$letter,predictCART1)

confusionMatrix(predictCART1,test$letter)


#---------------------------End of the CART model---------------------------------------------------------#



#------------------------------------A Random Forest Model-------------------------------------------------#
PredictForest1<-randomForest(letter~.,data = train)
PredictForest1


#--------------------Checking the accuracy of the model-------------------------------------------#
predForest1<-predict(PredictForest1, newdata=test, type = "class")
table(test$letter,predForest1)

#ConfusionMatrix
confusionMatrix(predForest1,test$letter)

#---------------------------Variable Importance chart in Random Forest---------------------------------------#
vu = varUsed(PredictForest1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(PredictForest1$forest$xlevels[vusorted$ix]), main = "Variable Importance Chart_Splitting")

#--------------------Measuring Impurity in the Random Forest Model-----------------------------------------#

#A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. 
#In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
#Therefore, one way to measure the importance of a variable is to average the reduction in impurity, 
#taken over all the times that variable is selected for splitting in all of the trees in the forest. 

varImpPlot(PredictForest1, main = "Variable Importance Chart_Impurity Red")
var<-varImp(PredictForest1)


