## Lead Scoring Project##

#Calling the necessary library#
library(caret)# LOGISTIC MODEL
library(ggplot2)# VISUALIZATION
library(MASS)# VIF CALCULATION
library(car)# VIF CALCULATION
library(mlogit)# LOGISTIC MODEL
library(sqldf)#WOE & IV
library(Hmisc)#WOE & IV
library(aod)#WALD TEST
library(ResourceSelection)#HOSMER LEMESHOW TEST
library(pROC)#ROC CURVE
library(ROCR)#ROC CURVE
library(caTools)#TRAIN AND TEST SPLIT
library(DescTools)#R-SQUARE

library(caret)
library(ggplot2)
library(rpart.plot)
library(pROC)
library(ROCR)
library(rpart)
library(randomForest)
library(caTools)
library(e1071)

#Setting the working directory and reading the data set#
setwd("C:/Users/TANIA DAS/OneDrive/Desktop/Rivy")
getwd()
data<-read.csv("Leads_data.csv")
lead<-data#Copy of the data set

#Basic Exploration of the data#
head(lead)
str(lead)
dim(lead)

#Dropping the unecessary columns#
lead<-lead[-c(1,2)]#Both the Prospect ID and Lead number are unique columns and hence we wont need for prediction#
dim(lead)
colnames(lead)

#Changing the column names#
colnames(lead)[13]<-"What_is_your_current_occupation"
colnames(lead)[14]<-"What_matters_most_to_you_in_choosing_a_course"
colnames(lead)[17]<-"Newspaper_Article"
colnames(lead)[18]<-"X_Education_Forums"
colnames(lead)[20]<-"Digital_Advertisement"
colnames(lead)[21]<-"Through_Recommendations"
colnames(lead)[22]<-"Receive_More_Updates_About_Our_Courses"
colnames(lead)[24]<-"Lead_Quality"
colnames(lead)[25]<-"Update_me_on_Supply_Chain_Content"
colnames(lead)[26]<-"Get_updates_on_DM_Content"
colnames(lead)[27]<-"Lead_Profile"
colnames(lead)[33]<-"I_agree_to_pay_the_amount_through_cheque"

#There are  columns which has blank cells.We will replace this field with NA#

lead[lead==""]<-NA

#There are quite a few columns which has 'Select' as one of the category. This is most probably because the person has not filled that field. 
#We will replace this field with NA#

lead[lead=="Select"]<-NA

#Checking the missing value#
colSums(is.na(lead))

lead$Converted<-as.factor(lead$Converted)
#Converting columns into factor#
convert<-c("Lead_Origin","Do_Not_Email","Do_Not_Call","Lead_Source","Last_Activity","Country","Specialization","How_did_you_hear_about_X_Education","What_is_your_current_occupation","What_matters_most_to_you_in_choosing_a_course","Search","Magazine","Newspaper_Article","X_Education_Forums","Newspaper","Digital_Advertisement","Through_Recommendations","Receive_More_Updates_About_Our_Courses","Tags",
           "Lead_Quality","Update_me_on_Supply_Chain_Content","Get_updates_on_DM_Content","Lead_Profile","City","Asymmetrique_Activity_Index","Asymmetrique_Profile_Index", "I_agree_to_pay_the_amount_through_cheque","A_free_copy_of_Mastering_The_Interview",       
           "Last_Notable_Activity")
lead[convert]<-lapply(lead[convert],factor)
str(lead)

#-------------------------------------Treating  Missing Values----------------------------------------------------------#

lead[is.na(lead$Total_Visits),6]<-median(lead$Total_Visits,na.rm=TRUE)
lead[is.na(lead$Page_Views_Per_Visit),8]<-median(lead$Page_Views_Per_Visit,na.rm=TRUE)
lead[is.na(lead$Last_Activity),9]<-"Email Opened"

library(stringr)
lead$Lead_Source<-str_replace(lead$Lead_Source,"google","Google")
table(lead$Lead_Source)
lead[is.na(lead$Lead_Source),2]<-"Google"

table(lead$Country)
lead[is.na(lead$Country),10]<-"India"

table(lead$Specialization)
lead[is.na(lead$Specialization),11]<-"Finance Management"

lead[is.na(lead$What_is_your_current_occupation),13]<-"Unemployed"
lead[is.na(lead$What_matters_most_to_you_in_choosing_a_course),14]<-"Better Career Prospects"

table(lead$How_did_you_hear_about_X_Education)
lead[is.na(lead$How_did_you_hear_about_X_Education),12]<-"Online Search"
lead[is.na(lead$Lead_Quality),24]<-"Might be"

table(lead$Tags)
lead[is.na(lead$Tags),23]<-"Will revert after reading the email"
lead[is.na(lead$Lead_Profile),27]<-"Potential Lead"
lead[is.na(lead$City),28]<-"Mumbai"
lead[is.na(lead$Asymmetrique_Activity_Index),29]<-"Medium"
lead[is.na(lead$Asymmetrique_Profile_Index),30]<-"Medium"
lead[is.na(lead$Asymmetrique_Activity_Score),31]<-mean(lead$Asymmetrique_Activity_Score,na.rm=TRUE)
lead[is.na(lead$Asymmetrique_Profile_Score),32]<-mean(lead$Asymmetrique_Profile_Score,na.rm=TRUE)

colSums(is.na(lead))
summary(lead)

#-------------------------------------Treating Outliers----------------------------------------------------------#

quantile(lead$Total_Visits,0.9)
quantile(lead$Total_Time_Spent_on_Website,0.9)
quantile(lead$Page_Views_Per_Visit,0.9)

lead$Total_Visits<-ifelse(lead$Total_Visits>7,7,lead$Total_Visits)
lead$Total_Time_Spent_on_Website<-ifelse(lead$Total_Time_Spent_on_Website>1380,1380,lead$Total_Time_Spent_on_Website)
lead$Page_Views_Per_Visit<-ifelse(lead$Page_Views_Per_Visit>5,5,lead$Page_Views_Per_Visit)

summary(lead)
sum<-summary(lead)
write.csv(sum,"summary.csv")

#--------------------------------------Exploratory Data Analysis--------------------------------------------------#
ggplot(lead, aes(x=Converted, fill=Converted )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("Orange","Red")) +
  theme(legend.position="none")

ggplot(lead, aes(x=Total_Time_Spent_on_Website, fill=Total_Time_Spent_on_Website )) + 
  geom_histogram(fill="blue" ,binwidth = 300) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

ggplot(lead, aes(x=Asymmetrique_Activity_Score, fill=Asymmetrique_Activity_Score )) + 
  geom_histogram(fill="pink" ,binwidth =10) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

ggplot(lead, aes(x=Asymmetrique_Profile_Score, fill=Asymmetrique_Profile_Score )) + 
  geom_histogram(fill="brown" ,binwidth =10) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

ggplot(lead, aes(x=Asymmetrique_Activity_Index, fill=Asymmetrique_Activity_Index )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("pink","yellow","brown")) +
  theme(legend.position="none")

ggplot(lead, aes(x=What_is_your_current_occupation, fill=What_is_your_current_occupation )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

ggplot(lead, aes(x=Lead_Quality, fill=Lead_Quality )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("blue","orange","black","green","pink")) +
  theme(legend.position="none")

ggplot(lead, aes(x=Search, fill=Search )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("red","pink")) +
  theme(legend.position="none")

ggplot(lead, aes(x=What_matters_most_to_you_in_choosing_a_course, fill= What_matters_most_to_you_in_choosing_a_course)) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

ggplot(lead, aes(x=Do_Not_Email, fill=Do_Not_Email )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("blue","brown")) +
  theme(legend.position="none")

ggplot(lead, aes(x=Do_Not_Call, fill=Do_Not_Call )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("orange","pink")) +
  theme(legend.position="none")

ggplot(lead, aes(x=Newspaper, fill=Newspaper )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("red","blue")) +
  theme(legend.position="none")


ggplot(lead, aes(x=Lead_Origin, fill=Lead_Origin )) + 
  geom_bar( ) +
  scale_fill_manual(values=c("orange","black","pink","brown","blue")) +
  theme(legend.position="none")


#Splitting the data into categorical and numerical data#
cat<-lead[,-c(6,7,8,31,32)]
num<-lead[,c(5,6,7,8,31,32)]
dim(cat)#Checking the dimension of the categorical column 
dim(num)#Checking the dimension of then numerical column
colnames(cat)
colnames(num)


#-------------------------------------Information Value for categorical data----------------------------------------------------------#


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

a1<-IVCal("Total_Visits","Converted",num,groups=10)
a2<-IVCal("Total_Time_Spent_on_Website","Converted",num,groups=10)
a3<-IVCal("Page_Views_Per_Visit","Converted",num,groups=10)
a4<-IVCal("Asymmetrique_Activity_Score","Converted",num,groups=10)
a5<-IVCal("Asymmetrique_Profile_Score","Converted",num,groups=10)

IV_num<- data.frame(rbind(a1,a2,a3,a4,a5))
IV_num

#-------------------------------------Information Value for categorical data----------------------------------------------------------#


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

A<- CA("Converted","Lead_Origin",cat)
B<- CA("Converted","Lead_Source",cat)
C<- CA("Converted","Do_Not_Email",cat)
D<- CA("Converted","Do_Not_Call",cat)
E<- CA("Converted","Last_Activity",cat)
F<- CA("Converted","Country",cat)
G<- CA("Converted","Specialization",cat)
H<- CA("Converted","How_did_you_hear_about_X_Education",cat)
I<- CA("Converted","What_is_your_current_occupation",cat)
J<- CA("Converted","What_matters_most_to_you_in_choosing_a_course",cat)
K<- CA("Converted","Search",cat)
L<- CA("Converted","Magazine",cat)
M<- CA("Converted","Newspaper_Article",cat)
N<- CA("Converted","Digital_Advertisement",cat)
O<- CA("Converted","Through_Recommendations",cat)
P<- CA("Converted","Receive_More_Updates_About_Our_Courses",cat)
Q<- CA("Converted","Tags",cat)
R<- CA("Converted","Lead_Quality",cat)
S<- CA("Converted","Update_me_on_Supply_Chain_Content",cat)
T<- CA("Converted","Get_updates_on_DM_Content",cat)
U<- CA("Converted","Lead_Profile",cat)
V<- CA("Converted","City",cat)
W<- CA("Converted","Asymmetrique_Activity_Index",cat)
X<- CA("Converted","Asymmetrique_Profile_Index",cat)
Y<- CA("Converted","I_agree_to_pay_the_amount_through_cheque",cat)
Z<- CA("Converted","A_free_copy_of_Mastering_The_Interview",cat)
A1<-CA("Converted","Last_Notable_Activity",cat)
B1<-CA("Converted","Newspaper",cat)
C1<-CA("Converted","X_Education_Forums",cat)


IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,A1,B1,C1))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV


lead<-lead[,-c(6,8,10,12,16,20,21,22,25,26,27,28,30,33,34)]

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(144)#This is used to produce reproducible results, everytime we run the model

sql<-sample.split(lead$Converted,0.7)
train<-subset(lead,sql==TRUE)
dim(train)
head(train)
lead$Lead_Source<-as.factor(lead$Lead_Source)
summary(train)
str(train)
lead<-lead[,-c(12,13)]#We are dropping Newspaper_Article and X_Education Forums 
#since under train dataset these are only one  factor levels

test<-subset(lead,sql==FALSE)
dim(test)
head(test)


#-------------------------------------Logistic Regression Model Building------------------------------------------#

model <- glm(Converted~., data=train, family=binomial())
summary(model)

##Removing Last_Notable_Activity==Marked Spam
model1 <- glm(Converted~Lead_Origin+Lead_Source+Do_Not_Email+Do_Not_Call+Total_Time_Spent_on_Website+Last_Activity+Specialization+What_is_your_current_occupation+Search+Newspaper+Tags+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score+I(Last_Notable_Activity=="Email Link Clicked")+
I(Last_Notable_Activity=="Email Opened")+I(Last_Notable_Activity=="Email Received")+I(Last_Notable_Activity=="Form Submitted on Website")+I(Last_Notable_Activity=="Had a Phone Conversation")+I(Last_Notable_Activity=="Modified")+I(Last_Notable_Activity=="Olark Chat Conversation")+I(Last_Notable_Activity=="Page Visited on Website")+I(Last_Notable_Activity=="SMS Sent")+I(Last_Notable_Activity=="Unreachable")+I(Last_Notable_Activity=="Unsubscribed")+I(Last_Notable_Activity=="View in browser link Clicked"), data=train, family=binomial())
summary(model1)

##Removing Lead_Source
model2 <- glm(Converted~Lead_Origin+Do_Not_Email+Do_Not_Call+Total_Time_Spent_on_Website+Last_Activity+Specialization+What_is_your_current_occupation+Search+Newspaper+Tags+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score+I(Last_Notable_Activity=="Email Link Clicked")+
                I(Last_Notable_Activity=="Email Opened")+I(Last_Notable_Activity=="Email Received")+I(Last_Notable_Activity=="Form Submitted on Website")+I(Last_Notable_Activity=="Had a Phone Conversation")+I(Last_Notable_Activity=="Modified")+I(Last_Notable_Activity=="Olark Chat Conversation")+I(Last_Notable_Activity=="Page Visited on Website")+I(Last_Notable_Activity=="SMS Sent")+I(Last_Notable_Activity=="Unreachable")+I(Last_Notable_Activity=="Unsubscribed")+I(Last_Notable_Activity=="View in browser link Clicked"), data=train, family=binomial())
summary(model2)

##Removing Last_Notable_Activity
model3<- glm(Converted~Lead_Origin+Do_Not_Email+Do_Not_Call+Total_Time_Spent_on_Website+Last_Activity+Specialization+What_is_your_current_occupation+Search+Newspaper+Tags+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score, data=train, family=binomial())
summary(model3)

##Removing Last_Activity
model4<- glm(Converted~Lead_Origin+Do_Not_Email+Do_Not_Call+Total_Time_Spent_on_Website+Specialization+What_is_your_current_occupation+Search+Newspaper+Tags+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score, data=train, family=binomial())
summary(model4)

##Removing Tags
model5<-glm(Converted~Lead_Origin+Do_Not_Email+Do_Not_Call+Total_Time_Spent_on_Website+Specialization+What_is_your_current_occupation+Search+Newspaper+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model5)

##Removing Do_Not_Call
model6<-glm(Converted~Lead_Origin+Do_Not_Email+Total_Time_Spent_on_Website+Specialization+What_is_your_current_occupation+Search+Newspaper+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model6)

##Removing Newspaper
model7<-glm(Converted~Lead_Origin+Do_Not_Email+Total_Time_Spent_on_Website+Specialization+What_is_your_current_occupation+Search+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model7)

##Removing Lead_Origin=="Quick Add Form"
model8<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+I(Lead_Origin=="Lead Import")+Do_Not_Email+Total_Time_Spent_on_Website+Specialization+What_is_your_current_occupation+Search+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model8)

##Removing Specialization==Projects Management,Specialization==Supply Chain Management,Specialization==Media and Advertising
model9<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+I(Lead_Origin=="Lead Import")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Healthcare Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
I(Specialization=="Marketing Management")+I(Specialization=="Operations Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+What_is_your_current_occupation+Search+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model9)

##Removing What_is_your_current_occupation=="Housewife"
model10<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+I(Lead_Origin=="Lead Import")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Healthcare Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
              I(Specialization=="Marketing Management")+I(Specialization=="Operations Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Student")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Search+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model10)

##Removing Lead_Origin=="Lead Import"
model11<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Healthcare Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
I(Specialization=="Marketing Management")+I(Specialization=="Operations Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Student")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Search+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model11)

##Removing Search
model12<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Healthcare Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Operations Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Student")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model12)

##Removing What_is_your_current_occupation=="Student"
model13<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Healthcare Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Operations Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model13)

##Removing Specialization=="Healthcare Management"
model14<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Operations Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model14)

##Removing Specialization=="Operations Management"
model15<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="E-COMMERCE")+I(Specialization=="Finance Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model15)

##Removing Specialization=="E-Commerce"
model16<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="Finance Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Services Excellence")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model16)

##Removing Specialization=="Services Excellence"
model17<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="Finance Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Other")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model17)

##Removing What_is_your_current_occupation=="Other"
model18<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="Finance Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Marketing Management")+I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model18)

##Removing Specialization=="Marketing Management"
model19<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="Finance Management")+I(Specialization=="Hospitality Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model19)

##Removing Specialization=="Hospitality Management"
model20<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="E-Business")+I(Specialization=="Finance Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
               I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model20)

##Removing Specialization=="E-Business"
model21<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+I(Specialization=="Human Resource Management")+I(Specialization=="International Business")+                   
I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model21)

##Removing Specialization=="International Business"
model22<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+I(Specialization=="Human Resource Management")+                   
               I(Specialization=="Retail Management")+I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model22)

##Removing Specialization=="Retail Management"
model23<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+I(Specialization=="Human Resource Management")+                   
I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model23)

##Removing Specialization=="Human Resource Management"
model24<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+                   
               I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Activity_Score+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model24)

#Variance Inflation Factor
vif(model24)

##Removing Asymmetrique Activity Score
model25<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+                   
               I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+Asymmetrique_Activity_Index+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model25)

##Removing Asymmetrique Activity Index=="Medium"
model26<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+                   
               I(Specialization=="Rural and Agribusiness")+I(Specialization=="Travel and Tourism")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+I(Asymmetrique_Activity_Index=="Low")+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model26)

##Removing Specialization=="Travel and Tourism"
model27<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+                   
               I(Specialization=="Rural and Agribusiness")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+I(Asymmetrique_Activity_Index=="Low")+Asymmetrique_Profile_Score,data=train, family=binomial())
summary(model27)
vif(model27)


#------------------------------Validating the model on test data------------------------------#

modeltest<-glm(Converted~I(Lead_Origin=="Landing Page Submission")+I(Lead_Origin=="Lead Add Form")+Do_Not_Email+Total_Time_Spent_on_Website+I(Specialization=="Finance Management")+                   
               I(Specialization=="Rural and Agribusiness")+I(What_is_your_current_occupation=="Unemployed")+I(What_is_your_current_occupation=="Working Professional")+Lead_Quality+I(Asymmetrique_Activity_Index=="Low")+Asymmetrique_Profile_Score,data=test, family=binomial())
summary(modeltest)
vif(modeltest)

dwt(modeltest)

#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(modeltest),Sigma=vcov(modeltest),Terms=1:15)
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0

#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
#significantly improves the model fit or not)

# Difference between null deviance and deviance
modelchi<-modeltest$null.deviance-modeltest$deviance
modelchi

#Finding the degree of freedom for Null model and model with variables
chidf<-modeltest$df.null-modeltest$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob<-1-pchisq(modelchi,chidf)
format(round(chisq.prob, 2), nsmall = 5)

#------------------------------------Predicting power of the model using R2----------------------------#
#Expected good range of R2 is between 0.2-0.4
PseudoR2(modeltest)

# Hosmer and Lemeshow given by the McFadden 6R square
R2.hl<-modelchi/model$null.deviance
R2.hl


# Cox and Snell R Square (the last number; 

R.cs <- 1 - exp ((model$deviance - model$null.deviance) /nrow(test))
R.cs

# Max rescaled R square (Nagelkarke) 

R.n <- R.cs /(1-(exp(-(model$null.deviance/(nrow(test))))))
R.n


#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(modeltest) # deviance residuals
residuals(modeltest, "pearson") # pearson residuals

sum(residuals(modeltest, type = "pearson")^2)
deviance(modeltest)

#########Larger p value indicate good model fit
1-pchisq(deviance(modeltest), df.residual(modeltest))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies


################################################################################################################
# How to use the function. Data.train is the name of the dataset. model is name of the glm output
## This is not a very useful test. Some authors have suggested that sometimes it produces wrong result##
## High p value incidates the model fits well


# Hosmer and Lemeshow test 
options (scipen = 999)
hl <- hoslem.test(as.integer(test$Converted), fitted(modeltest), g=10)
hl
#####################################################################################################################
# Coefficients (Odds)
modeltest$coefficients
# Coefficients (Odds Ratio)
exp(modeltest$coefficients)#Interpret 


# Variable Importance of the model
varImp(modeltest)


# Predicted Probabilities
prediction<-predict(modeltest,newdata=test,type="response")

rocCurve   <- roc(response = test$Converted, predictor = factor(prediction, ordered =TRUE), 
                  levels = rev(levels(test$Converted)))

#Metrics - Fit Statistics

threshold<-as.numeric(coords(rocCurve,"best")[1])
predclass <-ifelse(prediction>threshold,1,0)

Confusion <- table(Predicted = predclass,Actual = test$Converted)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1


AUCmetric <- data.frame(c(coords(rocCurve,"best",transpose = TRUE),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
par(mar = c(1, 1, 1, 1))
plot(rocCurve)

### KS statistics calculation
test$m1.yhat <- predict(modeltest, test, type = "response")

m1.scores <- prediction(test$m1.yhat, test$Converted)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70


#-------------------------------------------Decision Tree----------------------------------------------#

#-------------------------------------------Building the CART model----------------------------------------------#

CART1<-rpart(Converted~.,data=train, method = "class")
prp(CART1)
CART1



#-------------------------Checking the accuracy of the model in the test data------------------------------#
predictCART1<-predict(CART1, newdata=test, type = "class")
table(test$Converted,predictCART1)



#ConfusionMatrix
confusionMatrix(predictCART1,test$Converted)

#----------------Reciever Operating Characterstics Curve for CART------------------------------------------#

CARTroc<-roc(response=test$Converted, predictor = factor(predictCART1,ordered=TRUE),
             level = rev(levels(test$Converted)))
CARTroc
plot(CARTroc)
#Area under the curve: 0.8972#

#---------------------------End of the CART model---------------------------------------------------------#



#------------------------------------A Random Forest Model-------------------------------------------------#
PredictForest1<-randomForest(Converted~.,data = train)
PredictForest1


#--------------------Checking the accuracy of the model-------------------------------------------#
predForest1<-predict(PredictForest1, newdata=test, type = "class")
table(test$Converted,predForest1)

#ConfusionMatrix
confusionMatrix(predForest1,test$Converted)

#---------------------------Variable Importance chart in Random Forest---------------------------------------#

vu = varUsed(PredictForest1, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(PredictForest1$forest$xlevels[vusorted$ix]))


#--------------------Measuring Impurity in the Random Forest Model-----------------------------------------#

#A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. 
#In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. 
#Therefore, one way to measure the importance of a variable is to average the reduction in impurity, 
#taken over all the times that variable is selected for splitting in all of the trees in the forest. 
varImpPlot(PredictForest1)
var<-varImp(PredictForest1)
var

