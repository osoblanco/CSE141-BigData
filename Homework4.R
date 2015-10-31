##########Homework 4 ###########
##You need to build a model that will predict probability of credit default

#Part 1.
##1. Read the dataset HW4_model into R

DataM<-read.csv("HW4-Modeling.csv")
library(caret)
library(caTools)
library(ggplot2)
library(ROCR)
library(class)


##2. (1p) Look at the descriptions of the variables. If there are variables 
##that needs to be converted into categorical ones, do it.
DataM$default<-as.factor(DataM$default)
DataM$ed<-as.factor(DataM$ed)
DataM$default<-factor(DataM$default,levels=c(0,1),labels=c("No","Yes"))
DataM$ed<-factor(DataM$ed,levels=c(1,2,3,4,5),labels=c("didnt complete High school","High School degree","college degree","undergraduate","postgraduate"))


##3. (2p) Create testing and training datasets with 75% going to 
#Testing dataset (dont forget to set the seed)
set.seed(0007)
Index<-createDataPartition(DataM$default,p=0.25,list=FALSE)
training<-DataM[Index,]
testing<-DataM[-Index,]

##4. (10p) Build the model, show all the steps
##(dont forget about multicollinearity and significance of the variables)
Model_Initial<-glm(default~age+ed+employ+address+income+debtinc+creddebt+othdebt,data=training,family="binomial")
summary(Model_Initial)
print(cor(training[,c("age","employ","address","income","debtinc","creddebt","othdebt")]), digits = 4)

#I can see that othdebt is colinear with debtinc. Looking in my model i can see that the P-value of debtinc(0.90729)
#is alot larger then the P-value of othdebt(0.43959). Thus we can remove debtinc.

Model_Second<-glm(default~age+ed+employ+address+income+creddebt+othdebt,data=training,family="binomial")
summary(Model_Second)
print(cor(training[,c("age","employ","address","income","creddebt","othdebt")]), digits = 4)

#Now I see that othdebt is collinear with creddebt(0.6883). The P-value of othdebt is alot greater then the P-value of 
#creddebt.

Model_Third<-glm(default~age+ed+employ+address+income+creddebt,data=training,family="binomial")
summary(Model_Third)
print(cor(training[,c("age","employ","address","income","creddebt")]), digits = 4)

#for the very same reasons income who is collinear with employ must be removed.

Model_Fourth<-glm(default~age+ed+employ+address+creddebt,data=training,family="binomial")
summary(Model_Fourth)
print(cor(training[,c("age","employ","address","creddebt")]), digits = 4)

#for the very same reasons age who is collinear with adress must be removed.
Model_Five<-glm(default~ed+employ+address+creddebt,data=training,family="binomial")
summary(Model_Five)
print(cor(training[,c("employ","address","creddebt")]), digits = 4)


##5. (1p) Report your final model
exp(Model_Five$coefficients)
#Depending on different dummy variables the intercept will be different. The base is (didnt complete school) people.
# || - means or
# ln(p/1-p)= -0.26865*employ + address*(-0.05917)+ 1.04458*creddebt + (0.60309 || 0.14972 || 0.19376 || -13.02107)


##5.1 (4p) Explain the meaninings of each coefficient in the regression

#Every 1 unit increase in employment constitutes to the DECREASE of the logit (logarithm of default odds) by 0.26865 
#Every 1 unit increase in creddebt constitutes to the INCREASE of the logit (logarithm of default odds) by 1.04458
#Every 1 unit increase in address(more years live in the same adress) constitutes to the INCREASE of the logit (logarithm of default odds) by 0.05917
#if the person has a high school degree then the logit is increased by 0.60309, compared to people with no degree.
#if the person has a college degree then the logit is increased by 0.14972, compared to people with no degree.
#if the person has a undergradute degree then the logit is increased by 0.19376, compared to people with no degree.
#if the person has a post graduate degree then the logit is decreased by 13.02107, compared to people with no degree.

##5.2 (2p) Create a confusion matrix on the testing dataset.
PredTest<-predict(Model_Five,newdata =testing,type="response")#Predict percentage of survival rate
summary(PredTest)
hist(PredTest)
table(Predicted=PredTest>0.5,Actual=testing$default)

#Report the following metrics on the testing dataset
#Overall Accuracy (361+52)/(361+85+26+52)=0.7881679
#Sensitivity 52/(52+85)=0.379562
#Specificity 361/(361+26)=0.9328165
#False Positive rate 26/(26+361)=0.06718346

##5.4 (4p) Explain the meaning of the Sensitivity, 
##Specificty and False Positive Rate within the context of this problem.
##If you are the manager of the bank how will you use this information?

#The sensitivity shows that of all predicted defaults only 37.9562% will be(are really) defaulted. 
#the specificity shows that of all predicted non-defaults (paybacks) 93.28165% will be accurate.
#The False Positive rate is at 6.718346 which shows that out of all predicted default cases 6.718346% is actually non-deafult.

#it is better(safer) to give a loan to people who are predicted to be non-default. (Specificity is awesome)

#I could have changed the cut of value to get better Sensitivity but its alot better to have high Specificty.
#It means we can have safe loans with 93.28165% accuracy.

##5.5 (2p) Create the ROC curve and find the AUC value. Are you happy with the result?

Pred<-prediction(PredTest,testing$default)
perf<-performance(Pred,"tpr","fpr") #tpr=sensitivity #fpr=1-sensitivity
plot(perf)

AUC<-performance(Pred,"auc")@y.values #INCHQAN LAVN MER MODEL@......meaning area under curve
AUC

#Im preety much happy...however could have tried playing with the cut of values.

##Part Two: Scoring
#The dataset HW4-Scoring.csv contains data on 150 potential customers for the loan.
#If you are the manager of the bank, you would prefer to give loanss to those who have lower probability of default
#As you can see the default variable is empty, as you need to predict probability of default for these cases.
#6.0 (2p) Predict credit score (probability of default in this case) for the new customers

DataS<-read.csv("HW4-Scoring.csv")
DataS$ed<-as.factor(DataS$ed)
DataS$ed<-factor(DataS$ed,levels=c(1,2,3,4,5),labels=c("didnt complete High school","High School degree","college degree","undergraduate","postgraduate"))

row.names(DataS) <- DataS$Cust.ID
DataS$Cust.ID <- NULL

#Add the new variable with the score to your scoring dataset

DataS$default<-predict(Model_Five,newdata =DataS,type="response")#Predict percentage of survival rate


##6.1 (2p) Create a histogram for the scores, what does it tell you?
hist(DataS$default)

##6.2 (5p) Lets say the bank wants to give a credit to the least riskier group. 
#Create a dataset with Top-25% of the customers with the 
#(hint: use quartiles. You need to be caautious about defining what is the risk here) 
#lowest risk of not paying back the credit
summary(DataS$default)
data<-DataS[DataS$default<=0.0283300,]
summary(data)
#How many customers are there?
#38 customers are present

#Make a summary for the dataset with Top25%. Describe these customers. Anything interesting? 

#The most interesting thing is that most of the people in the dataset havent completed high school degree.
#How is this possible i dont know???? INteresting case: 53 Year old person and income 324.000$ of income with no education?????
#HOWWW? Is it you God????? (Chto eto za sozdaniye takoye???)

summary(data$income) #doesnt give any particular information (much fluctuations)
summary(data$debtinc) # doesnt give anythig either.
