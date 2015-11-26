####HW6- You are going to use the same dataset we have used for the logistic regression model
### the goal is to build a model that will predict default for the customer
##Read train and test datasets, you need to build the model on Train dataset
##1. prepare your dataset for the analysis 
library(class)
library(caret)
library(caTools)
library(jsonlite)
library(tm)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)

default_train<-read.csv("default train.csv")
default_test<-read.csv("default test.csv")

default_train$X<-NULL
default_test$X<-NULL

default_train$default<-factor(default_train$default,levels=c(0,1),labels=c("Not Defaulted","Defaulted"))
default_test$default<-factor(default_test$default,levels=c(0,1),labels=c("Not Defaulted","Defaulted"))

##2.(7points) Build a decision tree on the default_train dataset, 
fit<-rpart(default~., data=default_train, method="class") #method-> classification
prp(fit, type=1, extra=4)
prp(fit)
PredFit<-predict(fit, newdata=default_test, type="class")
table(default_test$default,PredFit)
confusionMatrix(table(PredFit,default_test$default),positive = "Defaulted")
fit

asRules(fit)

#from the Homework04 we can see the collinear variables. 
#Interesting observation 1: when we remove the collinears from the tree
#we get a bump in sensativity and SLIGHT devrease in specificity
fit1<-rpart(default~.-othdebt-income-age-debtinc-address, data=default_train, method="class") #method-> classification
prp(fit1, type=1, extra=4)
prp(fit1)
PredFit1<-predict(fit1, newdata=default_test, type="class")
table(default_test$default,PredFit1)
confusionMatrix(table(PredFit1,default_test$default),positive = "Defaulted")

#Interesting observation 2: the maximum i got was 48% of sensitivity although might have lost some info.
#i guess not meant tobe sensative :(


##2.1 (3points)Clearly state decision rules, resulting classsification and
##the probability of the class at the terminal node
asRules(fit) # The command as rules gives the rules and the prob for terminal node
print("************")
asRules(fit1)
print("************")

##3.(5points) Test the accuracy of your model on the default_test dataset: 
##report Accuracy, Snesitivity and Specificity
confusionMatrix(table(PredFit,default_test$default),positive = "Defaulted")

#Accuracy : 0.7314   
#Sensitivity : 0.3878          
#Specificity : 0.8651  

confusionMatrix(table(PredFit1,default_test$default),positive = "Defaulted")

#Accuracy : 0.7429 
#Sensitivity : 0.4898          
#Specificity : 0.8413 


##4.(8points)KNN
DataS<-as.data.frame(scale(default_train[-9]))
DataS$default<-default_train$default #already factored

TestS<-as.data.frame(scale(default_test[-9]))
TestS$default<-default_test$default

dim(TestS)
dim(DataS)

M<-knn(DataS[,-9],TestS[,-9],cl=DataS$default,k=4)
table(TestS$default,M)
summary(M)
confusionMatrix(M, TestS$default,positive = "Defaulted" )

#k-fold cross validation

ctrl<-trainControl(method="repeatedcv", repeats=5, number=10)
knnFit<-train(default~., data=DataS, method="knn",trControl=ctrl, tuneLength=20)
knnFit
plot(knnFit) # ay sents lav patkeracreci.

finalKNN<-knn(DataS[,-9],TestS[,-9],cl=DataS$default,k=7)#best k=7
table(TestS$default,finalKNN)

confusionMatrix(finalKNN, TestS$default,positive = "Defaulted" ) 

##4.1(5points) Report Accuracy, Sensitivity and Specificity for the default_test dataset

confusionMatrix(M, TestS$default,positive = "Defaulted" ) #just better sensitivity nearly at 30
#Sensitivity : 0.3673         
#Specificity : 0.8968   
#Accuracy : 0.7486         

confusionMatrix(finalKNN, TestS$default,positive = "Defaulted" )  #best model with big specificity but bad sensitivity

#Accuracy : 0.7371          
#Sensitivity : 0.26531         
#Specificity : 0.92063   

##5. (7points) Which model will you prefer ? explain using model acccuracy metrics

# i definelty  would take knn over tree just for the sole reason that knn gets
# better specificity with nearly same LOW sensitivity. So why not take the model
# that is at least working best in non default case.
