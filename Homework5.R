####HW5-KNN###
#Download and read "Wine train.csv" and "Wine test.csv", 
#you will use them as training and testing datasets
#The dataset contain 11 attributes describing chemcial characteristics of the wine.
#The categorical variable quality has two values -0="High Quality", 1="Poor Quality"
#Your goal is to build model to predict wine quality.
#1. (14 points) Develop k-NN algorithm, 
### Use caret functionality to find the optimal number of neighbours, 
### or try different values of K using "class" library.
## Report accuracy, Sensitivity and Specificity on the Testing dataset

library(class)
library(caret)
training<-read.csv("Wine train.csv")
DataS<-as.data.frame(scale(training[-12]))
DataS$quality<-training$quality

DataS$quality<-factor(DataS$quality,levels=c(0,1),labels=c("Poor Quality","High Quality"))

testing<-read.csv("Wine test.csv")
TestS<-as.data.frame(scale(testing[-12]))
TestS$quality<-testing$quality

TestS$quality<-factor(TestS$quality,levels=c(0,1),labels=c("Poor Quality","High Quality"))

dim(TestS)
dim(DataS)

M<-knn(DataS[,-12],TestS[,-12],cl=DataS$quality,k=3)
table(TestS$quality,M)
summary(M)

#k-fold cross validation
install.packages("e1071")
library(e1071) #just in case

ctrl<-trainControl(method="repeatedcv", repeats=5, number=10)
knnFit<-train(quality~., data=DataS, method="knn",trControl=ctrl, tuneLength=20)
knnFit
plot(knnFit) # ay sents lav patkeracreci.

finalKNN<-knn(DataS[,-12],TestS[,-12],cl=DataS$quality,k=25)
table(TestS$quality,finalKNN)

confusionMatrix(finalKNN, TestS$quality ) #found This ;) . Calculates the dirty job for me

#Accuracy - 0.7778
#Specificity - 0.5561
#Sensitivity - 0.8894

#2. (14 points) Develop Logistic regression to predict wine quality
##(follow all model building steps, and be sure you have a good model) 


Model_New<-glm(quality~.,data=training,family="binomial")
summary(Model_New)
print(cor(DataS[,-12]), digits = 1)
PredTest<-predict(Model_New,newdata =testing,type="response")#Predict percentage of survival rate
summary(PredTest)
hist(PredTest)
table(Predicted=PredTest>0.5,Actual=testing$quality)
Pred<-prediction(PredTest,testing$quality)
perf<-performance(Pred,"tpr","fpr") #tpr=sensitivity #fpr=1-sensitivity
plot(perf)

AUC<-performance(Pred,"auc")@y.values #INCHQAN LAVN MER MODEL@
AUC

#I see a great colliniarety between residual.sugar ~ density(0.84), density is less significant
# so it must go. 

#I also see a great colliniarety between total.sulfur.dioxide ~ free.sulfur.dioxide, total.sulfur.dioxide is less significant
#so it must go.

#I also see a great colliniarety between alcohol ~ density, density is less significant
#so  one more time must go.

TrainMutable<-training
TrainMutable$density<-NULL
TrainMutable$total.sulfur.dioxide<-NULL

Model_New1<-glm(quality~.,data=TrainMutable,family="binomial")
summary(Model_New1)
PredTest1<-predict(Model_New1,newdata =testing,type="response")#Predict percentage of survival rate

summary(PredTest1)
hist(PredTest1)

table(Predicted=PredTest1>0.6,Actual=testing$quality)
Pred1<-prediction(PredTest1,testing$quality)
perf<-performance(Pred1,"tpr","fpr") #tpr=sensitivity #fpr=1-sensitivity
plot(perf)

AUC<-performance(Pred1,"auc")@y.values #INCHQAN LAVN MER MODEL@
AUC

table(Predicted=PredTest1>0.4,Actual=testing$quality)
table(Predicted=PredTest1>0.5,Actual=testing$quality)
table(Predicted=PredTest1>0.6,Actual=testing$quality)
table(Predicted=PredTest1>0.7,Actual=testing$quality)

#balancing between good specificity and sensitivity lets choose.... 
table(Predicted=PredTest1>0.5,Actual=testing$quality)
accuracy=(720+203)/(720+203+94+207)
accuracy

#3. (7points) If you have to compare both models, which of them is doing a better job?
#Explain


#if i was to compare both models i would definetly choose KNN due to this reason

# 1.Higher accuracy vs glm
# 2.KNN algo is based on numerous tryout and error by machine while in glm
# i made my own assumptions on colliniarity what to remove what not to.
# It might sound strange but here i trust the machin more.
# 3. KNN is also doing better in Sensitivity/Specificity departament

#Unanimous decison by all 3 judges to KNN!!!!


