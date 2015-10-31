example<-read.csv("KNN small example.csv")
names(example)
summary(example)
dim(example)
example$Default<-factor(example$Default,levels=c(0,1),labels=c("No","Yes"));
library(ggplot2)
qplot(Age,Loan,data=example,colour=Default,size=I(5)) #we scatterplot the Age by Loan graphic paying respect wether it was DEFAULTED or not, thus the colours are different

#New customer
#Age=30,Loan= 130.000
New<-c(30,130000,NA)
e1<-rbind(example,New);

#lets Run 3 neares neighbors algorithm.... to see if the 3 closest neighbors are Yes or No.
#we calculate the close ones and see wether Yes cases or  No cases win.
qplot(Age,Loan,data=e1,colour=Default,size=I(5)) #we scatterplot the Age by Loan graphic paying respect wether it was DEFAULTED or not, thus the colours are different

#New customer
#Age=50,Loan= 145000
New<-c(50,145000,NA)
e2<-rbind(e1,New)
scaled<-as.data.frame(scale(e2[-3]))
scaled$Default<-e2$Default
dist<-dist(scaled[-3],method="euclidian")
dist

scaled[c(3,5,9),]

rm(list=ls())
diabetis<-read.csv("Diabetes.csv")
DataS<-as.data.frame(scale(diabetis[-9]))
DataS$Class<-diabetis$Class
DataS$Class<-factor(DataS$Class,levels=c(0,1),labels=c("Negative","Positive"))
table(DataS$Class)

library(class)
library(caret)

set.seed(0007)
Index<-createDataPartition(DataS$Class,p=0.8,list=FALSE,times =1)
training<-diabetis[Index,]
testing<-diabetis[-Index,]
training$Class<-factor(training$Class,levels=c(0,1),labels=c("Negative","Positive"))
testing$Class<-factor(testing$Class,levels=c(0,1),labels=c("Negative","Positive"))


#BUGGGGGGG

M<-knn(training,testing ,cl=training$Class,k=3)
table(testing$Class,M)

#k-fold cross validation
ctrl<-trainControl(method = "repeatedcv",number = 10,repeats=3)
knnFit<-train(Class~.,data=training,method="knn",trcontrol=ctrl,tuneLength=21)



