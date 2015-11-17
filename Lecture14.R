library(e1071)
Diabetes<-read.csv("Diabetes.csv")
Diabetes$Class<-factor(Diabetes$Class,levels = c(0,1),labels = c("No","Yes"))
library(caTools)
set.seed(0007)
Index<-createDataPartition(Diabetes$Class,p=0.8,list=FALSE)
training<-Diabetes[Index,]
testing<-Diabetes[-Index,]

Model<-svm(Class~.,data = training,kernel="linear",probability=TRUE)
Predicted<-predict(Model,newdata = testing)
table(Predicted,testing$Class)
confusionMatrix(table(Predicted,testing$Class))

library(MASS)
lda.diab<-lda(Class~.,data = training)
x<-predict(lda.diab, newdata = testing)
names(x)
table(testing$Class,x$class) #confusion matrix for the reality vs prediction
x$x
confusionMatrix(table(testing$Class,x$class))

