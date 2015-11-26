####HW7 LDA and SVM
###For this hoemwork we are going to use wine dataset (from k-nn model) to 
##predict wine quality. For the problem, poor quality will be 
##regarded as a positive case. The homework worths 70 points.

library(kernlab) #better SVM library. Kaggle :D Best tune finding
library(MASS)
library(e1071)
library(caTools)
library(caret)


train<-read.csv("Wine train.csv")
test<-read.csv("Wine test.csv")

train$quality<-factor(train$quality, levels=c(0,1),labels=c("Poor Quality", "Good Quality"))
test$quality<-factor(test$quality, levels=c(0,1),labels=c("Poor Quality", "Good Quality"))

##1. (30 points) develop LDA model to predict wine quality:

model<-lda(quality ~., data=train)
x<-predict(model, newdata = test)
names(x)
table(test$quality,x$class) #confusion matrix for the reality vs prediction
x$x
confusionMatrix(table(test$quality,x$class)) #by default positive class is POOR QUALLITY class

#Accuracy : 0.7467  
#Sensitivity : 0.7698          
#Specificity : 0.6724    

###report accuracy measures on testing dataset
##2. (30 points) Develop SVM model, try for different "cost values" in the model. 
##Report accuracy measures on testing dataset. Report your best model
obj_linear <-best.tune(svm, quality ~., data = train, kernel = "linear")
obj_linear

svm_linear<-svm(quality~.,data = train,kernel =
                  "linear",gamma=0.09090909,cost=1)


Prediction3 <- predict(svm_linear, newdata = test)
plot(Prediction3)
confusionMatrix(table(Prediction3,test$quality))

#Accuracy : 0.7598          
#Sensitivity : 0.8980          
#Specificity : 0.4854  

obj_polynomial <-best.tune(svm, quality ~., data = train, kernel = "polynomial") #obtaining best polynomial SVM
obj_polynomial

svm_polynomial<-svm(quality~.,data = train,kernel =
                  "polynomial",gamma=0.09090909,degree=3,cost=1)



Prediction4 <- predict(svm_polynomial, newdata = test)
plot(Prediction4)
confusionMatrix(table(Prediction4,test$quality))

#Accuracy : 0.7712          
#Sensitivity : 0.9545          
#Specificity : 0.4073 

obj_radial <-best.tune(svm, quality ~., data = train, kernel = "radial") #obtaining best polynomial SVM
obj_radial

svm_radial<-svm(quality~.,data = train,kernel =
                      "radial",gamma=0.09090909,cost=1)



Prediction5 <- predict(svm_radial, newdata = test)
plot(Prediction5)
confusionMatrix(table(Prediction5,test$quality))

#Sensitivity : 0.8894          
#Specificity : 0.5805   
#Accuracy : 0.7859          

obj_sigmoid <-best.tune(svm, quality ~., data = train, kernel = "sigmoid") #obtaining best polynomial SVM
obj_sigmoid

svm_sigmoid<-svm(quality~.,data = train,kernel =
                  "sigmoid",gamma=0.09090909,cost=1,coef.0=0)



Prediction6 <- predict(svm_sigmoid, newdata = test)
plot(Prediction6)
confusionMatrix(table(Prediction6,test$quality))

#Accuracy : 0.6789        
#Sensitivity : 0.7703        
#Specificity : 0.4976   

##3. (10 points) Compare the results from point 1 and point 2. Which one do you chose?

#lda gave a quite mediocre result of all models, yet the specificity that it had
# was the best of all, so it might be a good idea to test the GOOD WINE QUALITY on that model

# The Winner of models for today was the Polynomial tuned model with 95.45% sensitivity
#and overally the SVM models do mostly better then svm in terms of sensitivity.

#SVM-it is.....with kernlab with can have a lot of model variations. So be it :D

