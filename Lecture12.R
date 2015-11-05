library(caTools)
library(caret)
titanic<-read.csv("Titanic_imputed.csv")
titanic$pclass<-factor(titanic$pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))
titanic$survived<-factor(titanic$survived, levels=c(0,1), labels=c("No","Yes"))

set.seed(0007)

index <- createDataPartition(titanic$survived, p=.75, list=FALSE)
train<-titanic[index,]
test<-titanic[-index,]
library(rpart) # recursive partition
library("rpart.plot")
#install.packages("rpart.plot")
fit<-rpart(survived~sex, data=train, method="class") #method-> classification
# fit is decision tree object, dependent var= survived, indep var = sex
prp(fit, type=1, extra=2) #plot a anum prost@, extra->extra inform is drawn on the plot
prop.table(table(train$survived,train$sex),2)
fit1<-rpart(survived~sex+age+sibsp+parch+pclass,data=train,method="class")
prp(fit1, type=1, extra=4) #plot a anum prost@, extra->extra inform is drawn on the plot
?prp
install.packages("rattle")
library(rattle)
asRules(fit1)
pred <- predict(fit1, test, type="class")
table(pred,test$survived)

weather<-read.csv("weather.csv")
row.names(weather) <- weather$Date
weather$X<-NULL
weather$Date<-NULL
weather$Location<-NULL

index <- createDataPartition(weather$RainTomorrow, p=.75, list=FALSE)
train<-weather[index,]
test<-weather[-index,]

train$RISK_MM<-NULL
test$RISK_MM<-NULL


fit<-rpart(RainTomorrow~., data=train, method="class") 
prp(fit, type=1, extra=4) 

?prp
pred <- predict(fit, test, type="class")
table(pred,test$RainTomorrow)
accuracuy=(72+5)/(72+11+3+5)
  
accuracuy

