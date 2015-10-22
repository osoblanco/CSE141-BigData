titanic<-read.csv("Titanic_imputed.csv")
str(titanic)
titanic$pclass<-as.factor(titanic$pclass)
titanic$survived<-factor(titanic$survived,levels=c(0,1),labels=c("No","Yes"))
Model_S<-glm(survived~sex,data=titanic,family="binomial")
summary(Model_S)
names(Model_S)

exp(Model_S$coefficients)
#the probabilty tha males survie is 0.08843935 less oddly the for females

table(titanic$survived,titanic$sex)
#P(S) = 0.381971 <---- probability of surviing
#P(S|M) = 161/(161+682) <----probability of surviving given that male 0.1909846
#P(S|F) = 339/(339+127) <----probability of surviving given that male 0.7274678

#Odds(male)=0.1909846/(1-0.1909846)
#Odds(female)=0.7274678/(1-0.7274678)
#odds=Odds(male)/odds(female)=0.088...

prop.table(table(titanic$survived))

install.packages("caret")
install.packages("ROCR")
install.packages("caTools")


Index<-createDataPartition(titanic$survived,p=0.75,list=FALSE)
trainig<-titanic[Index,]
testing<-titanic[-Index,]
Model_New<-glm(survived~pclass+sex+age+sibsp+parch,data=trainig,family="binomial")
summary(Model_New)
print(cor(trainig[,c("age","sibsp","parch")]), digits = 4)
Model_New1<-glm(survived~pclass+sex+age+sibsp,data=trainig,family="binomial")
summary(Model_New1)
PredTest<-predict(Model_New1,newdata =testing,type="response")#Predict percentage of survival rate

summary(PredTest)
hist(PredTest)

table(Predicted=PredTest>0.5,Actual=testing$survived) #if more then 50% then survied class other case not
accuracy=0.7553517
sensit=83/(83+42)
sensit

specific=164/(164+38)
specific

Pred<-prediction(PredTest,testing$survived)
perf<-performance(Pred,"tpr","fpr") #tpr=sensitivity #fpr=1-sensitivity
plot(perf)

AUC<-performance(Pred,"auc")@y.values #INCHQAN LAVN MER MODEL@
AUC

#AUC shows that for maximal senitivity increase we have minimal specificity loss
