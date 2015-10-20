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
