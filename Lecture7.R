#Lecture 7

abalone <- read.csv("Abalone.csv")
names(abalone)
summary(abalone)
hist(abalone$rings, xlab = "Ring size", main = "Ring size distribution")

#See if means of rings are different by gender
boxplot(rings~sex, data=abalone)
#...they're not, that's why we combine Male and Female categories (mean Infant age is slightly lower)

#Transform factor variable into character variable
abalone$sex <- as.character(abalone$sex)

#Create new label "NI" - Non Infant, when label is M or F
abalone$sex[abalone$sex != 'I'] <- 'NI'

#Now transform character variable back into factor
abalone$sex <- as.factor(abalone$sex)


#Now separate the dataset into Training and Testing (75% / 25%)
set.seed(8492)
sub <- sample(nrow(abalone),floor(nrow(abalone) * 0.75))
training <- abalone[sub,] #All rows from sub into training
testing <- abalone[-sub,] #All rows that are NOT in sub, go into testing

#Begin building a linear regression model
#1) Take a look at colinearity
#2) Remove correlated variables
GM <- lm(rings~.,data = training)
summary(GM)
cor(training[-1])
print(cor(training[-1]), digits = 3)

training1<-training
training1$length<-NULL
print(cor(training1[-1]), digits = 3)

model1<-lm(rings~.,data = training)
summary(model1)

training2<-training1
training2$weight.w<-NULL
training2$weight.w<-NULL
training2$weight.s<-NULL
training2$weight.v<-NULL
training2$diameter<-NULL
print(cor(training2[-1]), digits = 3)
model2<-lm(rings~.,data = training2)
summary(model2)


Pred<-predict(model2, newdata = testing)
Pred
model2

RMSE<-sqrt(mean((testing$rings-Pred)^2))
RMSE
