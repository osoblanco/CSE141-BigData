boston <- read.csv("Boston.csv")
names(boston)
summary(boston)
hist(boston$MEDV, xlab = "Price", main = "Price size distribution")

boxplot(MEDV~CHAS, data=boston)
boston$CHAS <- as.character(boston$CHAS)

set.seed(1982)
sub <- sample(nrow(boston),floor(nrow(boston) * 0.75))
training <- boston[sub,] #All rows from sub into training
testing <- boston[-sub,] #All rows that are NOT in sub, go into testing

GM <- lm(MEDV~.,data = training)
summary(GM)
cor(training[-4])
print(cor(training[-4]), digits = 3)



training1<-training

training1$NOX<-NULL
training1$DIS<-NULL
training1$TAX<-NULL
training1$RAD<-NULL
training1$AGE<-NULL
training1$ZN<-NULL





print(cor(training1[-3]), digits = 3)

model1<-lm(MEDV~.,data = training1)
summary(model1)
print(cor(training1[-3]), digits = 3)

Pred<-predict(model1, newdata = testing)
Pred
model1
RMSE<-sqrt(mean((testing$MEDV-Pred)^2))
RMSE

