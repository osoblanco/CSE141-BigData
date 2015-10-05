###Homework 2-Multiple Regression ####
##Your task is to built a model that will predict mpg - miles per gallon (dependent vaariable)
#by using a set of independent variables
#1. Load the datawuto-mpg into R
DatAauto <- read.csv("auto-mpg.csv");

#2. (2 points) The variable "origin" shows the coutnry of origin of the car and is stored as discrete numeric variable in R. 
#However, it is a nominal variable, with 1="American", 2="European", 3="Japan". Transfor the variable into factor variable

DatAauto$origin <- factor(DatAauto$origin, levels = c(1,2,3), labels=c("American","European","Japanese"))

#3. (1p) The variable car.name shows the names of each car. remove the variable from the dataframe
DatAauto$car.name<-NULL
#4. (1p) Run the correlation matrix for numeric variables. What are the top 3 variables 
#that has the highest correlation with dependent variable.
print(cor(DatAauto[-8]), digits = 4)
#weight,displacement,horsepower

##5. (2p)Create a scatterplot for the variables that has the least correlation (smallest two) with mpg.
plot(DatAauto$acceleration, DatAauto$model.year, xlab="Acceleration", ylab = "Model year", main = "Acceleration by year")
##If not linear, what kind of other relationship can you see?
#Actually it is linear!!!

##6. (1p) Create training and testing datasets (75% to Train dataset), dont forget to set the seed
set.seed(1997)
sub <- sample(nrow(DatAauto),floor(nrow(DatAauto) * 0.75))
training <- DatAauto[sub,] #All rows from sub into training
testing <- DatAauto[-sub,] #All rows that are NOT in sub, go into testing

##7. (4p) Test for multicollinearity, do you spot any (for test dataset)? 
#If so, report the variables with their respective correlation coefficients (4 digits after 0)
print(cor(training[-8]), digits = 4)
#8. (3p) Start the modeling on training dataset, if you have so, remove correlated variables. Explain your choice.

#Correlated (multicollinear) variables 

# cylinders~ displacement 0.9505
# cylinders~ horsepower 0.8416
# cylinders~ weight 0.8924
# displacement~horsepower 0.8962
# displacement~weight 0.9280
# horsepower~ weight 0.8580

#knowing that the greatest correlation to mpg is weight 0.8302
#and also seeing that is has a very strong correlation with displacement 0.9280
#we can remove displacement. Displacement is also strongly correlated to cylinders 0.9505
#thus we can also remove cylinders. The correlation of horespower with mpg and cylinders is also considered.


training1<-training
training1$displacement<-NULL
training1$cylinders<-NULL
print(cor(training1[-6]), digits = 3)

model1<-lm(mpg~.,data = training1)
summary(model1)

Pred<-predict(model1, newdata = testing)
Pred
model1

RMSE<-sqrt(mean((testing$mpg-Pred)^2))
RMSE

#8.1 (2p) Comment on the significance of the individual predictors (report p-values for each)

#horsepower - 0.604465
#weight - < 2e-16
#acceleration - 0.425476 
#model.year - < 2e-16 
#originEuropean - 2.41e-05
#originJapanese - 0.000238 

#8.2 (2p) Comment on the sign change problem, do you have any? What will you do with them?

#No sign change occurs. Hell yeah!!

#8.3 (2p) Report the R-squared and Rsquared adjusted values and interpret their meaning. 
##Which of the two metrics will you use?

#R-squared:  0.8273
#Adjusted R-squared:  0.8236

#R-squared show how much of the data varience our model is able to explain and predict

#Adjusted R-squared has the same concept as the normal R-squared just with a small difference
#that it penalizes for every variable that we choose to exclude.

#We choose  Adjusted R-squared as our general indicator for how well the model is!

#8.4 (2p)Show your final model, clearly show the steps you took and explain them.

#knowing that the greatest correlation to mpg is weight 0.8302
#and also seeing that is has a very strong correlation with displacement 0.9280
#we can remove displacement. Displacement is also strongly correlated to cylinders 0.9505
#thus we can also remove cylinders. The correlation of horespower with mpg and cylinders is also considered.
summary(model1)


#8.5 (4p) Comment on the explanatory power of your final model (R square adjusted), 
#overall significance (p-value for F test), significance of individual variables.

#We can see by adjusted R-squared that our model is able to explain 82.36% of the data varience.
#p-value: < 2.2e-16

#8.6 (2p) In non-statistical terms, explain the coefficients of the individual predictor variables

#For every additional horsepower unit MPG is decreased by -7.568e-03
#For every additional kilogram of weight MPG is decreased by -5.390e-03
#For every additional acceleration unit MPG is increased by 8.703e-02
#For every additional year of car model MPG is increased by 7.699e-01
#If the car is made in Europe the MPG is increased by 2.614
#If the car is made in Japan the MPG is increased by 2.196

#8.7 (7p) Lets say, at some point of model building, you had made a choice for one 
##model over another (for example while choosing to exclude one of two variables). 
##Create your alternative model-name it differently then your final one. Calculate RMSE value for testing dataset for both models. Which one is better now?

training2<-training
training2$displacement<-NULL
training2$horsepower<-NULL
training2$cylinders<-NULL
print(cor(training2[-5]), digits = 3)

model2<-lm(mpg~.,data = training2)
summary(model2)

Pred<-predict(model2, newdata = testing)
Pred
model2

RMSE<-sqrt(mean((testing$mpg-Pred)^2))
RMSE

#initial model
Pred1<-predict(model1, newdata = testing)
Pred1
model1

RMSE1<-sqrt(mean((testing$mpg-Pred1)^2))
RMSE1

install.packages("memisc")

library(memisc)
mtable(model1, model2)

##8.8 (No points, just 5star question) Think about what you could have done to increase the model's quality.

#could have played with colinear variable more...

