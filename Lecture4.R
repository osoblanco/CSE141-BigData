Census_csv<-read.csv("Census R.csv")
gender.dist<-table(Census_csv$sex)
barplot(gender.dist)

barplot(gender.dist, xlab="Gender Category", ylab="Frequences", main="Gender distribution", col="red", border=NA)
data(mtcars)
str(mtcars)
plot(mtcars$mpg, mtcars$wt)
plot(mtcars$mpg, mtcars$wt,xlab = "Miles Per Gallon", ylab = "Weight", main="Weight/MPG",type="b") 
#xlab- Nerqevi anun #ylab - zaxi Anun 
# type="a" sovorakan ketikner #type="b" gtsiknerov kptsnuma

hist(Census_csv$age,xlab = "Age",main="Age Distribution",freq = FALSE)
library("ggplot2")
data(diamonds)
str(diamonds)
set.seed(0007)
dSmall<-diamonds[sample(nrow(diamonds),200),]
nrow(dSmall)
qplot(carat,price, data=dSmall)
qplot(log(carat),log(price),data=dSmall)
qplot(carat, price,data=dSmall,colour=color) #different colors of diamonds haev differnt colours on graph
qplot(carat, price,data=dSmall,shape=cut)
qplot(carat, price,data=dSmall,shape=clarity)
qplot(color, price/carat,data=dSmall,geom="boxplot")

qplot(price,data=dSmall,geom="histogram")
qplot(carat,data=dSmall,geom="histogram",binwidth=0.1)
qplot(carat,data=dSmall,geom="histogram",binwidth=0.2,xlim = c(0,3))

qplot(price,data=diamonds,geom="density")


