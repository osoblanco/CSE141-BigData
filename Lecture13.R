data("iris")
str(iris)
View(iris)
table(iris$Species)
iris1<-iris[iris$Species!="virginica",];
library(ggplot2)
plot<-qplot(iris1$Sepal.Width,iris1$Sepal.Length,colour=iris1$Species,main = "ScatterPlot of Sepal width and Sepal length");
plot
plot+geom_abline(intercept=2.15,slope=1.05,size=1)

plot1<-qplot(iris1$Petal.Width,iris1$Petal.Length,colour=iris1$Species,main = "ScatterPlot of Sepal width and Sepal length");
plot1
plot1+geom_abline(intercept=3.5,slope=-1.05,size=1)

plot2<-qplot(iris$Sepal.Width,iris$Petal.Length,colour=iris$Species,main = "ScatterPlot of Sepal width and Sepal length");
plot2

library(MASS)
lda.iris<-lda(Species~.,data = iris)
lda.iris #Proportion of trace shows the importance of discriminant. Meaning how much it explains the varience.

x<-predict(lda.iris)
names(x)
table(iris$Species,x$class) #confusion matrix for the reality vs prediction
x$x


LD1<-x$x[,1]
LD2<-x$x[,2]

qplot(LD1,LD2,color=iris$Species,size=I(3))
