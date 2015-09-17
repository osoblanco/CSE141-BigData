install.packages("ggplot2")
library(ggplot2)
??qplot
data("mtcars")
str(mtcars)

#subsetting with if

New<-mtcars[mtcars$am==1,]
View(New)
New1<-mtcars[mtcars$mpg>19,]
View(New1)
New2<-mtcars[mtcars$am!=1,]
View(New2)
New3<-mtcars[mtcars$am ==1 & mtcars$mpg>19,] # c++ && = & in R

mtcars[order(mtcars$mpg, decreasing = TRUE),]

New4<-mtcars[order(mtcars$mpg, decreasing = FALSE),]
Census.csv<-read.csv("Census R.csv")
str(Census.csv)
Census.csv$sex<-factor(Census.csv$sex, levels = c(1,2),labels = c("Male","Female"))
Census.csv$happy<-ordered(Census.csv$happy, levels=c(1,2,3),labels=c("Very Happy", "Pretty Happy","Not too Happy"),exclude=c(8,9))
Census.csv$age[Census.csv$age==99]<-NA
summary(Census.csv$age)
read.table.url("https://docs.google.com/forms/d/1CtaIXkMapIupHUYDzaydaH-hRshFqp8SA8PylHEXExE/viewanalytics")
