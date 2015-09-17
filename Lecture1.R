x=5
y=7.5 #heavyLife
z=sqrt(x+y)
data("mtcars")
str(mtcars)
dim(mtcars)
length(mtcars)
nrow(mtcars)
ncol(mtcars)
head(mtcars,3)
tail(mtcars,5)

#creating new Vars (subsetting) from mtcars dataset
New<-mtcars
New<-mtcars[,1:3]
New1<-mtcars[1:20, 4:5]
New2<-mtcars[,c("mpg","hp","wt")]
str(New2)
names(New2)

#place Finding (row, column)
mtcars[[10]]
mtcars[[10,3]]
mtcars[3,10]
mtcars[10,3]

#Place Finding(min,max)
min(mtcars$mpg)
which.min(mtcars$mpg)
mtcars$mpg[[15]]

max(mtcars$hp)

which.min(mtcars$mpg) #index Finding
mtcars[15,]

#import new csv File
Census.csv<-read.csv("Census R.csv")

names(Census.csv)
str(Census.csv)
