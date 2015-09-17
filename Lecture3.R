#Creating Vectors
x <- c(155,160,171,182,162,153,190,167,168,165,191)
min(x)
max(x)
mean(x)
median(x)
mod(x)
mode(x)
sd(x)
var(x)
Census.csv<-read.csv("Census R.csv")
str(Census.csv)
summary(Census.csv)
view(Census.csv)
Census.csv$sex<-factor(Census.csv$sex, levels = c(1,2),labels = c("Male","Female"))
Census.csv$happy<-ordered(Census.csv$happy, levels=c(1,2,3),labels=c("Very Happy", "Pretty Happy","Not too Happy"),exclude=c(8,9))
Census.csv$age[Census.csv$age==99]<-NA
summary(Census.csv$age)
scan(Census.csv$age)
mean(Census.csv$age, na.rm = TRUE)
sd(Census.csv$age, na.rm = TRUE)
names(Census.csv)

#helps to combine and filter the file under a function
aggregate(Census.csv$age, by=list(Census.csv$sex), FUN="mean", na.rm=TRUE)
aggregate(Census.csv[,c("age","sibs")],by=list(Census.csv$sex), FUN="mean", na.rm=TRUE)
aggregate(Census.csv[,c("age","sibs")],by=list(Census.csv$sex, Census.csv$region), FUN="mean", na.rm=TRUE)

#tables
table(Census.csv$sex)
table(Census.csv$sex,Census.csv$region)
table(Census.csv$sex,Census.csv$region,Census.csv$wrkstat)
my.table<-table(Census.csv$sex,Census.csv$region)
my.table
prop.table(my.table)   # percentage in the whole world population
prop.table(my.table,1) # percentage in the whole MALE/FEMALE population
prop.table(my.table,2) # percentage in the REGIONS population
