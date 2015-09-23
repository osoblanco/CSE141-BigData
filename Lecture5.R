college.csv<-read.csv("College.csv")
str(college.csv)
names(college.csv)
college.csv$University
rownames(college.csv)<-college.csv[,1]
View(college.csv)
college.csv$University=NULL
plot(college.csv$Accept, college.csv$Apps,ylab = "Applcations",xlab = "Accepted Applicants")

cor(college.csv$Accept, college.csv$Apps)#correlation between 2 variables

pairs(~Apps+Accept+Enroll, data=college.csv) #Pairwise Plots each pairs of variables in all ways possible

cor(college.csv[,c("Apps","Accept","Enroll")]) #combined corelation matrix between all of them
cor(college.csv[,c("Apps","Outstate","Books","Grad.Rate","Room.Board","Personal")])
cor(college.csv[,-1])

View(college.csv)
college.csv$University=NULL
plot(college.csv$Accept, college.csv$Apps)

t.test(college.csv$Apps~college.csv$Private)

t.test(college.csv$Personal~college.csv$Private)
t.test(college.csv$Terminal~college.csv$Private)
t.test(college.csv$Terminal~college.csv$Private, alternative="less")
t.test(college.csv$Terminal~college.csv$Private, alternative="greater")

data(mtcars)
my<-mtcars
t.test(my$mpg~mtcars$am)
t.test(my$wt~mtcars$am, alternative="less")
t.test(my$wt~mtcars$am)

t.test(my$hp~my$am)

titanic<-read.csv("Titanic_imputed.csv")
str(titanic)
titanic$survived=factor(titanic$survived, level=c(0,1), labels=c("Oxormi","Abris"))

t<-table(titanic$survived,titanic$sex)
t1<-table(titanic$sex,titanic$pclass)

chisq.test(t)
chisq.test(t1)

