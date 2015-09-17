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
