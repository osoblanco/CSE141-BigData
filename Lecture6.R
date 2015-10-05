table1<-read.table("blood pressure.txt",header = TRUE)
View(table1)
table2=table1[,3:4] #removed first 2 columns
library("ggplot2")
qplot(table2$Age,table2$Pressure)
model<-lm(Pressure~Age,data=table2)
summary(model)
which.max(table2$Pressure)

table3<-table2[-2,]
model1<-lm(Pressure~Age,data=table3)
summary(model1)
