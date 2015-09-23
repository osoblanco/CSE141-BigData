#####Homework 1#######
#1. Code line should be written under its question
#2. Whenever you need to give interpretation 
#(mean, stdev, result of the hypothesis test) write the interpretations
#under the question
#The homework is worth of 35 points
#submit your R code to moodle

#1. Read the dataset bank-full.csv
bank_full<-read.csv("bank-full.csv")

##2. How many variables do you have? How many cases do you have? (1 point)

nrow(bank_full) #number of cases=45211
ncol(bank_full) # number of variables =17

##3. make a random sample of 20,000 cases, for the further analysis use this dataset (1 point)

set.seed(0007)
bank_samp<-bank_full[sample(nrow(bank_full),20000),]

#4. Give summary statistics for the variable Age (1 point)
summary(bank_samp$age)
sd(bank_samp$age) #standard Deviation

##Minimum - 18.00
##Maximum - 94.00
##Median - 39.00
## stdev  - 10.56646

##5. The variable education shows the highest education level that a customer has 
##(categorical variable). The variable balance is the average yearly balance of 
##the customer. Find the average balance for each category for the education. (2 points)
aggregate(bank_samp$balance, by=list(bank_samp$education), FUN="mean", na.rm=TRUE)

#Fill the values
#Primary - 1232.377
#secondary - 1146.512
#tertiary - 1702.478
#Unknown - 1672.252

##6. Find the sdandard deviation of the variable balance for each category 
## of education (3 points)
## hint - (look at the argument FUN)
aggregate(bank_samp$balance, by=list(bank_samp$education), FUN="sd", na.rm=TRUE)

#Primary - 2679.933
#Secondary - 2608.905
#tertirary - 3644.386
#unknown - 3217.068

##7.  The variable marital shows the marital status of the respondent
#what is the most frequent category for this variable? (2 points)
table(bank_samp$marital)

## The most frequent category is __MARRIED____ with the frequency of ___12070__

##8. Create frequency (contingency) table for the variables marital and loan. (2 points)
table(bank_samp$marital, bank_samp$loan)

##9.  What percentage of maried customers have a loan? (3 points)
prop.table(table(bank_samp$marital,bank_samp$loan),1)
## % of married that have a loan = 17.08368%

##10. From those who have a loan, what percentage are married people? (3 points)
prop.table(table(bank_samp$marital,bank_samp$loan),2)
## % of those who have loan, married are  = 65.28117%

##11.  Create a bar chart for the variable marital. Be sure that it is a "good graph".
## The bars in the chart should be colored in blue. Hint: use argument "col". (2 points)
barplot(table(bank_samp$marital), xlab="Marital Status", ylab="Frequences", main="Marital distribution", col="blue", border=NA)

##11. Create histogram for the variable balance. Use both qplot and hist functions.
##Which chart looks better? can you make the histogram more readable? 
##(hint: play with xlim, ylim and bin lenghts). Be sure to make  a good graph. (4 points)

hist(bank_samp$balance,xlab = "Balance",main="Balance Distribution",freq = FALSE)
library("ggplot2")
qplot(balance,data=bank_samp,geom="histogram",binwidth=0.2,xlim = c(0,200),ylim = c(0,100))
qplot(balance,data=bank_samp,geom="histogram",binwidth=3,xlim = c(0,2000),ylim = c(0,100))

##Interpret the histogram in your words (hint: is it skewed?)
#The Graph is obviously skewed to right.The Greater The Balance the smaller the Count a.k.a Frequency.
# Shat balansov Mard gnalov aveli qich ka

##12. You need to explore a little capacities of qplot. 
##Create one graph on your own, that gives an interesting insight about the 
##variables (4 points)

qplot(education,balance,data=bank_samp,geom="jitter",stat="identity")
qplot(education,age,data=bank_samp,geom="jitter",stat="identity")


####Inferential Statistics#####
## 13. Is there a relationship between default and balance? (2 points)
chisq.test(table(bank_samp$default, bank_samp$balance))

##Interpret the results of the hypothesis testing
#The variables Default and Balance are obviously related due to the fact that p we got is 
# 2.2*10^(-16) meaning that it is sufficiently small to reject the null hypothesis.

##14. Have a look at the variables loan and housing. Are they independent? (2 points)
chisq.test(table(bank_samp$loan, bank_samp$housing))


##Interpret the results
#The variables Loan and Housing are also  related due to the fact that p we got is sufficently minute 
# 2.2*10^(-16) meaning that we can reject the null hypothesis.

##15. Look at the correlation coefficients of variable campaign and all other 
##numerical variables.  
cor(bank_samp[,c("campaign","age", "balance", "day", "duration", "pdays", "previous")])
##What are two variables that have the strongest correlation with Age. (3 points)

# The two strongest correlations found to variable Age are the Pdays and Balance
