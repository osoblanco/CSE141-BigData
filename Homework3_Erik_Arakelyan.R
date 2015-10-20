#####Homework 3#####
#The Global Competitiveness Report 2014-2015 assesses the competitiveness 
#landscape of 144 economies, providing insight into the drivers of their 
#productivity and prosperity. The report remains the most comprehensive 
#assessment of national competitiveness worldwide, providing a platform for 
#dialogue between government, business and civil society about the actions 
#required to improve economic prosperity. Competitiveness is defined as the 
#set of institutions, policies and factors that determine the level of 
#productivity of a country. The level of productivity, in turn, sets the 
#level of prosperity that can be earned by an economy.
#Visit http://www.weforum.org/reports/global-competitiveness-report-2014-2015 for more details

#The different aspects of competitiveness are captured in 12 pillars, 
#which compose the Global Competitiveness Index. The value for pillar for each country is a 
#number between 1-7 and is measured based on the set of sub-indicators (also measured by the scale 1-7)
# Your task is to create clusters of countries based on the pillars of competitivness.

#1. Load the dataset GCI into R#
GCI<-read.csv("GCI.csv")

#2. (2p) Make the variable Country.code as rownames for the dataframe 
#(hint: use rowname() command)
row.names(GCI) <- GCI$Country.code
#3. (2p) Make the summary of the dataset
summary(GCI)
#4. (2p) Run hierarchical clustering on the dataset, using 12 pillars as clustering variables
GCI$Country.code<-NULL
scaled<-as.data.frame(scale(GCI[,-1]))
dist<-dist(scaled[-1],method="euclidian")
hc<-hclust(dist)
plot(hc)
#5. (8p) Plot the dendogram. What you think is the optimal number of clusters? Try several options

#install.packages("NbClust")
#library(NbClust)
#NbClust(data = GCI,diss = NULL, distance = "euclidean", min.nc = 3, max.nc = 15,method = "complete", index = "all", alphaBeale = 0.1)

#lets try going by the powers of 2 and see
rect.hclust(hc,2)
rect.hclust(hc,4)
rect.hclust(hc,8)
rect.hclust(hc,16)
rect.hclust(hc,32)
rect.hclust(hc,64)

#I see that the optimal number lies withing the 32 and 64 because by my reasoning the countries
#that are clustered togeter are quite obviously (even regionaly) connected
# however 32 is not sufficent and 64 is too much thus lets take the middle number between them
# (32+64/2=48 

rect.hclust(hc,48) # this is the real command use this one

#5.1 (2p) Find the centers of the clusters, describe what are 
###the differnces between the clusters in terms of differences in  means 
GCI$cluster<-cutree(hc,48) ##cluster membership variable
d1<-aggregate(GCI[,2:13], by=list(GCI$cluster), FUN="mean")
summary(d1)
View(d1)

#Here we calculate the means of each cluster while simmultaneously computing the ditance
# of each means with respect to eachother.

#5.2 (2p) How will you describe your clusters? try to give them names

#This cluster combines lots of countries by their geographical origin and fiancial technological and economialc potential

# Lets Look up the 28'th cluster which includes NOR, DNK(Denmark),LUX, SWE,QAT, ARE 
# This are mainly nordic countries except for two, however all of this countries are alike
#in their foreign investment strategies and revenue generation. (Refer FT)
#Lets call them (Nordlike Investors)


# Lets Look up the 40'th cluster which includes USA, GBR, JPN. 
#This are post modernized and post industrialized countries
#that are one the top of world technological, inovative and finacial force.
#Lets call them (The Dream Team)

#Lets Look up the 42'nd cluster which only includes KOR(south Korea)
#It is quite natural because Korea is indeed quite unique and unparralled
#in the gloabal perspective. Lets call it (isolated  mysterious Asians)

#Lets Look up the 29'th cluster which includes GUY, ZMB, GHA, DOM, HND
#This are African and American (also Carribean pool) countries which
#share  a same kind of history and development (economical,political) patterns.
#Lets call them (Afro-Americans)

#Lets Look up the the 23'rd cluster which include RUS, CHN.
#This countries share a political and financial bond and sociliast-like(tyranistic) regimes
#Lets call them (Soyuz Nerushimiy)

#the other clusters are also combined quite alike mostly by their geographical,
#historical, financial similariteis.

#5.3 (3p) Create a boxplot for the means of the pillar(factor) that 
##you think makes the biggest difference between clusters: 
summary(d1)
View(d1)
#We clearly see from these 2 that the Pillar with gratest range in it and
#the greatest varience is the 10th pillar (distance from mean to Min and Max)...thus it makes the biggest difference 
boxplot(GCI$Pillar.10)
#6. (4p) Run K-means algorithm, with the same number of clusters, are you getting the same results?
#Now choose one of the methods and continue with that.
k<-kmeans(GCI[,2:13], 48)
names(k)
GCI$kmeans<-k$cluster
table(row.names(GCI), GCI$kmeans)

#the numbers(ID's) of clusters are changed however the contents of the clusters remain
#fairly alike. The clusters are not all in all perfectly the same, however big 
#similarities can be observed.

#The dataset WDI indicators has some social and economic data on the countries included in GCI study
#Note the WDI dataset has the same order as GCI, so you can easly add cluster variable to the WDI dataset
#8 Read the dataset into R
WDI<-read.csv("WDI indicators.csv")
WDI$cluster<-GCI$cluster
#8.1 (4p) Look at the Life expectancy - what are the min and max values for each cluster?
#what does this info tell you about the clusters?

ClustMin <- aggregate(WDI$SP.DYN.LE00.IN, by = list(WDI$cluster), FUN="min", na.rm = TRUE)
ClustMax <- aggregate(WDI$SP.DYN.LE00.IN, by = list(WDI$cluster), FUN="max", na.rm = TRUE)
summary(ClustMin)
summary(ClustMax)
View(ClustMin)
View(ClustMax)
#Facts

#we can see from the summaries and overall that
#1.The rates in  cluster 16 that is in BFA,MOZ are low and the average male doesnt survive until 53
#2 The Nigerians which are also a cluster (43) are the lowest life excectancy country
#3 The Post soviet countries share almost the same age boundries at 75+-4
#4 In scandinavian countries + Luxemburg + Qatar the life expectancy is from 77-81
#5 Yemen is a unique country representing the median place between EUROPE and AFRICA 


#8.2 (6p) Describe your clusters using indicators from the WDI dataset: Note, the meanings of the variables are given in seperate file
#you need to do a small research, if you want to understand more about these variables
#Try to give description of each cluster in 2-3 sentences

ClustUneploymentMean <- aggregate(WDI$SL.UEM.TOTL.ZS, by = list(WDI$cluster), FUN="mean", na.rm = TRUE)
ClustGDPPerCapitaMean <-aggregate(WDI$NY.GDP.PCAP.CD, by = list(WDI$cluster), FUN="mean", na.rm = TRUE)
summary(ClustGDPPerCapitaMean)
summary(ClustUneploymentMean)
View(ClustGDPPerCapitaMean)
View(ClustUneploymentMean)

#We can see that our famous cluster 28 (Nordlike investors) are having a great time 
# with the unemployment rates which are almost the minumum of all the clusters and their
#GDP per capta is the higest among all of the clusters. The economical infostructure is efficeint
#and flourishing.


#Lets Look up the 40th cluster (The Dream Team). Although they are quite developed in their infostructure
#the unemployment rates are quite average in the scale of post industrial countries yet are impressive in 
#comparison to the whole world 6,7~=6,3<9,126. The  overal GDP is in TOP 3 in the dateset.

#Lets look at 23rd cluster (Souys Nerushimiy) Although they have quite low unimployment rates that
#are in TOP 5 of the overal dataset teir GDP Per capita Bellow the average of the datestet.
#Show lower production or cheaper production rates in this countreis.

#Lets look the 29th Cluster (Afro-Americans) Although the uneployment rates in this countries 
#are quite average the GDP per capita almost one of the lowest in the datest. This speaks both
#of low and cheap productios.
