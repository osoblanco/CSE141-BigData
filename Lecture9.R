##clustering
market<-read.csv("Market segmentation.csv")
mall.dist<-dist(market[,1:6], method="euclidian")
mall.cl<-hclust(mall.dist)    
plot(mall.cl, labels=FALSE)
rect.hclust(mall.cl,3) ##dividing into 3 clusteriks
market$cluster<-cutree(mall.cl,3) ##cluster membership variable
d1<-aggregate(market, by=list(market$cluster), FUN="mean")
d1
prop.table(table(market$cluster))
##ik means algo: k is the number of clusters
k<-kmeans(market[,1:6], 3)
names(k)
market$kmeans<-k$cluster
table(market$kmeans, market$cluster)
