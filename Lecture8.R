New <- read.csv("new.csv");
scaled<-scale(New[,-1])
scaled$customerID<-New$Customer.ID


#better way

New <- read.csv("new.csv");
scaled<-as.data.frame(scale(New[,-1]))
scaled$customerID<-New$Customer.ID
dist<-dist(scaled[-3],method="euclidian")
dist
hc<-hclust(dist)
plot(hc)
rect.hclust(hc,2)
