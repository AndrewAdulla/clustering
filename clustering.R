seedsdata= read.csv("C:/R_data/seeds_dataset(1).csv", sep=",")
seedsreal =read.csv("C:/R_data/seeds_real.csv", sep=",")

source("C:/Users/Andrew/Downloads/WK_R.R")

mydata<-seedsdata
plot(mydata)

#mydata<-seedsdata

#preparing data
mydata<-scale(mydata[,-8])

#kmeans clustering
fitk<- kmeans(mydata, 3)
fitk
str(fitk)
plot(mydata, col=fitk$cluster)#extracting the clustering

#choosing k
k<- list()
  for (i in 1:10) {
   k[[i]]<-kmeans(mydata, i)
  
}

k

#visualizing k 
betweenss_totss<- list()
 for (i in 1:10){
   betweenss_totss[[i]]<- k[[i]]$betweenss/k[[i]]$totss
 }

plot(1:10, betweenss_totss, type= "b", ylab = "between ss/ total SS", xlab ="cluster(k)")

#k diff algorithms
for(i in 1:4){
  plot(mydata, col = k[[i]]$cluster)
  
}
#using diff value for k=9
fitk1<- kmeans(mydata, 9)
fitk1
str(fitk1)
plot(mydata, col=fitk1$cluster)

#k=4
fitk2<- kmeans(mydata, 4)
fitk2
str(fitk2)
plot(mydata, col=fitk2$cluster)

#Weighted Kappa

wkval <- c()

#k=3
WK =WK_R(fitk$cluster, fitk1$cluster)
print(WK)
wkval<- c(wkval, WK)

#second k=4
WK1 =WK_R(fitk1$cluster,fitk2$cluster)
print(WK1)
wkval<- c(wkval, WK1)

#k=9
WK2 =WK_R(fitk$cluster, fitk2$cluster)
print(WK2)
wkval<- c(wkval, WK2)

plot(wkval)


#hierarchical clustering
d<- dist(mydata)
fitH<-hclust(d, "complete")
plot(fitH)
rect.hclust(fitH, k=3, border = "red")
clusters1<- cutree(fitH, 3)


plot(mydata, col =clusters1)

#using single
d<- dist(mydata)
fitH1<-hclust(d, "single")
plot(fitH1)
rect.hclust(fitH1, k=3, border = "red")
clusters2<- cutree(fitH1, 3)


plot(mydata, col =clusters2)

#using average
d<- dist(mydata)
fitH21<-hclust(d, "average")
plot(fitH21)
rect.hclust(fitH21, k=3, border = "red")
clusters3<- cutree(fitH21, 3)


plot(mydata, col =clusters3)

#k=4
d<- dist(mydata)
fitH2<-hclust(d, "complete")
plot(fitH2)
rect.hclust(fitH2, k=4, border = "red")
clusters4<- cutree(fitH2, 4)


plot(mydata, col =clusters4)

#k=9
d<- dist(mydata)
fitH3<-hclust(d, "complete")
plot(fitH3)
rect.hclust(fitH3, k=9, border = "red")
clusters5<- cutree(fitH3, 9)


plot(mydata, col =clusters5)

#Weighted Kappa
source("C:/Users/Andrew/Downloads/WK_R.R")

#k=3
wkval1 <- c()
WKH =WK_R(fitk$cluster,clusters1)
wkval1 <- c(wkval1,WKH)

#second k=4
WKH1 =WK_R(fitk1$cluster,clusters2)
wkval1 <- c(wkval1,WKH1)

#second k =9
WKH2 =WK_R(fitk$cluster, clusters4)
wkval1 <-c(wkval1,WKH2)

plot(wkval1)

