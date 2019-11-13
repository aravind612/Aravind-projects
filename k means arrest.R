View(USArrests)
arrests=USArrests
str(arrests)
summary(arrests)

#data manipulation
install.packages('tidyverse')
install.packages('cluster')
install.packages('factoextra')
library(tidyverse)
library(cluster)
library(factoextra)

#splitting
library(caTools)
sample_arrest=sample.split(arrests,SplitRatio = 0.8)
sample_arrest
train_arrest=subset(arrests,sample_arrest=='TRUE')
train_arrest=scale(train_arrest)
train_arrest
test_arrest=subset(arrests,sample_arrest=='FALSE')
test_arrest=scale(test_arrest)
test_arrest
#model
k_means_arrest=kmeans(train_arrest,centers = 2,nstart = 25)
k_means_arrest
fviz_cluster(k_means_arrest,data = train_arrest)

#different groups
k_means_arrest_1=kmeans(train_arrest,centers = 3,nstart = 25)
k_means_arrest_2=kmeans(train_arrest,centers = 4,nstart = 25)
k_means_arrest_3=kmeans(train_arrest,centers = 5,nstart = 25)

#plot to compare
p1=fviz_cluster(k_means_arrest,geom='points',data = train_arrest)+ggtitle('K=2')
p2=fviz_cluster(k_means_arrest_1,geom = 'points',data=train_arrest)+ggtitle('K=3')
p3=fviz_cluster(k_means_arrest_2,geom = 'points',data=train_arrest)+ggtitle('K=4')
p4=fviz_cluster(k_means_arrest_3,geom = 'points',data=train_arrest)+ggtitle('K=5')

#displaying all graphs
library(gridExtra)
grid.arrange(p1,p2,p3,p4,nrow=2)

#elbow technique
elbow=function(k){
kmeans(train_arrest,k,nstart = 10)$tot.withinss
}
#k=1 to k=15
k.values=c(1:15)
elbow_values=map_dbl(k.values,elbow)
plot(k.values,elbow_values,type='b',xlab='Number of clusters k',ylab='Total withiness')
