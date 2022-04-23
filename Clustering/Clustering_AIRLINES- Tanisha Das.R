
#K-Means Clustering Project
rm(list = ls())
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)



##############################################################################################
###### Build an analytical model to create clusters of airline travellers ####################
##############################################################################################

#---------------------Step1: Loading the Data in R
Path<-setwd("C:/Users/TANIA DAS/OneDrive/Desktop/Rivy")
airlines<-read.csv("AirlinesCluster.csv", header = TRUE, as.is = TRUE, na.strings = c(""))


#Understand the data type and summary of each coloumn
str(airlines)
summary(airlines)


#Checking missing values
as.data.frame(colSums(is.na(airlines)))

#Treating the outliers by using caping method
quantile(airlines$Balance,0.9)
quantile(airlines$BonusMiles,0.9)
quantile(airlines$BonusTrans,0.9)
quantile(airlines$FlightMiles,0.9)
quantile(airlines$FlightTrans,0.9)
quantile(airlines$DaysSinceEnroll,0.9)

airlines$Balance<-ifelse(airlines$Balance>164186.8,164186.8,airlines$Balance)
airlines$BonusMiles<-ifelse(airlines$BonusMiles>48529.6,48529.6,airlines$BonusMiles)
airlines$BonusTrans<-ifelse(airlines$BonusTrans>23,23,airlines$BonusTrans)
airlines$FlightMiles<-ifelse(airlines$FlightMiles>1246,1246,airlines$FlightMiles)
airlines$FlightTrans<-ifelse(airlines$FlightTrans>4,4,airlines$FlightTrans)
airlines$DaysSinceEnroll<-ifelse(airlines$DaysSinceEnroll>6982.8,6982.8,airlines$DaysSinceEnroll)

summary(airlines)

#Normalizing the Data for clustering 
library(caret)
preproc<-preProcess(airlines)
airlinesNorm<-predict(preproc,airlines)
summary(airlinesNorm)


#Hiearchical Clustering
distan<-dist(airlinesNorm, method = "euclidean")
ClusterAirline<-hclust(distan, method = "ward.D")
plot(ClusterAirline)


#Assigning points to the clusters
AirlineCluster<-cutree(ClusterAirline, k = 5)
table(AirlineCluster)


#Computing the average values of the cluster groups
MeanComp<-function(var, clustergrp, meas){
  z<-tapply(var, clustergrp, meas)
  print(z)
}

Bal_mean<-MeanComp(airlines$Balance, AirlineCluster, mean)
Bal_DaysSinceEnroll<-MeanComp(airlines$DaysSinceEnroll, AirlineCluster, mean)
Bonus_Miles<-MeanComp(airlines$BonusMiles, AirlineCluster, mean)
Bonus_Trans<-MeanComp(airlines$BonusTrans, AirlineCluster, mean)
Flight_Miles<-MeanComp(airlines$FlightMiles, AirlineCluster, mean)
Flight_Trans<-MeanComp(airlines$FlightTrans, AirlineCluster, mean)
Qual_Miles<-MeanComp(airlines$QualMiles, AirlineCluster, mean)
#Appending the Clusters Assignment
Airlines_H<-data.frame(airlines,AirlineCluster)
write.csv(Airlines_H,"Airlines_Hierarchical.csv", row.names = FALSE)

######################################################################

#k-Means Clustersing
set.seed(88)

#Finding and Visualizing out the various clusters
AirlineCluster_K1<-kmeans(airlinesNorm, centers = 4,iter.max = 1000)
AirlineCluster_K2<-kmeans(airlinesNorm, centers = 5,iter.max = 1000)
AirlineCluster_K3<-kmeans(airlinesNorm, centers = 6,iter.max = 1000)
AirlineCluster_K4<-kmeans(airlinesNorm, centers = 7,iter.max = 1000)


# plots to compare
p1 <- fviz_cluster(AirlineCluster_K1, geom = "point", data = airlinesNorm) + ggtitle("k = 4")
p2 <- fviz_cluster(AirlineCluster_K2, geom = "point",  data = airlinesNorm) + ggtitle("k = 5")
p3 <- fviz_cluster(AirlineCluster_K3, geom = "point",  data = airlinesNorm) + ggtitle("k = 6")
p4 <- fviz_cluster(AirlineCluster_K4, geom = "point",  data = airlinesNorm) + ggtitle("k = 7")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Determing optimal numbers of clusters using the Elbow Method
set.seed(123)
fviz_nbclust(airlinesNorm, kmeans, method = "wss")

k<-5
AirlineCluster_K<-kmeans(airlinesNorm, centers = k,iter.max = 1000)
AirlineCluster_K



table(AirlineCluster_K$cluster)
AirlineCluster_K$centers
fviz_cluster(AirlineCluster_K, data = airlinesNorm)



#Finding out the Mean Values of the Variables in the Clusters
Bal_mean_k<-aggregate(airlines, by=list(cluster=AirlineCluster_K$cluster), mean)


#Visualization
library(ggplot2)
bal<-ggplot(Bal_mean_k, aes(x=cluster, y=Balance)) +
  geom_bar(stat="identity",fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
bal

qual<-ggplot(Bal_mean_k, aes(x=cluster, y=QualMiles)) +
  geom_bar(stat="identity", fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
qual

bonus<-ggplot(Bal_mean_k, aes(x=cluster, y=BonusMiles)) +
  geom_bar(stat="identity", fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
bonus

bonust<-ggplot(Bal_mean_k, aes(x=cluster, y=BonusTrans)) +
  geom_bar(stat="identity", fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
bonust

flightt<-ggplot(Bal_mean_k, aes(x=cluster, y=FlightTrans)) +
  geom_bar(stat="identity", fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
flightt

flight<-ggplot(Bal_mean_k, aes(x=cluster, y=FlightMiles)) +
  geom_bar(stat="identity", fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
flight

days<-ggplot(Bal_mean_k, aes(x=cluster, y=DaysSinceEnroll)) +
  geom_bar(stat="identity", fill=c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0","#A52A2A"))
days

#Appending the Clusters Assignment
airlines_new_k <- data.frame(airlines, AirlineCluster_K$cluster)
write.csv(airlines_new_k,"Airlines_k-Means.csv", row.names = FALSE)
