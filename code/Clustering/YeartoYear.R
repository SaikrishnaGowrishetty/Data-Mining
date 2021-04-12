x<-read.table("hourly_2007.g",header=TRUE,skip=1);
dim(x);
library(dplyr)
library(amap)
library(tidyr)
y<- subset(x, select=c(1,3,4,5,9,13));
dim(y);
y[1,];
x[1,];
colnames(y)<-c("station","date","temp","dewpoint","stationpressure","windspeed");
head(y);
y1<-y
y1<-separate(y1,date,c("d","hours"));
y1
y1<-transform(y1, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8))
y1 <- subset(y1, Month == "04")
y1
which(y1$windspeed==9999.9)
y1 <- y1 %>%
  mutate(temp = replace(temp, temp == 9999.9, NA));
y1 <- y1 %>%
  mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, NA));
y1 <- y1 %>%
  mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, NA));
y1 <- y1 %>%
  mutate(windspeed = replace(windspeed, windspeed == 999.9, NA));
b<-group_by(y1,station)
avg<-summarize(b,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg$sp_avg[is.nan(avg$sp_avg)]<-NA
avg$sp_avg[is.na(avg$sp_avg)]<-mean(avg$sp_avg,na.rm=TRUE)
avg$temp_avg[is.nan(avg$temp_avg)]<-NA
avg$temp_avg[is.na(avg$temp_avg)]<-mean(avg$temp_avg,na.rm=TRUE)
avg$dp_avg[is.nan(avg$dp_avg)]<-NA
avg$dp_avg[is.na(avg$dp_avg)]<-mean(avg$dp_avg,na.rm=TRUE)
avg$ws_avg[is.nan(avg$ws_avg)]<-NA
avg$ws_avg[is.na(avg$ws_avg)]<-mean(avg$ws_avg,na.rm=TRUE)
avg1<-avg



x1<-read.table("hourly_2008.g",header=TRUE,skip=1);
dim(x1);
library(dplyr)
library(amap)
library(tidyr)
y1<- subset(x1, select=c(1,3,4,5,9,13));
dim(y1);
y1[1,];
x1[1,];
colnames(y1)<-c("station","date","temp","dewpoint","stationpressure","windspeed");
head(y1);
y11<-y1
y11<-separate(y11,date,c("d","hours"));
y11
y11<-transform(y11, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8))
y11 <- subset(y11, Month == "04")
y11
which(y11$windspeed==9999.9)
y11 <- y11 %>%
  mutate(temp = replace(temp, temp == 9999.9, NA));
y11 <- y11 %>%
  mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, NA));
y11 <- y11 %>%
  mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, NA));
y11 <- y11 %>%
  mutate(windspeed = replace(windspeed, windspeed == 999.9, NA));
y11
b1<-group_by(y11,station)
u1 <- c(unique(y11[,1]))
u1

avg1<-summarize(b1,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg1$sp_avg[is.nan(avg1$sp_avg)]<-NA
avg1$sp_avg[is.na(avg1$sp_avg)]<-mean(avg1$sp_avg,na.rm=TRUE)
avg1$temp_avg[is.nan(avg1$temp_avg)]<-NA
avg1$temp_avg[is.na(avg1$temp_avg)]<-mean(avg1$temp_avg,na.rm=TRUE)
avg1$dp_avg[is.nan(avg1$dp_avg)]<-NA
avg1$dp_avg[is.na(avg1$dp_avg)]<-mean(avg1$dp_avg,na.rm=TRUE)
avg1$ws_avg[is.nan(avg1$ws_avg)]<-NA
avg1$ws_avg[is.na(avg1$ws_avg)]<-mean(avg1$ws_avg,na.rm=TRUE)
avg11<-avg1




x2<-read.table("hourly_2009.g",header=TRUE,skip=1);
dim(x2);
library(dplyr)
library(amap)
library(tidyr)
y2<- subset(x2, select=c(1,3,4,5,9,13));
dim(y2);
y2[1,];
x2[1,];
colnames(y2)<-c("station","date","temp","dewpoint","stationpressure","windspeed");
head(y2);
y12<-y2
y12<-separate(y12,date,c("d","hours"));
y12
y12<-transform(y12, Year = substr(d, 1, 4), Month = substr(d, 5, 6),day = substr(d,7,8))
y12 <- subset(y12, Month == "04")
y12
which(y12$windspeed==9999.9)
y12 <- y12 %>%
  mutate(temp = replace(temp, temp == 9999.9, NA));
y12 <- y12 %>%
  mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, NA));
y12 <- y12 %>%
  mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, NA));
y12 <- y12 %>%
  mutate(windspeed = replace(windspeed, windspeed == 999.9, NA));
b2<-group_by(y12,station)
avg2<-summarize(b2,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))
avg2$sp_avg[is.nan(avg2$sp_avg)]<-NA
avg2$sp_avg[is.na(avg2$sp_avg)]<-mean(avg2$sp_avg,na.rm=TRUE)
avg2$temp_avg[is.nan(avg2$temp_avg)]<-NA
avg2$temp_avg[is.na(avg2$temp_avg)]<-mean(avg2$temp_avg,na.rm=TRUE)
avg2$dp_avg[is.nan(avg2$dp_avg)]<-NA
avg2$dp_avg[is.na(avg2$dp_avg)]<-mean(avg2$dp_avg,na.rm=TRUE)
avg2$ws_avg[is.nan(avg2$ws_avg)]<-NA
avg2$ws_avg[is.na(avg2$ws_avg)]<-mean(avg2$ws_avg,na.rm=TRUE)
avg12 <- avg2



avg
avg11
avg12

common1 <- intersect(avg$station, avg11$station)  
common<- intersect(avg12$station,common1)
View(common)

avg2007<-subset(avg, station %in% common)
avg2007[,2:5]<-scale(avg2007[,2:5])
avg2008<-subset(avg11, station %in% common)
avg2008[,2:5]<-scale(avg2008[,2:5])
avg2009<-subset(avg12, station %in% common)
avg2009[,2:5]<-scale(avg2009[,2:5])

set.seed(10)
result2007e10<-Kmeans(avg2007[,2:5],2, iter.max=30,method = "euclidean")
result2008e10<-Kmeans(avg2008[,2:5],2, iter.max=30,method = "euclidean")
result2009e10<-Kmeans(avg2009[,2:5],2, iter.max=30,method = "euclidean")
set.seed(50)
result2007e50<-Kmeans(avg2007[,2:5],2, iter.max=30,method = "euclidean")
result2008e50<-Kmeans(avg2008[,2:5],2, iter.max=30,method = "euclidean")
result2009e50<-Kmeans(avg2009[,2:5],2, iter.max=30,method = "euclidean")

maxe10_78<-0
for(i in c(1:2))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2007e10$cluster==i), (result2008e10$cluster==j), similarity = "jaccard", method = "independence")
    if(maxe10_78<sim)
    {
      maxe10_78<-sim
      ce10_78_1<-i
      ce10_78_2<-j
    }
  }
}
maxe10_78
ce10_78_1
ce10_78_2

maxe50_78<-0
for(i in c(1:2))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2007e50$cluster==i), (result2008e50$cluster==j), similarity = "jaccard", method = "independence")
    if(maxe50_78<sim)
    {
      maxe50_78<-sim
      ce10_78_1<-i
      ce10_78_2<-j
    }
  }
}
maxe50_78
ce10_78_1
ce10_78_2

maxe10_89<-0
for(i in c(1:2))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2008e10$cluster==i), (result2009e10$cluster==j), similarity = "jaccard", method = "independence")
    if(maxe10_89<sim)
    {
      maxe10_89<-sim
      ce10_89_1<-i
      ce10_89_2<-j
    }
  }
}
maxe10_89
ce10_89_1
ce10_89_2

maxe50_89<-0
for(i in c(1:2))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2008e50$cluster==i), (result2009e50$cluster==j), similarity = "jaccard", method = "independence")
    if(maxe50_89<sim)
    {
      maxe50_89<-sim
      ce50_89_1<-i
      ce50_89_2<-j
    }
  }
}
maxe50_89
ce50_89_1
ce50_89_2

maxe10_79<-0
for(i in c(1:2))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2007e10$cluster==i), (result2009e10$cluster==j), similarity = "jaccard", method = "independence")
    if(maxe10_79<sim)
    {
      maxe10_79<-sim
      ce10_79_1<-i
      ce10_79_2<-j
    }
  }
}
maxe10_79
ce10_79_1
ce10_79_2

maxe50_79<-0
for(i in c(1:2))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2007e50$cluster==i), (result2009e50$cluster==j), similarity = "jaccard", method = "independence")
    if(maxe50_79<sim)
    {
      maxe50_79<-sim
      ce50_79_1<-i
      ce50_79_2<-j
    }
  }
}
maxe50_79
ce50_79_1
ce50_79_2

set.seed(10)
result2007p10<-Kmeans(avg2007[,2:5],7, iter.max=30,method = "pearson")
result2008p10<-Kmeans(avg2008[,2:5],2, iter.max=30,method = "pearson")
result2009p10<-Kmeans(avg2009[,2:5],8, iter.max=30,method = "pearson")
set.seed(50)
result2007p50<-Kmeans(avg2007[,2:5],7, iter.max=30,method = "pearson")
result2008p50<-Kmeans(avg2008[,2:5],2, iter.max=30,method = "pearson")
result2009p50<-Kmeans(avg2009[,2:5],8, iter.max=30,method = "pearson")

maxp10_78<-0
for(i in c(1:7))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2007p10$cluster==i), (result2008p10$cluster==j), similarity = "jaccard", method = "independence")
    if(maxp10_78<sim)
    {
      maxp10_78<-sim
      cp10_78_1<-i
      cp10_78_2<-j
    }
  }
}
maxp10_78
cp10_78_1
cp10_78_2

maxp50_78<-0
for(i in c(1:7))
{
  for(j in c(1:2))
  {
    sim<-cluster_similarity((result2007p50$cluster==i), (result2008p50$cluster==j), similarity = "jaccard", method = "independence")
    if(maxp50_78<sim)
    {
      maxp50_78<-sim
      cp50_78_1<-i
      cp50_78_2<-j
    }
  }
}
maxp50_78
cp50_78_1
cp50_78_2

maxp10_89<-0
for(i in c(1:2))
{
  for(j in c(1:8))
  {
    sim<-cluster_similarity((result2008p10$cluster==i), (result2009p10$cluster==j), similarity = "jaccard", method = "independence")
    if(maxp10_89<sim)
    {
      maxp10_89<-sim
      cp10_89_1<-i
      cp10_89_2<-j
    }
  }
}
maxp10_89
cp10_89_1
cp10_89_2

maxp50_89<-0
for(i in c(1:2))
{
  for(j in c(1:8))
  {
    sim<-cluster_similarity((result2008p50$cluster==i), (result2009p50$cluster==j), similarity = "jaccard", method = "independence")
    if(maxp50_89<sim)
    {
      maxp50_89<-sim
      cp50_89_1<-i
      cp50_89_2<-j
    }
  }
}
maxp50_89
cp50_89_1
cp50_89_2


maxp10_79<-0
for(i in c(1:7))
{
  for(j in c(1:8))
  {
    sim<-cluster_similarity((result2007p10$cluster==i), (result2009p10$cluster==j), similarity = "jaccard", method = "independence")
    if(maxp10_79<sim)
    {
      maxp10_79<-sim
      cp10_79_1<-i
      cp10_79_2<-j
    }
  }
}
maxp10_79
cp10_79_1
cp10_79_2

maxp50_79<-0
for(i in c(1:7))
{
  for(j in c(1:8))
  {
    sim<-cluster_similarity((result2007p50$cluster==i), (result2009p50$cluster==j), similarity = "jaccard", method = "independence")
    if(maxp50_79<sim)
    {
      maxp50_79<-sim
      cp50_79_1<-i
      cp50_79_2<-j
    }
  }
}
maxp50_79
cp50_79_1
cp50_79_2