library(dplyr)
library(amap)
library(tidyr)
library(clusteval)
library(purrr)
library(factoextra)
x<-read.table("hourly_2007.g",header=TRUE,skip=1);
dim(x);
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
y1 <- y1 %>%
  mutate(temp = replace(temp, temp == 9999.9, NA));
y1 <- y1 %>%
  mutate(dewpoint = replace(dewpoint, dewpoint == 9999.9, NA));
y1 <- y1 %>%
  mutate(stationpressure = replace(stationpressure, stationpressure == 9999.9, NA));
y1 <- y1 %>%
  mutate(windspeed = replace(windspeed, windspeed == 999.9, NA));

#Average for a day
b<-group_by(y1,station,d)
avg<-summarize(b,temp_avg=mean(temp,na.rm=TRUE),dp_avg=mean(dewpoint,na.rm=TRUE),sp_avg=mean(stationpressure,na.rm=TRUE),ws_avg=mean(windspeed,na.rm=TRUE))

#Average for month
c<-group_by(avg,station)
month_avg<-summarize(c,temp_avg=mean(temp_avg,na.rm=TRUE),dp_avg=mean(dp_avg,na.rm=TRUE),sp_avg=mean(sp_avg,na.rm=TRUE),ws_avg=mean(ws_avg,na.rm=TRUE))
month_avg$sp_avg[is.nan(month_avg$sp_avg)]<-NA
month_avg$sp_avg[is.na(month_avg$sp_avg)]<-mean(month_avg$sp_avg,na.rm=TRUE)
month_avg$temp_avg[is.nan(month_avg$temp_avg)]<-NA
month_avg$temp_avg[is.na(month_avg$temp_avg)]<-mean(month_avg$temp_avg,na.rm=TRUE)
month_avg$dp_avg[is.nan(month_avg$dp_avg)]<-NA
month_avg$dp_avg[is.na(month_avg$dp_avg)]<-mean(month_avg$dp_avg,na.rm=TRUE)
month_avg$ws_avg[is.nan(month_avg$ws_avg)]<-NA
month_avg$ws_avg[is.na(month_avg$ws_avg)]<-mean(month_avg$ws_avg,na.rm=TRUE)

temp_av<-mean(month_avg$temp_avg,na.rm=TRUE)
dp_av<-mean(month_avg$dp_avg,na.rm=TRUE)
ws_av<-mean(month_avg$ws_avg,na.rm=TRUE)
sp_av<-mean(month_avg$sp_avg,na.rm=TRUE)
temp_sd<-sd(month_avg$temp_avg,na.rm=TRUE)
dp_sd<-sd(month_avg$dp_avg,na.rm=TRUE)
ws_sd<-sd(month_avg$ws_avg,na.rm=TRUE)
sp_sd<-sd(month_avg$sp_avg,na.rm=TRUE)
temp_av
dp_av
ws_av
sp_av
temp_sd
dp_sd
ws_sd
sp_sd

#Scaling the data
month_avg[,2:5]<-scale(month_avg[,2:5])

#Performs Kmeans and computes SSE
wss <- function(p,q,s) {
  set.seed(s)
  nm<-paste("result2007",substring(q, 1, 1),s,"_k",p,sep="")
  assign(nm, Kmeans(month_avg[,2:5],p,iter.max = 30, method = q),env=.GlobalEnv)
  wss_values[p-1]<<-sum(get(nm)$withinss)
}

#no. of clusters
wss_values<-2:20
k.values<-2:20

#seed=10
#kmeans using Euclidean
mapply(wss,k.values,"euclidean",10)
plot(k.values, wss_values,type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

result2007e10_k2$centers[,1]<-(result2007e10_k2$centers[,1]*temp_sd)+temp_av
result2007e10_k2$centers[,2]<-(result2007e10_k2$centers[,2]*dp_sd)+dp_av
result2007e10_k2$centers[,3]<-(result2007e10_k2$centers[,3]*sp_sd)+sp_av
result2007e10_k2$centers[,4]<-(result2007e10_k2$centers[,4]*ws_sd)+ws_av
result2007e10_k2$centers

plot(c(1:2),result2007e10_k2$centers[,1],xlab="clusters",ylab="temp_avg")
plot(c(1:2),result2007e10_k2$centers[,2],xlab="clusters",ylab="dp_avg")
plot(c(1:2),result2007e10_k2$centers[,3],xlab="clusters",ylab="ws_avg")
plot(c(1:2),result2007e10_k2$centers[,4],xlab="clusters",ylab="sp_avg")
result2007e10_k2$station<-month_avg$station
plot(result2007e10_k2$station,result2007e10_k2$cluster,col=c(1:2))

#kmeans using pearson
mapply(wss,k.values,"pearson",10)
plot(k.values, wss_values,type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

result2007p10_k7$centers[,1]<-(result2007p10_k7$centers[,1]*temp_sd)+temp_av
result2007p10_k7$centers[,2]<-(result2007p10_k7$centers[,2]*dp_sd)+dp_av
result2007p10_k7$centers[,3]<-(result2007p10_k7$centers[,3]*sp_sd)+sp_av
result2007p10_k7$centers[,4]<-(result2007p10_k7$centers[,4]*ws_sd)+ws_av
result2007p10_k7$centers
plot(c(1:4),result2007p10_k7$centers[,2],xlab="clusters",ylab="dp_avg")
plot(c(1:4),result2007p10_k7$centers[,3],xlab="clusters",ylab="ws_avg")
plot(c(1:4),result2007p10_k7$centers[,4],xlab="clusters",ylab="sp_avg")
result2007p10_k7$station<-month_avg$station
plot(result2007p10_k7$station,result2007p10_k7$cluster,col=c(1:4))


#seed=50
#kmeans using Euclidean
set.seed(50)
result2007e50_k2<-Kmeans(month_avg[,2:5],2,iter.max = 30, method = "euclidean")

result2007e50_k2$centers[,1]<-(result2007e50_k2$centers[,1]*temp_sd)+temp_av
result2007e50_k2$centers[,2]<-(result2007e50_k2$centers[,2]*dp_sd)+dp_av
result2007e50_k2$centers[,3]<-(result2007e50_k2$centers[,3]*sp_sd)+sp_av
result2007e50_k2$centers[,4]<-(result2007e50_k2$centers[,4]*ws_sd)+ws_av
result2007e50_k2$centers

plot(c(1:2),result2007e50_k2$centers[,2],xlab="clusters",ylab="dp_avg")
plot(c(1:2),result2007e50_k2$centers[,3],xlab="clusters",ylab="ws_avg")
plot(c(1:2),result2007e50_k2$centers[,4],xlab="clusters",ylab="sp_avg")
result2007e50_k2$station<-month_avg$station
plot(result2007e50_k2$station,result2007e50_k2$cluster,col=c(1:2))

#kmeans using pearson
set.seed(50)
result2007p50_k7<-Kmeans(month_avg[,2:5],7,iter.max = 30, method = "pearson")
result2007p50_k7$centers[,1]<-(result2007p50_k7$centers[,1]*temp_sd)+temp_av
result2007p50_k7$centers[,2]<-(result2007p50_k7$centers[,2]*dp_sd)+dp_av
result2007p50_k7$centers[,3]<-(result2007p50_k7$centers[,3]*sp_sd)+sp_av
result2007p50_k7$centers[,4]<-(result2007p50_k7$centers[,4]*ws_sd)+ws_av
result2007p50_k7$centers

plot(c(1:4),result2007p50_k7$centers[,2],xlab="clusters",ylab="dp_avg")
plot(c(1:4),result2007p50_k7$centers[,3],xlab="clusters",ylab="ws_avg")
plot(c(1:4),result2007p50_k7$centers[,4],xlab="clusters",ylab="sp_avg")
result2007p50_k7$station<-month_avg$station
plot(result2007p50_k7$station,result2007p50_k7$cluster,col=c(1:7))

#Total SSE values
sum(result2007e10_k2$withinss)
sum(result2007e50_k2$withinss)
sum(result2007p10_k7$withinss)
sum(result2007p50_k7$withinss)

maxep10<-0
for(i in c(1:2))
{
  for(j in c(1:7))
  {
    sim<-cluster_similarity((result2007e10_k2$cluster==i), (result2007p10_k7$cluster==j), similarity = "jaccard", method = "independence")
    if(maxep10<sim)
    {
      maxep10<-sim
      c1ep10<-i
      c2ep10<-j
    }
  }
}
maxep10
c1ep10
c2ep10

maxep50<-0
for(i in c(1:2))
{
  for(j in c(1:7))
  {
    sim<-cluster_similarity((result2007e50_k2$cluster==i), (result2007p50_k7$cluster==j), similarity = "jaccard", method = "independence")
    if(maxep10<sim)
    {
      maxep50<-sim
      c1ep50<-i
      c2ep50<-j
    }
  }
}
maxep50
c1ep50
c2ep50

maxep1050<-0
for(i in c(1:2))
{
  for(j in c(1:7))
  {
    sim<-cluster_similarity((result2007e10_k2$cluster==i), (result2007p50_k7$cluster==j), similarity = "jaccard", method = "independence")
    if(maxep1050<sim)
    {
      maxep1050<-sim
      c1ep1050<-i
      c2ep1050<-j
    }
  }
}
maxep1050
c1ep1050
c2ep1050

maxep5010<-0
for(i in c(1:2))
{
  for(j in c(1:7))
  {
    sim<-cluster_similarity((result2007e50_k2$cluster==i), (result2007p10_k7$cluster==j), similarity = "jaccard", method = "independence")
    if(maxep5010<sim)
    {
      maxep5010<-sim
      c1ep5010<-i
      c2ep5010<-j
    }
  }
}
maxep5010
c1ep5010
c2ep5010