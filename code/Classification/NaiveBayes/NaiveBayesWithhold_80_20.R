library(e1071)
require(dplyr)
require(rpart)
require(rpart.plot)
d = read.table(text = gsub("\"", "", readLines("bank-additional-full.csv")),sep=";", header=TRUE)
e <-d[!(d$age=="unknown"|d$job=="unknown"|d$marital=="unknown"|d$education=="unknown"|d$default=="unknown"|d$housing=="unknown"|d$loan=="unknown"|d$contact=="unknown"|d$month=="unknown"|d$day_of_week=="unknown"|d$duration=="unknown"|d$campaign=="unknown"|d$pdays=="unknown"|d$previous=="unknown"|d$poutcome=="unknown"|d$emp.var.rate=="unknown"|d$cons.price.idx=="unknown"|d$cons.conf.idx=="unknown"|d$euribor3m=="unknown"|d$nr.employed=="unknown"|d$y=="unknown"),]
f <- subset(e, select = -c(marital, default, housing, loan, contact) )
f$education<-factor(f$education)
f$job<-factor(f$job)
f$month<-factor(f$month)
f$day_of_week<-factor(f$day_of_week)
f$y<-factor(f$y)
set.seed(40)
x<-sample(1:nrow(f),10000)
y1<-f[x,]
y<-subset(y1, select = -c(previous))
dim(y)
tridx<-sample(1:nrow(y),0.8*nrow(y))
teidx<-setdiff(1:nrow(y),tridx)
train<-y[tridx,]
test<-y[teidx,]
N=naiveBayes(y ~., data=train)
N
Np=predict(N,test)
Np
cfmtx<-table(Np,test$y)
cfmtx
accuracy<-sum(diag(cfmtx))/sum(cfmtx)
accuracy
precision<-cfmtx[1,1]/(cfmtx[1,1]+cfmtx[1,2])
precision
recall<-cfmtx[1,1]/(cfmtx[1,1]+cfmtx[2,1])
recall
f1_score<-2*precision*recall/(precision+recall)
f1_score