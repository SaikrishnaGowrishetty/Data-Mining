rm(list=ls())
library(readxl)
library(dplyr)
library(plyr)
library(arules)
library(stringr)
library(data.table)
library(RColorBrewer)
library(arulesViz)
retail <- read_excel("online-retail/online-retail.xlsx")
retail <- retail[complete.cases(retail), ]
retail %>% mutate(Description = as.factor(Description))
retail_wo_cncl<-retail[-which(substr(str_to_upper(retail$InvoiceNo),1,1) == "C"),]
a<-c("WRONG","LOST", "CRUSHED", "SMASHED", "DAMAGED",
     "FOUND", "THROWN", "MISSING", "AWAY", "\\?", "CHECK", "POSTAGE",
     "MANUAL", "CHARGES", "AMAZON", "FEE", "FAULT", "SALES", "ADJUST",
     "COUNTED", "LABEL", "INCORRECT", "SOLD", "BROKEN", "BARCODE",
     "CRACKED", "RETURNED", "MAILOUT", "DELIVERY", "MIX UP", "MOULDY", "PUT
ASIDE", "ERROR", "DESTROYED", "RUSTY")
a<-paste0(" ",a," ")
b<-paste0(a, collapse = "|")
retail_wo_cncl$Description<-str_pad(retail_wo_cncl$Description,width=16,side="both")
retail_valid<-retail_wo_cncl[!retail_wo_cncl$Description %like%  b,]
set.seed(40)
retail_randsel<-retail_valid
#[sample(nrow(retail_valid),200000),]
l=unique(c(retail_randsel$Description))
retail_con <- data.frame(InvoiceNo= retail_randsel$InvoiceNo,desc=retail_randsel$Description,desc_int=as.numeric(factor(retail_randsel$Description, levels=l)))
transactionData <- ddply(retail_con,c("InvoiceNo"),
                         function(df1)paste(df1$desc_int,
                                            collapse = ","))
transactionData$InvoiceNo <- NULL
colnames(transactionData) <- c("items")
write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('market_basket_transactions.csv', format = 'basket', sep=',')

coul <- brewer.pal(5, "Set2") 
name=c(1,2,3,4,5)

#First
candidateitemsets111 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.005, target="frequent itemsets", minlen=1, maxlen=1))
candidateitemsets111len <- length(candidateitemsets111)
candidateitemsets122 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.005, target="frequent itemsets", minlen=2, maxlen=2))
candidateitemsets122len <- length(candidateitemsets122)
candidateitemsets133 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.005, target="frequent itemsets", minlen=3, maxlen=3))
candidateitemsets133len <- length(candidateitemsets133)
candidateitemsets144 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.005, target="frequent itemsets", minlen=4, maxlen=4))
candidateitemsets144len <- length(candidateitemsets144)
candidateitemsets155 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.005, target="frequent itemsets", minlen=5, maxlen=5))
candidateitemsets155len <- length(candidateitemsets155)
plotcand1 <- c(candidateitemsets111len,candidateitemsets122len,candidateitemsets133len,candidateitemsets144len,candidateitemsets155len)
plotcand1
barplot(plotcand1,names=name,xlab="Iterations",ylab="Length of Itemsets",col=coul)

frqntitemsets111 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.5, target="frequent itemsets", minlen=1, maxlen=1))
frqntitemsets111len <- length(frqntitemsets111)
frqntitemsets122 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.5, target="frequent itemsets", minlen=2, maxlen=2))
frqntitemsets122len <- length(frqntitemsets122)
frqntitemsets133 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.5, target="frequent itemsets", minlen=3, maxlen=3))
frqntitemsets133len <- length(frqntitemsets133)
frqntitemsets144 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.5, target="frequent itemsets", minlen=4, maxlen=4))
frqntitemsets144len <- length(frqntitemsets144)
frqntitemsets155 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.5, target="frequent itemsets", minlen=5, maxlen=5))
frqntitemsets155len <- length(frqntitemsets155)
plotfreq1 <- c(frqntitemsets111len,frqntitemsets122len,frqntitemsets133len,frqntitemsets144len,frqntitemsets155len)
plotfreq1
barplot(plotfreq1,names=name,xlab="Iterations",ylab="Length of Itemsets",col=coul)


#second

#candidate
#min_sup=0.0005,min_conf=0.007
candidateitemsets211 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.007, target="frequent itemsets", minlen=1, maxlen=1))
candidateitemsets211len <- length(candidateitemsets211)
candidateitemsets222 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.007, target="frequent itemsets", minlen=2, maxlen=2))
candidateitemsets222len <- length(candidateitemsets222)
candidateitemsets233 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.007, target="frequent itemsets", minlen=3, maxlen=3))
candidateitemsets233len <- length(candidateitemsets233)
candidateitemsets244 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.007, target="frequent itemsets", minlen=4, maxlen=4))
candidateitemsets244len <- length(candidateitemsets244)
candidateitemsets255 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.007, target="frequent itemsets", minlen=5, maxlen=5))
candidateitemsets255len <- length(candidateitemsets255)
plotcand2 <- c(candidateitemsets211len,candidateitemsets222len,candidateitemsets233len,candidateitemsets244len,candidateitemsets255len)
plotcand2
barplot(plotcand2,names=name,xlab="Iterations",ylab="Length of Itemsets",col=coul)

#frequent
#min_sup=0.005,min_conf=0.7
frqntitemsets211 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.7, target="frequent itemsets", minlen=1, maxlen=1))
frqntitemsets211len <- length(frqntitemsets211)
frqntitemsets222 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.7, target="frequent itemsets", minlen=2, maxlen=2))
frqntitemsets222len <- length(frqntitemsets222)
frqntitemsets233 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.7, target="frequent itemsets", minlen=3, maxlen=3))
frqntitemsets233len <- length(frqntitemsets233)
frqntitemsets244 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.7, target="frequent itemsets", minlen=4, maxlen=4))
frqntitemsets244len <- length(frqntitemsets244)
frqntitemsets255 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.7, target="frequent itemsets", minlen=5, maxlen=5))
frqntitemsets255len <- length(frqntitemsets255)
plotfreq2 <- c(frqntitemsets211len,frqntitemsets222len,frqntitemsets233len,frqntitemsets244len,frqntitemsets255len)
plotfreq2
barplot(plotfreq2,names=name,xlab="Iterations",ylab="Length of Itemsets",col=coul)

#third

candidateitemsets311 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.008, target="frequent itemsets", minlen=1, maxlen=1))
candidateitemsets311len <- length(candidateitemsets311)
candidateitemsets322 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.008, target="frequent itemsets", minlen=2, maxlen=2))
candidateitemsets322len <- length(candidateitemsets322)
candidateitemsets333 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.008, target="frequent itemsets", minlen=3, maxlen=3))
candidateitemsets333len <- length(candidateitemsets333)
candidateitemsets344 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.008, target="frequent itemsets", minlen=4, maxlen=4))
candidateitemsets344len <- length(candidateitemsets344)
candidateitemsets355 <- apriori(tr, parameter = list(supp = 0.0005, conf = 0.008, target="frequent itemsets", minlen=5, maxlen=5))
candidateitemsets355len <- length(candidateitemsets355)
plotcand3 <- c(candidateitemsets311len,candidateitemsets322len,candidateitemsets333len,candidateitemsets344len,candidateitemsets355len)
plotcand3
barplot(plotcand3,names=name,xlab="Iterations",ylab="Length of Itemsets",col=coul)

frqntitemsets311 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, target="frequent itemsets", minlen=1, maxlen=1))
frqntitemsets311len <- length(frqntitemsets311)
frqntitemsets322 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, target="frequent itemsets", minlen=2, maxlen=2))
frqntitemsets322len <- length(frqntitemsets322)
frqntitemsets333 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, target="frequent itemsets", minlen=3, maxlen=3))
frqntitemsets333len <- length(frqntitemsets333)
frqntitemsets344 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, target="frequent itemsets", minlen=4, maxlen=4))
frqntitemsets344len <- length(frqntitemsets344)
frqntitemsets355 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, target="frequent itemsets", minlen=5, maxlen=5))
frqntitemsets355len <- length(frqntitemsets355)
plotfreq3 <- c(frqntitemsets311len,frqntitemsets322len,frqntitemsets333len,frqntitemsets344len,frqntitemsets355len)
plotfreq3
barplot(plotfreq3,names=name,xlab="Iterations",ylab="Length of Itemsets",col=coul)

length(rules11)
rules11 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.5, target="rules", minlen=2))
plot(rules11,jitter = 0)
rules12 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.7, target="rules", minlen=2))
plot(rules12,jitter = 0)
rules13 <- apriori(tr, parameter = list(supp = 0.002, conf = 0.8, target="rules", minlen=2))
plot(rules13,jitter = 0)

rules21 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.5, target="rules", minlen=2))
plot(rules21,jitter = 0)
rules22 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.7, target="rules", minlen=2))
plot(rules22,jitter = 0)
rules23 <- apriori(tr, parameter = list(supp = 0.005, conf = 0.8, target="rules", minlen=2))
plot(rules23,jitter = 0)



rules31 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.5, target="rules", minlen=2))
plot(rules31,jitter = 0)
rules32 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.7, target="rules", minlen=2))
plot(rules32,jitter = 0)
rules33 <- apriori(tr, parameter = list(supp = 0.01, conf = 0.8, target="rules", minlen=2))
plot(rules33,jitter = 0)

#plotting number of rules with min_sup & min_conf
sup<-c(0.002,0.002,0.002,0.005,0.005,0.005,0.01,0.01,0.01)
con<-c(0.5,0.7,0.8,0.5,0.7,0.8,0.5,0.7,0.8)
col1<-c(1,2,3,1,2,3,1,2,3)
rul<-c(length(rules11),length(rules12),length(rules13),length(rules21),length(rules22),length(rules23),length(rules31),length(rules32),length(rules33))
rul
y<-data.frame(con=con,rul=rul,col=col1)
y

barplot(rul,names=sup,xlab="sup",ylab="no. of rules")
barplot(rul,names=con,xlab="conf",ylab="no. of rules")
barplot(con,rul)
plot(sup,rul,col=y$col,xlab="sup",ylab="no. of rules")
plot(con,rul)
barplot(rul,names=sup,col=y$col,xlab="sup",ylab="no. of rules",legend=c(0.5,0.7,0.8),args.legend = list(x = "topright", ncol = 3))

#filter rules
rulesgreaterthan10 <- sort(subset(rules11, subset = lift > 10),by="lift")
inspect(rulesgreaterthan10)
rulesgreaterthan10head <- head(rulesgreaterthan10, n = 10, by ="lift")
inspect(rulesgreaterthan10head)

ruleslessthan10 <- sort(subset(rules11, subset = lift < 10),by="lift")
#inspect(ruleslessthan10)
ruleslessthan10head <- head(ruleslessthan10, n = 10, by ="lift")
inspect(ruleslessthan10head)

ruleslessthan10head[1]

descintmap <- distinct(retail_con,desc, desc_int)
l<-as(lhs(ruleslessthan10head), "list")
r<-as(rhs(ruleslessthan10head), "list")
cnt <- 1
repeat {
  lhs<-""
  rhs<-""
  rv<-unlist(r[cnt])
  lv<-unlist(l[cnt])
  for(value in lv){
    lhs<-paste(lhs,descintmap[descintmap$desc_int==value,][,c(1)],sep=",")
  }
  rhs<-descintmap[descintmap$desc_int==rv[1],][,c(1)]
  rule=paste(lhs,rhs,sep="->")
  print(rule)
  cnt <- cnt+1
  
  if(cnt > 10) {
    break
  }
}

l<-as(lhs(rulesgreaterthan10head), "list")
r<-as(rhs(rulesgreaterthan10head), "list")
cnt <- 1
repeat {
  lhs<-""
  rhs<-""
  rv<-unlist(r[cnt])
  lv<-unlist(l[cnt])
  for(value in lv){
    lhs<-paste(lhs,descintmap[descintmap$desc_int==value,][,c(1)],sep=",")
  }
  rhs<-descintmap[descintmap$desc_int==rv[1],][,c(1)]
  rule=paste(lhs,rhs,sep="->")
  print(rule)
  cnt <- cnt+1
  
  if(cnt > 10) {
    break
  }
}


#visualize
rules <- apriori(tr, parameter = list(supp = 0.002, conf = 0.05, target="rules", minlen=2))
rules <- sort(rules, by ="confidence")
rules <- head(rules, n=100, by ="confidence")
inspect(rules)
plot(rules,measure=c("confidence","support"),shading="lift",col=c(1,2,3))
plot(rulesgreaterthan10head,measure=c("confidence","support"),shading="lift",col=c(1,2,3))
plot(ruleslessthan10head,measure=c("confidence","support"),shading="lift",col=c(1,2,3))