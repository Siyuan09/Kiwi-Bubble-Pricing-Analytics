library("dummies")
library("AER")
library("plotly")
library('RColorBrewer')
library("rgl")
library("data.table")
library("mlogit")
library("gmnl")

################################  Question 3  ###############################################
setwd("~/Desktop/Pricing Analytics/Project/Project 2")
library(readr)
kiwi <- read_csv("kiwi_bubbles_P2.csv")
kiwi<-as.data.frame(kiwi)
kiwi=kiwi[!(kiwi$price.MB==99),]
# unit cost = 0.5
# consumers = 1000

# simple multinomial logit model
mlogitdata <- mlogit.data(kiwi,id = "id",varying = 4:7, choice = "choice", shape = "wide")
mle_ori <- gmnl(choice~price, data = mlogitdata)
summary(mle_ori)

mlogitdata_temp <- mlogit.data(kiwi,id = "id",varying = c(4,6,7), choice = "choice", shape = "wide")
mle_ori_temp <- gmnl(choice~price, data = mlogitdata_temp)
summary(mle_ori_temp)


# find the profit-maximing price fo KR&KB
demand=function(priceKB,priceKR,priceMB,para = mle_ori$coefficients){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR,probMB))
}

profit=function(priceKB,priceKR,priceMB =1.43,para =mle_ori$coefficients){
    profitKB=demand(priceKB,priceKR,priceMB,para)[,1]*(priceKB-0.5)*1000
    profitKR=demand(priceKB,priceKR,priceMB,para)[,2]*(priceKR-0.5)*1000
    return(cbind(profitKB,profitKR))
}

aux=seq(0.5,1.8,0.01)
pricespace=expand.grid(aux,aux)

profitmat=matrix(0L,nrow(pricespace),1)
for (i in 1:nrow(pricespace)){
    profitmat[i]=sum(profit(pricespace[i,1],pricespace[i,2],para = mle_ori$coefficients))  
}

max(profitmat)
profitmat[which.max(profitmat),]
pricespace[which.max(profitmat),]

# find the profit-maximing price fo KR
demand=function(priceKR,priceMB,para = mle_ori_temp$coefficients){
    probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
    probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
    return(cbind(probKR,probMB))
}

profit=function(priceKR,priceMB =1.43,para =mle_ori_temp$coefficients){
    profitKR=demand(priceKR,priceMB,para)[,1]*(priceKR-0.5)*1000
    return(profitKR)
}

aux=seq(0.5,1.8,0.01)

profitmat=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat[i]=profit(aux[i],1.43) 
}
opti_KR <- aux[profitmat==max(profitmat)]
max(profitmat)


# plot the 3D profit 
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=as.numeric(profitmat),
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profitmat), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
    layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
    config(mathjax = 'cdn')
p


# own-elasticity
avgpriceKB <- mean(kiwi$price.KB)
avgpriceKR <- mean(kiwi$price.KR)
avgpriceMB <- mean(kiwi$price.MB)
para <- mle_ori_temp$coefficients
probKB=exp(para[1]+para[4]*avgpriceKB)/(1+exp(para[1]+para[4]*avgpriceKB)+exp(para[2]+para[4]*avgpriceKR)+exp(para[3]+para[4]*avgpriceMB))
probKR=exp(para[2]+para[4]*avgpriceKR)/(1+exp(para[1]+para[4]*avgpriceKB)+exp(para[2]+para[4]*avgpriceKR)+exp(para[3]+para[4]*avgpriceMB))
probMB=exp(para[3]+para[4]*avgpriceMB)/(1+exp(para[1]+para[4]*avgpriceKB)+exp(para[2]+para[4]*avgpriceKR)+exp(para[3]+para[4]*avgpriceMB))

ownKB<--para[4]*avgpriceKB*(1-probKB)
ownKR<--para[4]*avgpriceKR*(1-probKR)

# cross-elasticity
crossKB1<--para[4]*avgpriceKR*probKR
crossKB2<--para[4]*avgpriceMB*probMB
crossKR1<--para[4]*avgpriceKB*probKB
crossKR2<--para[4]*avgpriceMB*probMB


################################  Question 4  ###############################################
# segmentation = 3 + 1
set.seed(0)
demo <- read_csv("demo_P2.csv")
demo_cluster <- kmeans(x=demo[,2:18], centers = 3, nstart = 1000)
cluster_id<-data.frame(id = demo$id)
cluster_id$cluster<-demo_cluster$cluster

kiwi_new<-merge(kiwi,cluster_id,by='id',all.x = T)

kiwi_new$cluster[is.na(kiwi_new$cluster)]=4
seg.share = c(table(demo_cluster$cluster),359-sum(table(demo_cluster$cluster)))/359

# 
coef <- matrix(0,4,5)
for (seg in 1:4){
    kiwi.sub <- subset(kiwi_new,cluster==seg)
    mlogitdata<-mlogit.data(kiwi.sub,varying = 4:7, choice = 'choice',shape = 'wide')
    mle <- gmnl(choice~price,data = mlogitdata)
    mle
    coef[seg,1]<- seg
    coef[seg,2:5]<-mle$coefficients
}
coef<-as.data.frame(coef)
colnames(coef)=c('segment','intercept.KB','intercept.KR','intercept.MB','price.coef')

# scatter plot 
plot(coef[1,2]-coef[1,3],coef[1,5],cex=20*seg.share[1],xlim=c(-1,1),ylim=c(-5,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^KR",ylab=("beta_1"))
points(coef[2,2]-coef[2,3],coef[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef[3,2]-coef[3,3],coef[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef[4,2]-coef[4,3],coef[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)


plot(coef[1,3]-coef[1,4],coef[1,5],cex=20*seg.share[1],xlim=c(-1,1),ylim=c(-5,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KR-beta_0^MB",ylab=("beta_1"))
points(coef[2,3]-coef[2,4],coef[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef[3,3]-coef[3,4],coef[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef[4,3]-coef[4,4],coef[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)


plot(coef[1,2]-coef[1,4],coef[1,5],cex=20*seg.share[1],xlim=c(-1,1),ylim=c(-5,-1.5),
     col = "chocolate",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="beta_0^KB-beta_0^MB",ylab=("beta_1"))
points(coef[2,2]-coef[2,4],coef[2,5],cex=20*seg.share[2],col = "chocolate",pch=16)
points(coef[3,2]-coef[3,4],coef[3,5],cex=20*seg.share[3],col = "chocolate",pch=16)
points(coef[4,2]-coef[4,4],coef[4,5],cex=20*seg.share[4],col = "chocolate",pch=16)


# Profit plot
xaxis=list(title="P^{KB}")
yaxis=list(autorange = "reversed",title="P^{KR}")
zaxis=list(title="Profit")
p=plot_ly(x=pricespace[,1],y=pricespace[,2],z=profit,
          type="scatter3d",mode="markers",
          marker = list(color = as.numeric(profit), colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))%>%
    layout(scene=list(xaxis=xaxis,yaxis=yaxis,zaxis=zaxis))%>%
    config(mathjax = 'cdn')
p
max(profit)



pricespace[which.max(profit),]
pricespace[profit_noseg==max(profit_noseg)]

# profit for 2 products 
demand_2P <- function(priceKB,priceKR,priceMB,para){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR))
}

agg_choice=function(priceKB,priceKR,priceMB) {
    
    agg_choice=seg.share[1]*(demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[1,2:5]))[,1]+demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[1,2:5]))[,2])+
        seg.share[2]*(demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[2,2:5]))[,1]+demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[2,2:5]))[,2])+
        seg.share[3]*(demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[3,2:5]))[,1]+demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[3,2:5]))[,2])+
        seg.share[4]*(demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[4,2:5]))[,1]+demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[4,2:5]))[,2])+
        seg.share[5]*(demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[5,2:5]))[,1]+demand_2P(priceKB,priceKR,priceMB,as.numeric(coef[5,2:5]))[,2])
    
    return(agg_choice)
}

profit_2P=function(priceKB,priceKR,priceMB){
    profitKB=1000*(demand_2P(priceKB,priceKR,1.43,as.numeric(coef[1,2:5]))[,1]*(priceKB-uc)*seg.share[1]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[2,2:5]))[,1]*(priceKB-uc)*seg.share[2]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[3,2:5]))[,1]*(priceKB-uc)*seg.share[3]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[4,2:5]))[,1]*(priceKB-uc)*seg.share[4]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[5,2:5]))[,1]*(priceKB-uc)*seg.share[5])
                       #demand_2P(priceKB,priceKR,1.43,as.numeric(coef[6,2:5]))[,1]*(priceKB-uc)*seg.share[6])
    profitKR=1000*(demand_2P(priceKB,priceKR,1.43,as.numeric(coef[1,2:5]))[,2]*(priceKR-uc)*seg.share[1]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[2,2:5]))[,2]*(priceKR-uc)*seg.share[2]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[3,2:5]))[,2]*(priceKR-uc)*seg.share[3]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[4,2:5]))[,2]*(priceKR-uc)*seg.share[4]+
                       demand_2P(priceKB,priceKR,1.43,as.numeric(coef[5,2:5]))[,2]*(priceKR-uc)*seg.share[5])
                       #demand_2P(priceKB,priceKR,1.43,as.numeric(coef[6,2:5]))[,2]*(priceKR-uc)*seg.share[6])
    return(cbind(profitKB,profitKR))
}

aux=seq(0.5,1.8,0.01)
pricespace_1=expand.grid(aux,aux)
uc=0.5
profitmat_2P=matrix(0L,nrow(pricespace_1),1)
for (i in 1:nrow(pricespace_1)){
    profitmat_2P[i]=sum(profit_2P(pricespace_1[i,1],pricespace_1[i,2],1.43)) 
}
opti_KB <- pricespace_1[,1][profitmat_2P==max(profitmat_2P)]
opti_KR<- pricespace_1[,2][profitmat_2P==max(profitmat_2P)]
max(profitmat_2P)


# profit for only KR 
set.seed(0)
demo <- read_csv("demo_P2.csv")
demo_cluster <- kmeans(x=demo[,2:18], centers = 3, nstart = 1000)
cluster_id<-data.frame(id = demo$id)
cluster_id$cluster<-demo_cluster$cluster

kiwi_new<-merge(kiwi,cluster_id,by='id',all.x = T)

kiwi_new$cluster[is.na(kiwi_new$cluster)]=4
seg.share = c(table(demo_cluster$cluster),359-sum(table(demo_cluster$cluster)))/359

coef2 <- matrix(0,4,4)
for (seg in 1:4){
    kiwi.sub <- subset(kiwi_new,cluster==seg)
    mlogitdata<-mlogit.data(kiwi.sub,varying =c(4,6,7), choice = 'choice',shape = 'wide')
    mle <- gmnl(choice~price,data = mlogitdata)
    mle
    coef2[seg,1]<- seg
    coef2[seg,2:4]<-mle$coefficients
}
coef2<-as.data.frame(coef2)
colnames(coef2)=c('segment','intercept.KR','intercept.MB','price.coef')

demand_1P <- function(priceKR,priceMB,para){
    probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
    return(probKR)
}


profit_1P=function(priceKR,priceMB){
    profitKR=1000*(demand_1P(priceKR,1.43,as.numeric(coef2[1,2:4]))*(priceKR-uc)*seg.share[1]+
                       demand_1P(priceKR,1.43,as.numeric(coef2[2,2:4]))*(priceKR-uc)*seg.share[2]+
                       demand_1P(priceKR,1.43,as.numeric(coef2[3,2:4]))*(priceKR-uc)*seg.share[3]+
                       demand_1P(priceKR,1.43,as.numeric(coef2[4,2:4]))*(priceKR-uc)*seg.share[4])
                       #demand_1P(priceKR,1.43,as.numeric(coef2[5,2:4]))*(priceKR-uc)*seg.share[5])
                       #demand_1P(priceKR,1.43,as.numeric(coef[6,2:4]))[,1]*(priceKB-uc)*seg.share[6])
    return(profitKR)
}

aux=seq(0.5,1.8,0.01)

uc=0.5
profitmat_1P=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_1P[i]=profit_1P(aux[i],1.43) 
}
opti_KR <- aux[profitmat_1P==max(profitmat_1P)]
max(profitmat_1P)


# own price elasticity

#avgprice_seg<-matrix(0,4,3)
#for (i in 1:4){
 #   avgprice_seg[i,1]<-mean(kiwi_new[kiwi_new$cluster==i,]$price.KB)
  #  avgprice_seg[i,2]<-mean(kiwi_new[kiwi_new$cluster==i,]$price.KR)
   # avgprice_seg[i,3]<-mean(kiwi_new[kiwi_new$cluster==i,]$price.MB)
#}
#avgprice_seg<-as.data.frame(avgprice_seg)
#colnames(avgprice_seg)<-c('KB','KR','MB')

seg_part_1<-matrix(0,4,1)
seg_market_1<-matrix(0,4,1)

probKR1<-matrix(0,4,1)
for (i in 1:4){
    probKR1[i]=exp(coef[i,3]+coef[i,5]*avgpriceKR)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_1[i]<-seg.share[i]*coef[i,5]*probKR1[i]*(1-probKR1[i])
    seg_market_1[i]<-seg.share[i]*probKR1[i]
    ownKR1<--avgpriceKR/sum(seg_market_1)*sum(seg_part_1)
}


probKB1<-matrix(0,4,1)
for (i in 1:4){
    probKB1[i]=exp(coef[i,2]+coef[i,5]*avgpriceKB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_1[i]<-seg.share[i]*coef[i,5]*probKB1[i]*(1-probKB1[i])
    seg_market_1[i]<-seg.share[i]*probKB1[i]
    ownKB1<--avgpriceKB/sum(seg_market_1)*sum(seg_part_1)
}


probMB1<-matrix(0,4,1)
for (i in 1:4){
    probMB1[i]=exp(coef[i,4]+coef[i,5]*avgpriceMB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_1[i]<-seg.share[i]*coef[i,5]*probMB1[i]*(1-probMB1[i])
    seg_market_1[i]<-seg.share[i]*probMB1[i]
    ownMB1<--avgpriceMB/sum(seg_market_1)*sum(seg_part_1)
}

seg_own_elasticity<-cbind(ownKB1,ownKR1,ownMB1)
colnames(seg_own_elasticity)<-c('KB','KR','MB')

# cross price elasticity 
seg_part_2<-matrix(0,4,1)
seg_market_2<-matrix(0,4,1)

for (i in 1:4){
    probKR1[i]=exp(coef[i,3]+coef[i,5]*avgpriceKR)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    probKB1[i]=exp(coef[i,2]+coef[i,5]*avgpriceKB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_2[i]<-seg.share[i]*coef[i,5]*probKB1[i]*probKR1[i]
    seg_market_2[i]<-seg.share[i]*probKB1[i]
    crossKB_KR<--avgpriceKR/sum(seg_market_2)*sum(seg_part_2)
}


for (i in 1:4){
    probMB1[i]=exp(coef[i,4]+coef[i,5]*avgpriceMB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    probKB1[i]=exp(coef[i,2]+coef[i,5]*avgpriceKB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_2[i]<-seg.share[i]*coef[i,5]*probKB1[i]*probMB1[i]
    seg_market_2[i]<-seg.share[i]*probKB1[i]
    crossKB_MB<--avgpriceMB/sum(seg_market_2)*sum(seg_part_2)
}

for (i in 1:4){
    probKR1[i]=exp(coef[i,3]+coef[i,5]*avgpriceKR)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    probKB1[i]=exp(coef[i,2]+coef[i,5]*avgpriceKB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_2[i]<-seg.share[i]*coef[i,5]*probKB1[i]*probKR1[i]
    seg_market_2[i]<-seg.share[i]*probKR1[i]
    crossKR_KB<--avgpriceKB/sum(seg_market_2)*sum(seg_part_2)
}


for (i in 1:4){
    probMB1[i]=exp(coef[i,4]+coef[i,5]*avgpriceMB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    probKR1[i]=exp(coef[i,3]+coef[i,5]*avgpriceKR)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_2[i]<-seg.share[i]*coef[i,5]*probKR1[i]*probMB1[i]
    seg_market_2[i]<-seg.share[i]*probKR1[i]
    crossKR_MB<--avgpriceMB/sum(seg_market_2)*sum(seg_part_2)
}

for (i in 1:4){
    probMB1[i]=exp(coef[i,4]+coef[i,5]*avgpriceMB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    probKR1[i]=exp(coef[i,3]+coef[i,5]*avgpriceKR)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_2[i]<-seg.share[i]*coef[i,5]*probKR1[i]*probMB1[i]
    seg_market_2[i]<-seg.share[i]*probMB1[i]
    crossMB_KR<--avgpriceKR/sum(seg_market_2)*sum(seg_part_2)
}

for (i in 1:4){
    probMB1[i]=exp(coef[i,4]+coef[i,5]*avgpriceMB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    probKB1[i]=exp(coef[i,2]+coef[i,5]*avgpriceKB)/(1+exp(coef[i,2]+coef[i,5]*avgpriceKB)+exp(coef[i,3]+coef[i,5]*avgpriceKR)+exp(coef[i,4]+coef[i,5]*avgpriceMB))
    seg_part_2[i]<-seg.share[i]*coef[i,5]*probKB1[i]*probMB1[i]
    seg_market_2[i]<-seg.share[i]*probMB1[i]
    crossMB_KB<--avgpriceKB/sum(seg_market_2)*sum(seg_part_2)
}

seg_cross_elasticity<-cbind(crossKB_KR,crossKB_MB,crossKR_KB,crossKR_MB,crossMB_KR,crossMB_KB)
colnames(seg_cross_elasticity)<-c('KB_KR','KB_MB','KR_KB','KR_MB','MB_KR','MB_KB')




################################  Question 5  ###############################################

# Round 1 -- MB 
demand_MB_1 <- function(priceKB,priceKR,priceMB,para){
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(probMB)
}


profitMB=function(priceKB,priceKR,priceMB){
    profitMB=1000*(demand_MB_1(1.16,1.16,priceMB,as.numeric(coef[1,2:5]))*(priceMB-uc)*seg.share[1]+
                       demand_MB_1(1.16,1.16,priceMB,as.numeric(coef[2,2:5]))*(priceMB-uc)*seg.share[2]+
                       demand_MB_1(1.16,1.16,priceMB,as.numeric(coef[3,2:5]))*(priceMB-uc)*seg.share[3]+
                       demand_MB_1(1.16,1.16,priceMB,as.numeric(coef[4,2:5]))*(priceMB-uc)*seg.share[4])
    return(profitMB)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_MB=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_MB[i]=profitMB(1.16,1.16,aux[i]) 
}
opti_MB_1 <- aux[profitmat_MB==max(profitmat_MB)]
max(profitmat_MB) # 0.94

# Round 2 -- KB & KR
demand_2P <- function(priceKB,priceKR,priceMB,para){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR))
}

profit_2P=function(priceKB,priceKR,priceMB){
    profitKB=1000*(demand_2P(priceKB,priceKR,0.94,as.numeric(coef[1,2:5]))[,1]*(priceKB-uc)*seg.share[1]+
                       demand_2P(priceKB,priceKR,0.94,as.numeric(coef[2,2:5]))[,1]*(priceKB-uc)*seg.share[2]+
                       demand_2P(priceKB,priceKR,0.94,as.numeric(coef[3,2:5]))[,1]*(priceKB-uc)*seg.share[3]+
                       demand_2P(priceKB,priceKR,0.94,as.numeric(coef[4,2:5]))[,1]*(priceKB-uc)*seg.share[4])
                       
    profitKR=1000*(demand_2P(priceKB,priceKR,0.94,as.numeric(coef[1,2:5]))[,2]*(priceKR-uc)*seg.share[1]+
                       demand_2P(priceKB,priceKR,0.94,as.numeric(coef[2,2:5]))[,2]*(priceKR-uc)*seg.share[2]+
                       demand_2P(priceKB,priceKR,0.94,as.numeric(coef[3,2:5]))[,2]*(priceKR-uc)*seg.share[3]+
                       demand_2P(priceKB,priceKR,0.94,as.numeric(coef[4,2:5]))[,2]*(priceKR-uc)*seg.share[4])
    return(cbind(profitKB,profitKR))
}

aux=seq(0.5,1.8,0.01)
pricespace_1=expand.grid(aux,aux)
uc=0.5
profitmat_2P=matrix(0L,nrow(pricespace_1),1)
for (i in 1:nrow(pricespace_1)){
    profitmat_2P[i]=sum(profit_2P(pricespace_1[i,1],pricespace_1[i,2],0.94)) 
}
opti_KB <- pricespace_1[,1][profitmat_2P==max(profitmat_2P)] #1.04
opti_KR<- pricespace_1[,2][profitmat_2P==max(profitmat_2P)] #1.04
max(profitmat_2P)

# Round 3 -- MB 
demand_MB_2 <- function(priceKB,priceKR,priceMB,para){
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(probMB)
}


profitMB=function(priceKB,priceKR,priceMB){
    profitMB=1000*(demand_MB_2(1.04,1.04,priceMB,as.numeric(coef[1,2:5]))*(priceMB-uc)*seg.share[1]+
                       demand_MB_2(1.04,1.04,priceMB,as.numeric(coef[2,2:5]))*(priceMB-uc)*seg.share[2]+
                       demand_MB_2(1.04,1.04,priceMB,as.numeric(coef[3,2:5]))*(priceMB-uc)*seg.share[3]+
                       demand_MB_2(1.04,1.04,priceMB,as.numeric(coef[4,2:5]))*(priceMB-uc)*seg.share[4])
    return(profitMB)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_MB=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_MB[i]=profitMB(1.04,1.04,aux[i]) 
}
opti_MB_2 <- aux[profitmat_MB==max(profitmat_MB)]
max(profitmat_MB) #0.91

# Round 4 -- KB & KR 
demand_2P <- function(priceKB,priceKR,priceMB,para){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR))
}

profit_2P=function(priceKB,priceKR,priceMB){
    profitKB=1000*(demand_2P(priceKB,priceKR,0.91,as.numeric(coef[1,2:5]))[,1]*(priceKB-uc)*seg.share[1]+
                       demand_2P(priceKB,priceKR,0.91,as.numeric(coef[2,2:5]))[,1]*(priceKB-uc)*seg.share[2]+
                       demand_2P(priceKB,priceKR,0.91,as.numeric(coef[3,2:5]))[,1]*(priceKB-uc)*seg.share[3]+
                       demand_2P(priceKB,priceKR,0.91,as.numeric(coef[4,2:5]))[,1]*(priceKB-uc)*seg.share[4])
    
    profitKR=1000*(demand_2P(priceKB,priceKR,0.91,as.numeric(coef[1,2:5]))[,2]*(priceKR-uc)*seg.share[1]+
                       demand_2P(priceKB,priceKR,0.91,as.numeric(coef[2,2:5]))[,2]*(priceKR-uc)*seg.share[2]+
                       demand_2P(priceKB,priceKR,0.91,as.numeric(coef[3,2:5]))[,2]*(priceKR-uc)*seg.share[3]+
                       demand_2P(priceKB,priceKR,0.91,as.numeric(coef[4,2:5]))[,2]*(priceKR-uc)*seg.share[4])
    return(cbind(profitKB,profitKR))
}

aux=seq(0.5,1.8,0.01)
pricespace_1=expand.grid(aux,aux)
uc=0.5
profitmat_2P=matrix(0L,nrow(pricespace_1),1)
for (i in 1:nrow(pricespace_1)){
    profitmat_2P[i]=sum(profit_2P(pricespace_1[i,1],pricespace_1[i,2],0.91)) 
}
opti_KB <- pricespace_1[,1][profitmat_2P==max(profitmat_2P)] #1.03
opti_KR<- pricespace_1[,2][profitmat_2P==max(profitmat_2P)] #1.03
max(profitmat_2P)

# Round 5 -- MB 
demand_MB_3 <- function(priceKB,priceKR,priceMB,para){
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(probMB)
}


profitMB=function(priceKB,priceKR,priceMB){
    profitMB=1000*(demand_MB_2(1.03,1.03,priceMB,as.numeric(coef[1,2:5]))*(priceMB-uc)*seg.share[1]+
                       demand_MB_3(1.03,1.03,priceMB,as.numeric(coef[2,2:5]))*(priceMB-uc)*seg.share[2]+
                       demand_MB_3(1.03,1.03,priceMB,as.numeric(coef[3,2:5]))*(priceMB-uc)*seg.share[3]+
                       demand_MB_3(1.03,1.03,priceMB,as.numeric(coef[4,2:5]))*(priceMB-uc)*seg.share[4])
    return(profitMB)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_MB=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
    profitmat_MB[i]=profitMB(1.03,1.03,aux[i]) 
}
opti_MB_3 <- aux[profitmat_MB==max(profitmat_MB)]
max(profitmat_MB) #0.91

demand_3P<- function(priceKB,priceKR,priceMB,para){
  probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
  return(cbind(probKB,probKR,probMB))
}
demand_3P(1.16,1.16,1.43,coef)


agg_choice <- function(priceKB,priceKR,priceMB) {
  agg_choice_KB=seg.share[1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[1,2:5]))[,1]+
    seg.share[2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[2,2:5]))[,1]+
    seg.share[3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[3,2:5]))[,1]+
    seg.share[4]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[4,2:5]))[,1]
  agg_choice_KR=seg.share[1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[1,2:5]))[,2]+
    seg.share[2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[2,2:5]))[,2]+
    seg.share[3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[3,2:5]))[,2]+
    seg.share[4]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[4,2:5]))[,2]
  agg_choice_MB=seg.share[1]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[1,2:5]))[,3]+
    seg.share[2]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[2,2:5]))[,3]+
    seg.share[3]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[3,2:5]))[,3]+
    seg.share[4]*demand_3P(priceKB,priceKR,priceMB,as.numeric(coef[4,2:5]))[,3]
  return(cbind(agg_choice_KB,agg_choice_KR,agg_choice_MB))
}

agg_choice(1.16,1.16,1.43)

demand_2P <- function(priceKR,priceMB,para){
  probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(cbind(probKR,probMB))
}


agg_choice <- function(priceKR,priceMB) {
  agg_choice_KR=seg.share[1]*demand_2P(priceKR,priceMB,as.numeric(coef2[1,2:4]))[,1]+
    seg.share[2]*demand_2P(priceKR,priceMB,as.numeric(coef2[2,2:4]))[,1]+
    seg.share[3]*demand_2P(priceKR,priceMB,as.numeric(coef2[3,2:4]))[,1]+
    seg.share[4]*demand_2P(priceKR,priceMB,as.numeric(coef2[4,2:4]))[,1]
  agg_choice_MB=seg.share[1]*demand_2P(priceKR,priceMB,as.numeric(coef[1,2:4]))[,2]+
    seg.share[2]*demand_2P(priceKR,priceMB,as.numeric(coef2[2,2:4]))[,2]+
    seg.share[3]*demand_2P(priceKR,priceMB,as.numeric(coef2[3,2:4]))[,2]+
    seg.share[4]*demand_2P(priceKR,priceMB,as.numeric(coef2[4,2:4]))[,2]
  return(cbind(agg_choice_KR,agg_choice_MB))
}
agg_choice(1.06,1.43)

## do not launch KB
# Round 1 -- MB 
demand_MB_1 <- function(priceKR,priceMB,para){
  probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probMB)
}


profitMB=function(priceKR,priceMB){
  profitMB=1000*(demand_MB_1(1.06,priceMB,as.numeric(coef2[1,2:4]))*(priceMB-uc)*seg.share[1]+
                   demand_MB_1(1.06,priceMB,as.numeric(coef2[2,2:4]))*(priceMB-uc)*seg.share[2]+
                   demand_MB_1(1.06,priceMB,as.numeric(coef2[3,2:4]))*(priceMB-uc)*seg.share[3]+
                   demand_MB_1(1.06,priceMB,as.numeric(coef2[4,2:4]))*(priceMB-uc)*seg.share[4])
  return(profitMB)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_MB=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
  profitmat_MB[i]=profitMB(1.06,aux[i]) 
}
opti_MB_1 <- aux[profitmat_MB==max(profitmat_MB)]
max(profitmat_MB) # 0.94

# Round 2 -- KR
demand_1P <- function(priceKR,priceMB,para){
  probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probKR)
}


profit_1P=function(priceKR,priceMB){
  profitKR=1000*(demand_1P(priceKR,0.94,as.numeric(coef2[1,2:4]))*(priceKR-uc)*seg.share[1]+
                   demand_1P(priceKR,0.94,as.numeric(coef2[2,2:4]))*(priceKR-uc)*seg.share[2]+
                   demand_1P(priceKR,0.94,as.numeric(coef2[3,2:4]))*(priceKR-uc)*seg.share[3]+
                   demand_1P(priceKR,0.94,as.numeric(coef2[4,2:4]))*(priceKR-uc)*seg.share[4])
  return(profitKR)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_KR=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
  profitmat_KR[i]=profit_1P(aux[i],0.94) 
}
opti_KR <- aux[profitmat_KR==max(profitmat_KR)] #0.93
max(profitmat_KR) 

# Round 3 -- MB 
demand_MB_1 <- function(priceKR,priceMB,para){
  probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probMB)
}


profitMB=function(priceKR,priceMB){
  profitMB=1000*(demand_MB_1(0.93,priceMB,as.numeric(coef2[1,2:4]))*(priceMB-uc)*seg.share[1]+
                   demand_MB_1(0.93,priceMB,as.numeric(coef2[2,2:4]))*(priceMB-uc)*seg.share[2]+
                   demand_MB_1(0.93,priceMB,as.numeric(coef2[3,2:4]))*(priceMB-uc)*seg.share[3]+
                   demand_MB_1(0.93,priceMB,as.numeric(coef2[4,2:4]))*(priceMB-uc)*seg.share[4])
  return(profitMB)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_MB=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
  profitmat_MB[i]=profitMB(0.93,aux[i]) 
}
opti_MB_1 <- aux[profitmat_MB==max(profitmat_MB)]
max(profitmat_MB) # 0.89

# Round 4 -- KR
demand_1P <- function(priceKR,priceMB,para){
  probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probKR)
}


profit_1P=function(priceKR,priceMB){
  profitKR=1000*(demand_1P(priceKR,0.89,as.numeric(coef2[1,2:4]))*(priceKR-uc)*seg.share[1]+
                   demand_1P(priceKR,0.89,as.numeric(coef2[2,2:4]))*(priceKR-uc)*seg.share[2]+
                   demand_1P(priceKR,0.89,as.numeric(coef2[3,2:4]))*(priceKR-uc)*seg.share[3]+
                   demand_1P(priceKR,0.89,as.numeric(coef2[4,2:4]))*(priceKR-uc)*seg.share[4])
  return(profitKR)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_KR=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
  profitmat_KR[i]=profit_1P(aux[i],0.89) 
}
opti_KR <- aux[profitmat_KR==max(profitmat_KR)] #0.92
max(profitmat_KR) 

# Round 5 -- MB 
demand_MB_1 <- function(priceKR,priceMB,para){
  probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(probMB)
}


profitMB=function(priceKR,priceMB){
  profitMB=1000*(demand_MB_1(0.92,priceMB,as.numeric(coef2[1,2:4]))*(priceMB-uc)*seg.share[1]+
                   demand_MB_1(0.92,priceMB,as.numeric(coef2[2,2:4]))*(priceMB-uc)*seg.share[2]+
                   demand_MB_1(0.92,priceMB,as.numeric(coef2[3,2:4]))*(priceMB-uc)*seg.share[3]+
                   demand_MB_1(0.92,priceMB,as.numeric(coef2[4,2:4]))*(priceMB-uc)*seg.share[4])
  return(profitMB)
}

aux=seq(0.5,1.8,0.01)
uc=0.5
profitmat_MB=matrix(0L,length(aux),1)
for (i in 1:length(aux)){
  profitmat_MB[i]=profitMB(0.92,aux[i]) 
}
opti_MB_1 <- aux[profitmat_MB==max(profitmat_MB)]
max(profitmat_MB) #0.89

demand_2P<- function(priceKR,priceMB,para){
  probKR=exp(para[1]+para[3]*priceKR)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  probMB=exp(para[2]+para[3]*priceMB)/(1+exp(para[1]+para[3]*priceKR)+exp(para[2]+para[3]*priceMB))
  return(cbind(probKR,probMB))
}

agg_choice <- function(priceKR,priceMB) {
  agg_choice_KR=seg.share[1]*demand_2P(priceKR,priceMB,as.numeric(coef2[1,2:4]))[,1]+
    seg.share[2]*demand_2P(priceKR,priceMB,as.numeric(coef2[2,2:4]))[,1]+
    seg.share[3]*demand_2P(priceKR,priceMB,as.numeric(coef2[3,2:4]))[,1]+
    seg.share[4]*demand_2P(priceKR,priceMB,as.numeric(coef2[4,2:4]))[,1]
  agg_choice_MB=seg.share[1]*demand_2P(priceKR,priceMB,as.numeric(coef2[1,2:4]))[,2]+
    seg.share[2]*demand_2P(priceKR,priceMB,as.numeric(coef2[2,2:4]))[,2]+
    seg.share[3]*demand_2P(priceKR,priceMB,as.numeric(coef2[3,2:4]))[,2]+
    seg.share[4]*demand_2P(priceKR,priceMB,as.numeric(coef2[4,2:4]))[,2]
  return(cbind(agg_choice_KR,agg_choice_MB))
}

agg_choice(0.92,0.89)



