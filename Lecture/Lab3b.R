set.seed(2)
x=matrix(rnorm(20*2), ncol=2)
x[1:10,1]=x[1:10,1]+3; x[11:20,2]=x[1:10,2]-4

y = as.data.frame(x); y$obId = 1:20; y$group=as.factor(rep(1:2,each=10))
library(ggplot2)
ggplot(y,aes(V1,V2))+theme_bw()+guides(color = FALSE)+
  geom_label(aes(label=obId,color=group),nudge_x = 0.05)

## Example 1: clustering

hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
abline(h=2.5, col="red")
plot(hc.average, main="Average Linkage",xlab="",sub="",cex=.9)
abline(h=2.5, col="red")
plot(hc.single, main="Single Linkage",xlab="",sub="",cex=.9)
abline(h=2.5, col="red")

hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3),mar = c(3,2.5,2.4,1.4))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
abline(h=2.5, col="red")
plot(hc.average, main="Average Linkage",xlab="",sub="",cex=.9)
abline(h=2.5, col="red")
plot(hc.single, main="Single Linkage",xlab="",sub="",cex=.9)
abline(h=2.5, col="red")


hc.complete$height
min(dist(x)) == min(hc.complete$height)
max(hc.complete$height) == max(dist(x))


hc.average$height
min(dist(x)) == min(hc.average$height)
max(hc.average$height)
max(dist(x))


hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage",xlab="",sub="",cex=.9)
abline(h=max(hc.complete$height), col="red",lty="dotted")
plot(hc.average, main="Average Linkage",xlab="",sub="",cex=.9)
abline(h=max(hc.average$height), col="red",lty="dotted")
plot(hc.single, main="Single Linkage",xlab="",sub="",cex=.9)
abline(h=max(hc.single$height), col="red",lty="dotted")


cutree(hc.complete,2)
cutree(hc.average,2)
cutree(hc.single,2)


hc.average$height
clngrp = cutree(hc.average,k=3)
clngrp
nh = length(hc.average$height)
clhgt = cutree(hc.average,h=hc.average$height[nh-2])
clhgt
all.equal(clngrp,clhgt)

xsc=scale(x) # standardize each column (i.e., feature)
par(mfrow=c(1,1),mar = c(7,4.5,0,1.4))
plot(hclust(dist(xsc),method="complete"),xlab="",sub="",
     main="",ylab="Height")

par(mfrow=c(1,2),mar = c(3,2.5,2.4,2))
plot(hclust(dist(xsc), method="complete"), xlab="",sub="",
     main="HC via scaled features")
plot(hclust(dist(x), method="complete"), xlab="",sub="",
     main="HC via raw features")

## Example 1a: corr.-based distance


set.seed(1); x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x))) #corr.-based distance

plot(hclust(dd, method="complete"), xlab="", sub="",
     main="Complete Linkage with Correlation-Based Distance")

# bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1)
par(mfrow=c(1,2),mar = c(3,2.5,2.4,1))
plot(hclust(dd, method="complete"), xlab="", sub="",
     main="corr.-dased diss.")
abline(h=1.5,col="red")
plot(hclust(dist(x)/(max(dist(x))/max(dd)), method="complete"), xlab="", sub="",
     main="Euclidean diss.")
abline(h=1.5,col="red")


# HC software implementation: Example 2

## Example 2: data


library(ISLR)
nci.data=NCI60$data; nci.labs=NCI60$labs
dim(nci.data)
nci.labs[1:4]
table(nci.labs)


sd.data=scale(nci.data) # standardize data
# Euclidean norm as pairwise dissimiliarty
data.dist=dist(sd.data)
plot(hclust(data.dist, method="average"), labels=nci.labs,
     xlab="", sub="",ylab="", main="Average Linkage")

sd.data=scale(nci.data) # standardize data
# Euclidean norm as pairwise dissimiliarty
data.dist=dist(sd.data)
par(mfrow=c(1,1),mar = c(1,2.5,2.4,1.4))
# plot(hclust(data.dist), labels=nci.labs,
#     xlab="", sub="", ylab="", main="Complete Linkage")
plot(hclust(data.dist, method="average"), labels=nci.labs,
     xlab="", sub="",ylab="", main="HC with Average Linkage")
# plot(hclust(data.dist, method="single"), labels=nci.labs,
#     xlab="", sub="",ylab="", main="Single Linkage")

hc.al=hclust(dist(sd.data),method="average")
hcal.clusters=cutree(hc.al,4)
table(hcal.clusters,nci.labs)

cutheight=hc.al$height[length(hc.al$height)-3]
all.equal(cutree(hc.al,h=cutheight),cutree(hc.al,4))
par(mfrow=c(1,1),mar = c(3.5,2.5,1.4,1.4))
plot(hc.al, labels=nci.labs,xlab="", sub="",main="")
abline(h=cutheight, col="red",lty="dotted")

# HC software implementation: Example 3

## Example 3: data

library(ElemStatLearn) # library containing data
data(nci); n = dim(nci)[2]; p = dim(nci)[1] #get dimensions
set.seed(123)
rSel = sample(1:p, size=100, replace = FALSE)
cSel = sample(1:n, size=30, replace = FALSE)
nci_a = nci[rSel,cSel]
colnames(nci_a) = colnames(nci)[cSel]

head(nci_a[1:3,1:5])

# Euclidean norm as pairwise dissimilarity
# applied to scaled data
dMat = dist(scale(t(nci_a))) 
length(dMat)

## Example 3: average linkage
EHC_al = hclust(dMat, method = "average")
library(ggdendro)
ggdendrogram(EHC_al, rotate = F)

## Example 3: single linkage
EHC_SL = hclust(dMat, method = "single")
library(ggdendro)
ggdendrogram(EHC_SL, rotate = F)

## Example 3: complete linkage
EHC_CL = hclust(dMat, method = "complete")
library(ggdendro)
ggdendrogram(EHC_CL, rotate = F)

library(ElemStatLearn); library(ggplot2)
source("Plotggdendro.r")
cutheight = EHC_al$height[length(EHC_al$height)-3]
droplot = plot_ggdendro(dendro_data_k(EHC_al, 3),
                        direction = "tb",heightReferece=cutheight,expand.y = 0.2)
