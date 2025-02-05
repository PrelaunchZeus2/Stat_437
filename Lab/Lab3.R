## cluster simulated data

set.seed(2); x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3; x[1:25,2]=x[1:25,2]-4

km.out=kmeans(x,2,nstart=20)
# creat new data.frame with "group" and "cluster"
y = data.frame(x); colnames(y)=c("X1","X2")
y$group = factor(rep(c(1,2),each=25))
y$cluster=factor(km.out$cluster)
library(ggplot2)
p = ggplot(y,aes(X1,X2))+
  geom_point(aes(shape=group,color=cluster))+
  xlab("Feature 1")+ylab("Feature 2")+
  ggtitle("Results with K=2")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

p


### 3 clusters

set.seed(2); x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3; x[1:25,2]=x[1:25,2]-4


set.seed(4); km.out3 =kmeans(x,3,nstart=20)
# show the 3 centroids
km.out3$centers
# creat new data.frame with "group" and "cluster"
y = data.frame(x); colnames(y)=c("X1","X2")
y$group = factor(rep(c(1,2),each=25))
y$cluster=factor(km.out3$cluster)
library(ggplot2)
p = ggplot(y,aes(X1,X2))+
  geom_point(aes(shape=group,color=cluster))+
  xlab("Feature 1")+ylab("Feature 2")+
  ggtitle("Results with K=3")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))


p


## Effect of "nstart"

set.seed(2); x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3; x[1:25,2]=x[1:25,2]-4
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss


# Example 2: cluster "iris" data

## Clustering via 2 features

library(ggplot2)
head(iris)

library(ggplot2); set.seed(3.14)
# use `Sepal.Length` and `Sepal.Width`
km.out=kmeans(iris[,1:2],3,nstart=20)
# augment "iris" with "cluster"
iris$cluster=factor(km.out$cluster)
library(ggplot2)
p = ggplot(iris,aes(Sepal.Length,Sepal.Width))+
  geom_point(aes(shape=Species,color=cluster))+
  theme_bw()+ggtitle("3-means clustering via 2 features")

p

## Clustering via 4 features

library(ggplot2); set.seed(3.14)
# use `Sepal.Length` and `Sepal.Width` but not "Species"
km.out=kmeans(iris[,1:4],3,nstart=20)
# augment "iris" with "cluster"
iris$cluster=factor(km.out$cluster)
library(ggplot2)
p1 = ggplot(iris,aes(Sepal.Length,Sepal.Width))+
  geom_point(aes(shape=Species,color=cluster))+
  theme_bw()+ggtitle("3-means clustering via 4 features")

p1

## A comparison

library(gridExtra); grid.arrange(p,p1,nrow=1)
# Note: cluster numbering is non-essential


## Implement "gap statistic":

library(cluster); set.seed(3.14) 
gap = clusGap(iris[,1:2], kmeans, K.max=10, B=150,
              nstart=20, iter.max = 20)
gap$Tab[1:2,3:4]
# Use "1-SE rule" and the "Tibs2001SEmax" method
k = maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], 
          method="Tibs2001SEmax")
k # k is the estimated number of clusters


library(cluster); set.seed(3.14) 
# same `kmeans` settings
gap = clusGap(iris[,1:4], kmeans, K.max=10, B=150,
              nstart=20, iter.max = 20)
# Use "1-SE rule" and the "Tibs2001SEmax" method
k = maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], 
          method="Tibs2001SEmax")
k


# Example 3: cluster human cancer data

library(ElemStatLearn); data(nci) # load library and data
# set seed to make results of `sample` reproducible
set.seed(123)
# sample "rows" without replacement
rSel = sample(1:dim(nci)[1], size=100, replace = FALSE)
# extract the selected 100 rows, i.e., 100 genes
nci_a = nci[rSel,]
colnames(nci_a) = colnames(nci)
# pick observations for 4 cancer types
A1 = which(colnames(nci_a)=="BREAST")
A2 = which(colnames(nci_a)=="COLON")
A3 = which(colnames(nci_a)=="PROSTATE")
A4 = which(colnames(nci_a)=="LEUKEMIA")
nci_a1 = nci_a[,c(A1,A2,A3,A4)] 


length(A1) # number of ob's for "BREAST" cancer
length(A2) # number of ob's for "COLON" cancer
length(A3) # number of ob's for "PROSTATE" cancer
length(A4) # number of ob's for "LEUKEMIA" cancer


nci_b = t(nci_a1)
set.seed(123)
clIdx = kmeans(nci_b, 4, iter.max=30, nstart=20)

nci_b = data.frame(nci_b)
nci_b$Type = factor(colnames(nci_a1))
nci_b$cluster = factor(clIdx$cluster)
library(ggplot2)
p3 = ggplot(nci_b,aes(X1,X2))+xlab("Gene 1 expression")+
  ylab("Gene 2 expression")+theme_bw()+
  geom_point(aes(shape=Type,color=cluster),na.rm = T)+
  theme(legend.position="right")+
  ggtitle("Clustering via 100 features")+
  theme(plot.title = element_text(hjust = 0.5))


p3

## Clustering "nci_a1": K=2

nci_b1 = t(nci_a1)
set.seed(123)
clIdxB = kmeans(nci_b1, 2, iter.max=30, nstart=20)
clIdxB$cluster

nci_b1 = data.frame(nci_b1)
nci_b1$Type = factor(colnames(nci_a1))
nci_b1$cluster = factor(clIdxB$cluster)
library(ggplot2)
p4 = ggplot(nci_b1,aes(X1,X2))+xlab("Gene 1 expression")+
  ylab("Gene 2 expression")+theme_bw()+
  geom_point(aes(shape=Type,color=cluster),na.rm = T)+
  theme(legend.position="right")+
  ggtitle("Clustering via 100 features")+
  theme(plot.title = element_text(hjust = 0.5))
p4


## Clustering via more features


library(ElemStatLearn)
data(nci) # load data
# set seed to make results of `sample` reproducible
set.seed(123)
# sample without replacement
rSel = sample(1:dim(nci)[1], size=1000, replace = FALSE)
# extract the selected 100 rows, i.e., 100 genes
nci_a = nci[rSel,]
colnames(nci_a) = colnames(nci)
A1 = which(colnames(nci_a)=="BREAST")
A2 = which(colnames(nci_a)=="COLON")
A3 = which(colnames(nci_a)=="PROSTATE")
A4 = which(colnames(nci_a)=="LEUKEMIA")
nci_a2 = nci_a[,c(A1,A2,A3,A4)] 

nci_b2 = t(nci_a2)
set.seed(123)
clIdxA = kmeans(nci_b2, 4, iter.max = 30,nstart=20)
nci_b2 = data.frame(nci_b2)
nci_b2$Type = factor(colnames(nci_a2))
nci_b2$cluster = factor(clIdxA$cluster)
library(ggplot2)
p5 = ggplot(nci_b2,aes(X1,X2))+xlab("Gene 1 expression")+
  ylab("Gene 2 expression")+theme_bw()+
  geom_point(aes(shape=Type,color=cluster),na.rm = T)+
  theme(legend.position="right")+
  ggtitle("Clustering via 1,000 features")+
  theme(plot.title = element_text(hjust = 0.5))


## Visual comparison

library(gridExtra); grid.arrange(p3,p5,nrow=1)


## Standardization and clustering


nci_d = scale(t(nci_a2),center = TRUE, scale = TRUE)
nci_d = data.frame(nci_d)
set.seed(123)
clIdxC=kmeans(nci_d,4,iter.max=30,nstart=20)
nci_d$Type = factor(colnames(nci_a2))
nci_d$cluster = factor(clIdxC$cluster)
library(ggplot2)
p6 = ggplot(nci_d,aes(X1,X2))+xlab("Gene 1 expression")+
  ylab("Gene 2 expression")+theme_bw()+
  geom_point(aes(shape=Type,color=cluster),na.rm = T)+
  theme(legend.position="right")+
  ggtitle("Clustering via 1,000 std features")+
  theme(plot.title = element_text(hjust = 0.5))


library(gridExtra); grid.arrange(p5,p6,nrow=1)


## Estimate number of clusters

library(cluster) 
gap <- clusGap(t(nci_a1), kmeans, K.max=10, B=200,
               iter.max=30, nstart=20)
k <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], 
           method="Tibs2001SEmax")
k
# k is the estimated number of clusters



