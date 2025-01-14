#lec 3 code, K MEANS

library(dplyr)

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

#3 clusters
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
q = ggplot(y,aes(X1,X2))+
  geom_point(aes(shape=group,color=cluster))+
  geom_point(data = y %>% select(cluster) %>% mean())
  xlab("Feature 1")+ylab("Feature 2")+
  ggtitle("Results with K=3")+theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
q
