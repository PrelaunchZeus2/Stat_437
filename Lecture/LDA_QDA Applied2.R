#LDA_QDA Applied2
library(ISLR)
dim(Smarket)
Smarket[1,]
levels(Smarket$Direction)

#lda with 2 features, lag1 and lag2
train=(Smarket$Year<2005); Smarket.2005=Smarket[!train,]
Direction.2005=Smarket$Direction[!train]
library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
#both components are gaussian

dim(Smarket.2005)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

qda.fit=qda(Direction~Lag1+Lag2,data=Smarket,subset=train)
qda.fit

qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
