set.seed(2)
gp=rbinom(n=10, size=1, prob=0.5)
gp1=which(gp==0); gp2=which(gp==1)
x=matrix(rnorm(10*2), ncol=2)
x[gp1,1]=x[gp1,1]+3; x[gp2,2]=x[gp2,2]-4 #generate two separate groups for two different classes

y = as.data.frame(x); y$obId = 1:10; y$group=as.factor(gp) #coerce into a dataframe add column with observation ID
pointdata = as.data.frame(rbind(c(4,1),c(3.5,0))
colnames(pointdata) = c("V1","V2")
library(ggplot2)
ggplot(y,aes(V1,V2))+theme_bw()+guides(color = FALSE)+
  geom_label(aes(label=obId,color=group),nudge_x = 0.05)+
  geom_point(data = pointdata, aes(V1, V2,size=2))+guides(size = FALSE)
##################### start of codes for Lecture Notes 4b
## The two-group model
set.seed(2)
gp=rbinom(n=10, size=1, prob=0.5)
gp1=which(gp==0); gp2=which(gp==1)
x=matrix(rnorm(10*2), ncol=2)
x[gp1,1]=x[gp1,1]+3; x[gp2,2]=x[gp2,2]-4
colnames(x)=c("V1","V2")
y = as.data.frame(x); y$obId = 1:10; y$Class=as.factor(gp+1)
y
library(ggplot2)
ggplot(y,aes(V1,V2))+theme_bw()+guides(color = FALSE)+
  geom_label(aes(label=obId,color=Class))
## Obtain posterior probability
library(mvtnorm) # compute multivariate Gaussian probabilities
postPClass = matrix(0,ncol=2,nrow=10) # store prosterior probabilities
for (i in 1:10) {
  cndp1 = dmvnorm(x[i,], mean =c(3,0), sigma = diag(2))
  cndp2 = dmvnorm(x[i,], mean =c(0,-4), sigma = diag(2))
  fmarginal = 0.5*cndp1 +0.5*cndp2
  jp1=0.5*cndp1; jp2=0.5*cndp2
  postp1=jp1/fmarginal; postp2=jp2/fmarginal
  postPClass[i,]=c(postp1,postp2)
}
colnames(postPClass)=c("PPCL1","PPCL2")
postPClass
library(mvtnorm)
postPClass = matrix(0,ncol=2,nrow=10)
for (j in 1:10) {
  cndp1 = dmvnorm(x[j,], mean =c(3,0), sigma = diag(2))
  cndp2 = dmvnorm(x[j,], mean =c(0,-4), sigma = diag(2))
  fmarginal = 0.5*cndp1 +0.5*cndp2
  jp1=0.5*cndp1; jp2=0.5*cndp2
  postp1=jp1/fmarginal; postp2=jp2/fmarginal
  postPClass[j,]=c(postp1,postp2)
}
colnames(postPClass)=c("PPCL1","PPCL2")
postPClass
postPClass=data.frame(postPClass)
postPClass$TrueClass = rep(1,10)
postPClass$TrueClass[gp2]=2 #true class labels
postPClass$EstClass=rep(1,10)
# compare posterior probabilities
comp = (postPClass$PPCL1 >= postPClass$PPCL2)
postPClass$EstClass[comp==TRUE]=1 # assign Class 1
postPClass$EstClass[comp==FALSE]=2 # assign Class 2
postPClass
postPClass=data.frame(postPClass)
postPClass$TrueClass = rep(1,10)
postPClass$TrueClass[gp2]=2
postPClass$EstClass=rep(1,10)
comp = (postPClass$PPCL1 >= postPClass$PPCL2)
postPClass$EstClass[comp==TRUE]=1
postPClass$EstClass[comp==FALSE]=2
EstClass=postPClass$EstClass; TrueClass=postPClass$TrueClass
postPClass
# k-nearest neighbor (kNN) classifiers: software implementation
set.seed(1); x=matrix(rnorm(30*3), ncol=3)
colnames(x) = c("X1","X2","X3"); x = as.data.frame(x)
x$class= rep(1,nrow(x))
library(ggplot2)
ggplot(data=x,aes(X1,X2,color=X3))+geom_point()+theme_bw()
trainingSet =x[1:20,1:3]; trainingLabels =x[1:20,4]
testSet =x[21:30,1:3]; testLabels =x[21:30,4]
library(class)
knn1 = knn(trainingSet,testSet, cl=trainingLabels, k = 1)
table(knn1,testLabels)
knn3 = knn(trainingSet,testSet, cl=trainingLabels, k = 3)
table(knn3,testLabels)
library(class)
knn10 = knn(trainingSet,testSet, cl=trainingLabels, k = 10)
table(knn10,testLabels)
knn30 = knn(trainingSet,testSet, cl=trainingLabels, k = 30)
table(knn30,testLabels)
## kNN: k bigger than sample size
library(class)
knn(trainingSet,testSet, cl=trainingLabels, k = 31)
knn(trainingSet,testSet, cl=trainingLabels, k = 40)
# k-nearest neighbor (kNN) classifiers: Example 2
## Human cancer microarray data
library(ElemStatLearn) # library containing data
data(nci); n = dim(nci)[2]; p = dim(nci)[1] #get dimensions
set.seed(123)
rSel = sample(1:p, size=50, replace = FALSE)
chk = colnames(nci) %in% c("BREAST", "LEUKEMIA", "COLON")
cSel = which(chk ==TRUE)
ncia = nci[rSel,cSel]
colnames(ncia) = colnames(nci)[cSel]
ncib=data.frame(t(ncia)); ncib$Class=colnames(ncia)
dim(ncib)
head(ncib[1:3,1:5])
library(ggplot2)
ggplot(ncib,aes(X1,X2,color=Class))+geom_point()+theme_bw()
ncibsd=scale(ncib[,1:50]); set.seed(1)
rTrain= base::sample(1:nrow(ncibsd),0.6*nrow(ncibsd))
rTest =(1:nrow(ncibsd))[-rTrain]
trainSet =ncibsd[rTrain,]; testSet =ncibsd[rTest,]
trainLabels=ncib$Class[rTrain]; testLabels=ncib$Class[rTest]
library(class)
knn2eg2= knn(trainSet,testSet, cl=trainLabels, k = 2)
length(testLabels)
testLabels
table(knn2eg2,testLabels)
# classification error
sum(1- as.numeric(knn2eg2==testLabels))/length(testLabels)
### k=4
library(class)
knn4eg2= knn(trainSet,testSet, cl=trainLabels, k = 4)
length(testLabels)
testLabels
table(knn4eg2,testLabels)
# classification error
sum(1- as.numeric(knn4eg2==testLabels))/length(testLabels) #easieest way too compute classification error

# k-nearest neighbor (kNN) classifiers: Example 3
library(class); dim(iris)
iris[1,]
unique(iris$Species)
library(class); set.seed(314) # seed needed!!
trainId=c(sample(1:50,40),sample(51:100,40),
          sample(101:150,40))
testId = (1:150)[-trainId]
trainingSet=iris[trainId,1:4]; testSet=iris[testId,1:4]
trainingLabs=iris$Species[trainId]
testLabs=iris$Species[testId]
m=10; set.seed(123) # seed needed!!
nrow(trainingSet)
folds=sample(1:m,nrow(trainingSet),replace=TRUE)
folds[1:10]
which(folds==1) # obs. id's in fold 1
table(folds)
k=2 # k for kNN; 
m=10
testError1 = double(m) # store test error for each fold s
for (s in 1:m) { # loop through s=1,...,m
  trainingTmp =trainingSet[folds !=s,]
  testTmp =trainingSet[folds==s,]
  trainingLabsTmp =trainingLabs[folds !=s]
  testLabsTmp =trainingLabs[folds==s]
  knn2= knn(trainingTmp,testTmp,trainingLabsTmp,k)
  nOfMissObs= sum(1-as.numeric(knn2==testLabsTmp))
  terror=nOfMissObs/length(testLabsTmp) # test error
  testError1[s]=terror
} # end of loop
mean(testError1)
sd(testError1)
kmax=20 # m=10 fold cv
testErrors = matrix(0,nrow=2,ncol=kmax)
for (k in 1:kmax) { # loop through k
  testError1 = double(m) # store test errors for each k
  for (s in 1:m) { # loop through s
    trainingTmp =trainingSet[folds !=s,]
    testTmp =trainingSet[folds==s,]
    trainingLabsTmp =trainingLabs[folds !=s]
    testLabsTmp =trainingLabs[folds==s]
    knntmp= knn(trainingTmp,testTmp,trainingLabsTmp,k)
    nOfMissObs= sum(1-as.numeric(knntmp==testLabsTmp))
    terror=nOfMissObs/length(testLabsTmp) # test error
    testError1[s]=terror } # loop in s ends
  testErrors[,k]=c(mean(testError1),sd(testError1))
} # loop in k ends
colnames(testErrors)= paste("k=",1:kmax,sep="")
rownames(testErrors)=c("mean(TestError)","sd(TestError)")
testErrors=as.data.frame(testErrors)
as.numeric(testErrors[1,])
hatk=which(testErrors[1,]==min(testErrors[1,]))
hatk
## Apply optimal kNN classifier
library(class)
knnOpt= knn(trainingSet,testSet,trainingLabs,hatk)
nOfMissObs1= sum(1-as.numeric(knnOpt==testLabs))
terrorOpt=nOfMissObs1/length(testLabs) # test error
terrorOpt
table(knnOpt,testLabs)
testSet$Species=testLabs
testSet$EstimatedSpecies=knnOpt
library(ggplot2)
cp =ggplot(testSet,aes(Sepal.Length,Petal.Width))+
  geom_point(aes(shape=EstimatedSpecies,color=Species))+
  theme_bw()
cp
