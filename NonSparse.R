# clear memory
rm(list=ls())
# Clear all figures
graphics.off()
# Garbage Collect
gc()

# Examine non-sparsity first
ns<-100
xx<-mvrnorm(ns,Sigma=as.matrix(cbind(c(1,.9),c(.9,1))),mu=rep(0,2)) # x1 and x2 are correlated
xx<-cbind(xx,mvrnorm(ns,Sigma=as.matrix(cbind(c(1,.9),c(.9,1))),mu=rep(0,2))) # x3 och x4 ??r korrelerade
xx<-cbind(xx,mvrnorm(ns,Sigma=diag(rep(1,100)),mu=rep(0,100))) # resten, inklusive x5 ??r helt okorrelerade
# 104 variables, either 30, 60, or 90 contribute to the true model
y<- -1*xx[,1]-1*xx[,2]-1*xx[,3]-1*xx[,4]-1*xx[,5]+1*xx[,6]+1*xx[,7]+1*xx[,8]+1*xx[,9]+1*xx[,10]+
  -1*xx[,11]-1*xx[,13]-1*xx[,14]-1*xx[,15]+1*xx[,16]+1*xx[,17]+1*xx[,18]+1*xx[,19]+1*xx[,20]+
  -1*xx[,21]-1*xx[,23]-1*xx[,24]-1*xx[,25]+1*xx[,26]+1*xx[,27]+1*xx[,28]+1*xx[,29]+1*xx[,30]+
#  -1*xx[,31]-1*xx[,33]-1*xx[,34]-1*xx[,35]+1*xx[,36]+1*xx[,37]+1*xx[,38]+1*xx[,39]+1*xx[,40]+
#  -1*xx[,41]-1*xx[,43]-1*xx[,44]-1*xx[,45]+1*xx[,46]+1*xx[,47]+1*xx[,48]+1*xx[,49]+1*xx[,50]+
#  -1*xx[,51]-1*xx[,53]-1*xx[,54]-1*xx[,55]+1*xx[,56]+1*xx[,57]+1*xx[,58]+1*xx[,59]+1*xx[,60]+
#  -1*xx[,61]-1*xx[,63]-1*xx[,64]-1*xx[,65]+1*xx[,66]+1*xx[,67]+1*xx[,68]+1*xx[,69]+1*xx[,70]+
#  -1*xx[,71]-1*xx[,73]-1*xx[,74]-1*xx[,75]+1*xx[,76]+1*xx[,77]+1*xx[,78]+1*xx[,79]+1*xx[,80]+
#  -1*xx[,81]-1*xx[,83]-1*xx[,84]-1*xx[,85]+1*xx[,86]+1*xx[,87]+1*xx[,88]+1*xx[,89]+1*xx[,90]+
#  -1*xx[,91]-1*xx[,93]-1*xx[,94]-1*xx[,95]+1*xx[,96]+1*xx[,97]+1*xx[,98]+1*xx[,99]+1*xx[,100]+
  rnorm(ns)*1
df<-as.data.frame(cbind(xx,y))
names(df)<-c(as.character(seq(1,104)),"y")

####
# How well does lasso perform?
library(glmnet)

gg<-glmnet(x=xx,y=y)
plot(gg,xvar="lambda", main="p = 30")

cv.gg<-cv.glmnet(x=xx,y=y)
plot(cv.gg, main="p = 30")
names(cv.gg)

coef(cv.gg,s="lambda.1se")
coef(cv.gg,s="lambda.min")

# How well does adaptive lasso perform?
gamma<-1
ggr<-glmnet(x=xx,y=y,lambda=cv.gg$lambda.min,alpha=0)
xx2<-xx%*%diag(abs(coef(ggr)[-1])^gamma)
gg2<-glmnet(x=xx2,y=y)#,lambda=cv.gg$lambda.min)
cv.gg2<-cv.glmnet(x=xx2,y=y)
plot(gg2,xvar="lambda")
plot(cv.gg2, main="p = 30")


