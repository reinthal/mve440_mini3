# clear memory
rm(list=ls())
# Clear all figures
graphics.off()
# Garbage Collect
gc()

set.seed(100)
# Examine non-sparsity first
ns<-100
#xx<-mvrnorm(ns,Sigma=as.matrix(cbind(c(1,.9),c(.9,1))),mu=rep(0,2)) # x1 and x2 are correlated
#xx<-cbind(xx,mvrnorm(ns,Sigma=as.matrix(cbind(c(1,.9),c(.9,1))),mu=rep(0,2))) # x3 och x4 ??r korrelerade
xx<-mvrnorm(ns,Sigma=diag(rep(1,100)),mu=rep(0,100)) # resten, inklusive x5 ??r helt okorrelerade
# 100 variables, either 30, 60, or 90 contribute to the true model
nTrueVar = 60;
lambdaMinRatio = 1e-10;
y<- -1*xx[,1]-1*xx[,2]-1*xx[,3]-1*xx[,4]-1*xx[,5]+1*xx[,6]+1*xx[,7]+1*xx[,8]+1*xx[,9]+1*xx[,10]+
  -1*xx[,11]-1*xx[,13]-1*xx[,14]-1*xx[,15]+1*xx[,16]+1*xx[,17]+1*xx[,18]+1*xx[,19]+1*xx[,20]+
  -1*xx[,21]-1*xx[,23]-1*xx[,24]-1*xx[,25]+1*xx[,26]+1*xx[,27]+1*xx[,28]+1*xx[,29]+1*xx[,30]+
  -1*xx[,31]-1*xx[,33]-1*xx[,34]-1*xx[,35]+1*xx[,36]+1*xx[,37]+1*xx[,38]+1*xx[,39]+1*xx[,40]+
  -1*xx[,41]-1*xx[,43]-1*xx[,44]-1*xx[,45]+1*xx[,46]+1*xx[,47]+1*xx[,48]+1*xx[,49]+1*xx[,50]+
  -1*xx[,51]-1*xx[,53]-1*xx[,54]-1*xx[,55]+1*xx[,56]+1*xx[,57]+1*xx[,58]+1*xx[,59]+1*xx[,60]+
#  -1*xx[,61]-1*xx[,63]-1*xx[,64]-1*xx[,65]+1*xx[,66]+1*xx[,67]+1*xx[,68]+1*xx[,69]+1*xx[,70]+
#  -1*xx[,71]-1*xx[,73]-1*xx[,74]-1*xx[,75]+1*xx[,76]+1*xx[,77]+1*xx[,78]+1*xx[,79]+1*xx[,80]+
#  -1*xx[,81]-1*xx[,83]-1*xx[,84]-1*xx[,85]+1*xx[,86]+1*xx[,87]+1*xx[,88]+1*xx[,89]+1*xx[,90]+
#  -1*xx[,91]-1*xx[,93]-1*xx[,94]-1*xx[,95]+1*xx[,96]+1*xx[,97]+1*xx[,98]+1*xx[,99]+1*xx[,100]+
  rnorm(ns)*1
df<-as.data.frame(cbind(xx,y))
names(df)<-c(as.character(seq(1,100)),"y")

####
# How well does lasso perform?
library(glmnet)

gg<-glmnet(x=xx,y=y, lambda.min.ratio = lambdaMinRatio)
plot(gg,xvar="lambda", main=sprintf("Lasso, %i true features", nTrueVar))

cv.gg<-cv.glmnet(x=xx,y=y, lambda.min.ratio = lambdaMinRatio)
plot(cv.gg, main=sprintf("Lasso, %i true features", nTrueVar))
names(cv.gg)

coeffs<-as.matrix(coef(cv.gg, s="lambda.1se"))
print(nExcludedTrues<-sum(coeffs[(2:nTrueVar+1),1]==0))

# How well does adaptive lasso perform?
gamma<-1
ggr<-glmnet(x=xx,y=y,lambda=cv.gg$lambda.min,alpha=0)
xx2<-xx%*%diag(abs(coef(ggr)[-1])^gamma)
gg2<-glmnet(x=xx2,y=y, lambda.min.ratio = lambdaMinRatio)#,lambda=cv.gg$lambda.min)
cv.gg2<-cv.glmnet(x=xx2,y=y, lambda.min.ratio = lambdaMinRatio)
plot(gg2,xvar="lambda",main=sprintf("Adaptive lasso, %i true features", nTrueVar))
plot(cv.gg2, main="p = 30",main=sprintf("Adaptive lasso, %i true features", nTrueVar))

coeffs2<-as.matrix(coef(cv.gg2, s="lambda.1se"))
print(nExcludedTrues<-sum(coeffs2[(2:nTrueVar+1),1]==0))

# Elastic net, performance?
cv.gg1<-cv.glmnet(x=xx,y=y,alpha=1, lambda.min.ratio = lambdaMinRatio)
cv.gg.5<-cv.glmnet(x=xx,y=y,alpha=.5, lambda.min.ratio = lambdaMinRatio)
cv.gg.25<-cv.glmnet(x=xx,y=y,alpha=.25, lambda.min.ratio = lambdaMinRatio)

plot(log(cv.gg1$lambda),cv.gg1$cvm,xlab="Log(Lambda)",ylab="MSE", main=sprintf("Elastic net, %i true features", nTrueVar))
points(log(cv.gg.5$lambda),cv.gg.5$cvm,col=2)
points(log(cv.gg.25$lambda),cv.gg.25$cvm,col=3)
legend("topleft",legend=c("alpha=1","alpha=.5","alpha=.25"),pch=1,col=c("black","red","green"))

####################### TEST CODE BELOW; TRYING TO FIND THE GROUP LASSO ############
library(lasso2)
library(lqa)
data("Iowa")
mm<-lm(Yield~., data=Iowa)
plot.lqa(y=Iowa[,10],x=as.matrix(Iowa[,-10]),lambdaseq=seq(.1,10,by=.1),
         family=gaussian, penalty.family=lasso)
mlasso.cv<-cv.lqa(y.train=Iowa[,10],x.train=as.matrix(Iowa[,-10]),lambda.candidates=list(seq(.1,25,by=.1)),
                  family=gaussian, penalty.family=lasso,n.fold=10)
print(mlasso.cv)

mlasso<-lqa(Yield~.,data=Iowa,penalty=lasso(lambda=.7))
plot(mm$coef[-1],mlasso$coefficients[-1],xlab="LS coefficients",ylab="Regularized fit",
     xlim=c(min(mm$coef[-1]),max(mm$coef[-1])),ylim=c(min(mm$coef[-1]),max(mm$coef[-1])))
abline(0,1)
malasso<-lqa(Yield~.,data=Iowa,penalty=adaptive.lasso(lambda=.7,al.weights=(abs(round(mm$coefficients[-1],5))^.25)))
points(mm$coef[-1],malasso$coefficients[-1],col=5,pch=2)

####

library(glmnet)

gg<-glmnet(x=as.matrix(Iowa[,-10]),y=Iowa[,10])
plot(gg,xvar="lambda")
vnat<-coef(gg)
vnat<-vnat[-1,ncol(vnat)]
axis(2,at=vnat,line=-.5,label=names(Iowa[,-10]),las=1,tick=FALSE,cex.axis=.5)

cv.gg<-cv.glmnet(x=as.matrix(Iowa[,-10]),y=Iowa[,10])
plot(cv.gg)
names(cv.gg)

coef(cv.gg,s="lambda.1se")
coef(cv.gg,s="lambda.min")



cv.gg1<-cv.glmnet(x=as.matrix(Iowa[,-10]),y=Iowa[,10],alpha=1)
cv.gg.5<-cv.glmnet(x=as.matrix(Iowa[,-10]),y=Iowa[,10],alpha=.5)
cv.gg.25<-cv.glmnet(x=as.matrix(Iowa[,-10]),y=Iowa[,10],alpha=.25)

plot(log(cv.gg1$lambda),cv.gg1$cvm,xlab="Log(Lambda)",ylab="MSE")
points(log(cv.gg.5$lambda),cv.gg.5$cvm,col=2)
points(log(cv.gg.25$lambda),cv.gg.25$cvm,col=3)
legend("topleft",legend=c("alpha=1","alpha=.5","alpha=.25"),pch=1,col=c("black","red","green"))

#####



########
library(hdi)
ll<-lasso.proj(x=as.matrix(Iowa[,-10]),y=Iowa[,10]) # de-sparsified lasso
ll$pval
ll$pval.corr
confint(ll)



cc<-ll$clusterGroupTest()
plot(cc)
cc$pval
cc$clusters
cor(Iowa[,-10])



mm<-multi.split(x=as.matrix(Iowa[,-10]),y=Iowa[,10],return.nonaggr=T)
hist(mm$pvals.nonaggr[,9],20)




