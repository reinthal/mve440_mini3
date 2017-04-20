# clear memory
rm(list=ls())
# Garbage Collect
gc()

library(lasso2)
library(glmnet)
library(lqa)
library(bestglm)

data("Iowa", package = "lasso2")
yDat <- Iowa[,10]
xDat <- as.matrix(Iowa[,-10])

# Ordinary lasso
gg<-glmnet(x=xDat,y=yDat)
plot(gg,xvar="lambda")

cv.gg<-cv.glmnet(x=xDat,y=yDat)
plot(cv.gg)
names(cv.gg)

coef(cv.gg,s="lambda.1se")
coef(cv.gg,s="lambda.min")

# Adaptive lasso
gamma<-1
ggl<-glmnet(x=xDat,y=yDat,lambda=cv.gg$lambda.min)
ggr<-glmnet(x=xDat,y=yDat,lambda=cv.gg$lambda.min,alpha=0)
xDat2<-xDat%*%diag(abs(coef(ggr)[-1])^gamma)
gg2<-glmnet(x=xDat2,y=yDat,lambda=cv.gg$lambda.min)
cc<-coef(gg2)[-1]/coef(ggr)[-1]

plot(cv.gg)
coef(ggl)#[2:10]
coef(gg2)#[2:10]

# Plots from 7-1, using a different package for lasso
mm<-lm(Yield~.,data=Iowa)

mlasso<-lqa(Yield~.,data=Iowa,penalty=lasso(lambda=1))
malasso<-lqa(Yield~.,data=Iowa,penalty=adaptive.lasso(lambda=1,al.weights=abs(mm$coef[-1])))

plot(mm$coef[-1],mlasso$coefficients[-1],xlab="LS coefficients",ylab="Regularized fit",
     xlim=c(min(mm$coef[-1]),max(mm$coef[-1])),ylim=c(min(mm$coef[-1]),max(mm$coef[-1])))
abline(0,1)
points(mm$coef[-1],malasso$coefficients[-1],col=2)
# effect of lasso and adaptive penalties

malasso<-lqa(Yield~.,data=Iowa,penalty=adaptive.lasso(lambda=1,al.weights=(abs(mm$coefficients[-1]))^2))
points(mm$coef[-1],malasso$coefficients[-1],col=4)

