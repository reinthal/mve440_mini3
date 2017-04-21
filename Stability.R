# clear memory
rm(list=ls())
# Garbage Collect
gc()

library(lasso2)
library(glmnet)
library(lqa)
library(bestglm)

# sample(1:10,3,replace = FALSE)
# Cook dome data

ns<-100
var1 <- 1
covar1 <- 0.9
sigma_matrix <- as.matrix( cbind(c(var1,covar1) ,c(covar1,var1) ))
mus <- rep(0,2)

xx<-mvrnorm( 
  ns,
  Sigma = sigma_matrix,
  mu = mus )

xx <- cbind(xx,
            mvrnorm(ns,
                    Sigma=sigma_matrix,
                    mu=mus )
)
xx <- cbind( xx, 
             mvrnorm( ns, Sigma=diag(rep(1,100)), mu=rep(0,100) ) 
)

# True betas
bs <- as.matrix( c( c(-1,0,1,1,1), rep(0,99) ))

# 104 variables - add or change correlation structure ....
y <- xx %*% bs + + rnorm(ns)*1
df <- as.data.frame( cbind(xx,y) )
names(df) <- c(as.character( seq( ) ),"y")


yDat <- y
xDat <- xx

# create the resampled data indeces
nr_bootstraps <- 30
nr_datapoints <- dim(df)[1]
fracs <- seq(1,0.1,length=nr_bootstraps)

se1_errMATRIX <- matrix(0,length(fracs),2)
min_errMATRIX<- matrix(0,length(fracs),2)

i <- 1
nr_iterations <- 50
frac <- fracs[i]
nr_samples <- round( nr_datapoints * frac )
bootstrap <- sample( 1:nr_datapoints , size = nr_samples, replace = FALSE)
cv.gg<-cv.glmnet(x= xDat[bootstrap,], y=yDat[bootstrap])


se1_matrix <- coef(cv.gg,s="lambda.1se")[-1]
min_matrix <- coef(cv.gg,s="lambda.min")[-1]
se1_err <- t(se1_matrix - bs) %*% (se1_matrix - bs)
min_err <- t(min_matrix - bs) %*% (min_matrix - bs)



for (iIteration in (1:nr_iterations)) {
  print( paste("iteration: ", iIteration) )
  for (i in (1:length(fracs))){
    frac <- fracs[i]
    
    # sample with replacement
    nr_samples <- round( nr_datapoints * frac )
    bootstrap <- sample( 1:nr_datapoints , size = nr_samples, replace = FALSE)
    cv.gg<-cv.glmnet(x= xDat[bootstrap,], y=yDat[bootstrap])
    
    
    se1_matrix <- coef(cv.gg,s="lambda.1se")[-1]
    min_matrix <- coef(cv.gg,s="lambda.min")[-1]
    se1_err <- t(se1_matrix - bs) %*% (se1_matrix - bs)
    min_err <- t(min_matrix - bs) %*% (min_matrix - bs)
    
    se1_errMATRIX[i,1] <- frac
    se1_errMATRIX[i,2] <- se1_errMATRIX[i,2]+se1_err
    
    min_errMATRIX[i,1] <- frac
    min_errMATRIX[i,2] <- min_errMATRIX[i,2]+min_err
    
    
  }
}
se1_errMATRIX[,2] <- se1_errMATRIX[,2] / nr_iterations
min_errMATRIX[,2] <- min_errMATRIX[,2] / nr_iterations

plot(se1_errMATRIX[,1], sqrt(se1_errMATRIX[,2])
     ,xlab="Fraction of total data",
     ylab="Squared Error in Beta",
     main="Stability of LASSO")

