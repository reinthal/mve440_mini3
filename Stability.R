# clear memory
rm(list=ls())
# Garbage Collect
gc()

library(lasso2)
library(glmnet)
library(lqa)
library(bestglm)
library(MASS)
library(hdi)

# Cook dome data
ns<-200
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
nr_truevars <- length(bs[bs != 0])
nr_datapoints <- dim(df)[1]
nr_bootstraps <- 30
nr_iterations <- 1
fracs <- seq(1,0.1,length=nr_bootstraps)


oracle_errMATRIX <- array(0, dim=c(nr_bootstraps,nr_iterations,2))
pval_matrix <- array(0,dim=c(nr_truevars,nr_bootstraps,nr_iterations,2))


min_errMATRIX[,,1] <- fracs
pval_matrix[,,,1] <- fracs

# Unit Test the loop
iBootstrap <- 1
iIteration <- 1
frac <- 0.5
nr_samples <- round( nr_datapoints * fracs[iBootstrap] )
bootstrap <- sample( 1:nr_datapoints , size = nr_samples, replace = FALSE)
cv.gg<-cv.glmnet(x= xDat[bootstrap,], y=yDat[bootstrap])
ll <- lasso.proj(x=xDat[bootstrap,],y=yDat[bootstrap])

beta_hats <- coef(cv.gg,s="lambda.min")[-1]
min_err <- t(beta_hats - bs) %*% (beta_hats - bs)
pval_matrix[,iBootstrap,iIteration,2] <- ll$pval[bs != 0]


# Loop the loop, get errors - get rewarded
for (iIteration in (1:nr_iterations)) {
  
  print( paste("iteration: ", iIteration) )
  
  for (iBootstrap in (1:nr_bootstraps)){
  
    
    # sample without replacement
    nr_samples <- round( nr_datapoints * fracs[iBootstrap] )
    bootstrap <- sample( 1:nr_datapoints , size = nr_samples, replace = FALSE)
    #cv.gg <- cv.glmnet(x= xDat[bootstrap,], y = yDat[bootstrap])
    ll <- lasso.proj(x = xDat[bootstrap,],y = yDat[bootstrap])
    
   
    #beta_hats <- coef(cv.gg,s="lambda.min")[-1]
    #min_err <- t(beta_hats - bs) %*% (beta_hats - bs)
    
    #oracle_errMATRIX[iBootstrap,iIteration,2] <- min_err
    pval_matrix[,iBootstrap,iIteration,2] <- ll$pval[bs != 0]
    
    
  }
}
# Calculate the mean and std dev. from oracle error
M1 <- t(oracle_errMATRIX[,,2])
mus <- colMeans(M1)
sigmas <- apply(M1, 2, sd)

# calculate the points +1 sd
p1sd <- mus+sigmas


# calculate the points -1 sd
m1sd <- mus-sigmas



# Make new plot function
plot(fracs,mus,
     type="b"
     ,xlab="Fraction of total data",
     ylab="Oracle Squared Error",
     main="Stability of LASSO",lty=3)
lines(fracs,p1sd,col="green",lty=1)
lines(fracs,m1sd,col="green",lty=1)
legend('topright', c('mus','+/- 1 std dev'),
       lty=c(3,1), col=c('black', 'green'), bty='n', cex=1.5)
