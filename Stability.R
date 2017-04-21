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



# create the resampled data indeces
nr_bootstraps = 20
nr_datapoints <- dim(Iowa)[1]
folds <- matrix(0,nr_ensembles,nr_samples)

for (fraction in seq(1,0.1,length=nr_bootstraps)){
  
  # sample with replacement
  nr_samples <- round( nr_datapoints * fraction )
  folds[i,] <- sample( 1:nr_datapoints , size = nr_samples, replace = TRUE)  
  
}

