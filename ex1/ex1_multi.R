#!/usr/bin/Rscript
# Advanced branch:
# 1) Use ggplot2 package to plot excercise data
# 2) Calculate error on training set with params, calculated with other optimization algorithms

library(ggplot2)
#Closing all the plots and resetting all the variables in the workspace
graphics.off()
rm(list = ls())

source('computeCost.R')
source('gradientDescent.R')
source('featureNormalize.R')
source('normalEqn.R')
source('plotData.R')

#========= Part 1. Feature normalization ==========
# Importing data 
data <- read.delim('ex1data2.txt',sep=",",header=FALSE)
X <- data[,c('V1','V2')]
y <- data[,c('V3')]
m <- nrow(X)
n <- ncol(X)

#======Part 1: normalizing features ===========
X.norm <- featureNormalize(X)
mjus <- sapply(X,mean)
stddevs <- sapply(X,sd)
X.norm$V0 <- rep(1,m)
X.norm <- X.norm[c('V0','V1','V2')]

#======Part 2: running gradient descent ======
# Necessary variables for gradient descent 
initial_theta <- rep(0,n+1)
iterations <- 1500
alpha <- 0.01
cat(sprintf('Cost at initial parameters (all zeros): %f\n',computeCost(X.norm,y,initial_theta)))
cat("Running gradient descent...\n")
opt_params <- gradientDescent(X.norm,y,initial_theta,alpha,iterations)

opt_theta <- unlist(opt_params['theta'])
names(opt_theta)<-c('theta0','theta1','theta2')

cat('Optimal parameters found by gradient descent:\n')
print(opt_theta)
cat(sprintf('Cost at optimal parameters: %f\n\n',computeCost(X.norm,y,opt_theta)))

#=========== Part 3: Normal equations ========
cat("Calculating optimal parameters using normal equations.\n")
X$V0 <- rep(1,m)
X <- X[c('V0','V1','V2')]
theta_norm_eqn = as.numeric(normalEqn(X,y))
names(theta_norm_eqn) = c('theta0','theta1','theta2')
cat('Optimal parameters found by normal equations:\n')
print(theta_norm_eqn)
cat(sprintf('Cost at optimal parameters: %f\n\n',computeCost(X,y,theta_norm_eqn)))