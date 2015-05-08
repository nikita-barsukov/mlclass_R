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
source('warmUpExcercise.R')
source('plotData.R')

#================ Part 1:warming up ==========
cat('Running upwarmup excercise...\n')
cat('5x5 Identity matrix:\n')
print(warmUp())
#Pausing execution
cat("Program paused. Press enter to continue...")
readline()
#================ Part 2: Reading data and plotting ==========
data <- read.delim("ex1data1.txt", header=FALSE, sep=",")
plotData(data)
#Pausing execution
cat("Program paused. Press enter to continue...")
readline()
#================Part 3: Gragient descent ===================
cat("Running gradient descent:\n")
#Initializing variables
X <- as.data.frame(data[,1])
names(X) <- c('V1')
y <- data[,2]
m <- length(X)  
n <- ncol(X)
# Adding intercept variable and declaring initial theta, learning rate
# and number of iterations for gradient descent
X$V0<-rep(1,m)
X <- X[c('V0','V1')]
n <- ncol(X)
initial_theta <- rep(0,n)
iterations <- 1500
alpha <- 0.01
init_cost <- computeCost(X,y,initial_theta)
cat(sprintf('Cost with all the parameters equal to 0:\t %f \n', init_cost))
opt_params <- gradientDescent(X,y,initial_theta,alpha,iterations)
opt_theta <- unlist(opt_params['theta'])
names(opt_theta)<-c('theta0','theta1')
# Printing optimal values and plots
cat('Optimal parameters found by gradient descent:\n')
print(opt_theta)

cat(sprintf('Cost at optimal parameters: %f',computeCost(X,y,opt_theta)))
abline(opt_theta,lwd=2,col='red')

#================Part 4: Comparing gradient descent with least squares ===================
cat("\nProgram paused. Press enter to continue...")
readline()
cat('Comparing gradient descent and least squares\n')
lm.model <- lm(V1 ~ V2, data)
cat(sprintf('Cost at parameters found by least squares: %f',computeCost(X,y,lm.model$coefficients)))
abline(lm.model$coefficients,lwd=2,col='green')
    