#!/usr/bin/Rscript 
# Advanced branch:
# Compute decision boundaries, accuracy on training set
# with different parameters of Lambda.
# Plot different decision boundaries, and training accuracies,
# correspndng different values.

library(ggplot2)
library(reshape)
#Closing all the plots and resetting all the variables in the workspace
graphics.off()
rm(list = ls())

source("sigmoid.R")
source("costFunctionReg.R")
source("predict.R")
source('mapFeature.R')

#Reading excercise data
data <- read.delim("ex2data2.txt", header=FALSE, sep=",")
data$V3 <- factor(data$V3)

#=======Part 1. Plotting exam scores and admissions==============
p <- ggplot(data, aes(x = V1, y = V2, color = V3, shape = V3)) +
  geom_point() +
  scale_x_continuous("Microchip test 1") +
  scale_y_continuous("Microchip test 2") +
  scale_colour_hue(name = "QC pass", labels=c("No", "Yes")) +
  scale_shape(name = "QC pass", labels=c("No", "Yes"))   
print(p)
#Pausing execution
cat("Program paused. Press enter to continue...")
readline()
#======= Part2 Finding optimal params ====

# Initializing input variables X and output variables y
# Since plot showed that our decision bondary is not linear,
# we should build more features from each data point.
# It is given that we will construct a 6th power polynomial out of x1 and x2.
# The function that dues it is in mapFeature.R
X <- mapFeature(data[,c(1)],data[,c(2)])
y <- as.numeric(data[,3]) - 1

m <- nrow(X)  
n <- ncol(X)

initial_theta <- rep(0,n)


# Setting upregularization parameter equals to 1
lambda <- 1

#===== Finding optimal parameters with optim function =====
# Finding optimal parameters with optim function.
# Tip from stackoverflow: http://stackoverflow.com/q/11546036/218584
# More info: ?optim
o <- optim(initial_theta, cost_reg,X=X, y=y,lambda=lambda)
opt_params <- unlist(o["par"])
# Printing values
cat(sprintf('Cost at theta found by optim: %f\n\n', o["value"]))
#=====Plotting decision boundary ========
# Approach taken from here:
# http://ygc.name/2011/10/26/machine-learning-5-2-regularized-logistic-regression/

#Initializing argument vectors with arbitrary value
u <- seq(min(data[1]),max(data[1]), len=200)
v <- seq(min(data[2]),max(data[2]), len=200)
#Initializing vector with cartesian product of vectors u and v
product<-expand.grid(u,v)
#Mapping initial two arguments to a 6th power polynome
polynome_features <- mapFeature(product$Var1,product$Var2)
# Initializing dataframe of values with regularized optimal parameters
z <- as.data.frame(as.matrix(polynome_features) %*% opt_params)
z$u <- product$Var1
z$v <- product$Var2
z$lambda <- lambda
z$lambda <- factor(z$lambda)
# using geom_countour method from ggplot2 package
# note: we reinitialized colour and shape variables in aes
# because we assigned different vectors to these variables 
# when we drew p the first time
p <- p+geom_contour(data=z, aes(x=u, y=v, z=V1, group=lambda, colour=lambda,shape=lambda),bins=1)
print(p)
 #==== Prediction and accuracies =========
# Calculating accuracy of our algorithm on training set
p <- predict(opt_params, X) == y
acc <- length(p[p==TRUE])/length(p)
cat(sprintf('Train accuracy: %f\n',acc))