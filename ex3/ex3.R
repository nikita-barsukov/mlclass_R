#!/usr/bin/Rscript
library(R.matlab)
#Closing all the plots and resetting all the variables in the workspace
graphics.off()
rm(list = ls())

source('lrCostFunction.R')
source('oneVsAll.R')
source('predict.R')
source('predictOneVsAll.R')

## =========== Part 1: Loading and Visualizing Data =============
cat('Loading and visualizing data\n\n')  
raw_data <- readMat('ex3data1.mat')
X <- unlist(raw_data['X'])
# There are 5000 training examples, each containing 400 pixels. 
# THus we should convert X from a vector with 2.000.000 elements to 
# 5000x400 matrix
X <- matrix(X, nrow=5000)
y <-unlist(raw_data['y'])

# Plotting 100 random image matrices
# Specifying plot parameters: pin - size of single image (in), mfrow - dimensions of matrix of plots
# mai - margins
par(pin=c(0.2,0.2), mfrow=c(10,10), mai=c(0,0,0,0))
# Tipped off here: http://stackoverflow.com/questions/5638462/r-image-of-a-pixel-matrix
# And here: http://stackoverflow.com/questions/7991056/change-origin-in-image-plot-of-matrix-in-r
for (i in 1:100) {
  image(t(matrix(X[sample(1:nrow(X),1),],nrow=20)[20:1,]),
        axes=FALSE,
        col = grey(seq(0, 1, length = 256)))  
}
## ============ Part 2: Vectorize Logistic Regression ============
cat('Training One-vs-All Logistic Regression...\n\n')  
lambda <- 0.1
num_labels <- 10; 
all_theta <- oneVsAll(X,y,num_labels, lambda)

## ================ Part 3: Predict for One-Vs-All ================

pred <- predictOneVsAll(all_theta, X) == y
acc <- length(pred[pred==TRUE])/length(pred)
cat(sprintf('Train accuracy: %f\n',acc))
