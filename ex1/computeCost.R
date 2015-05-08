computeCost <- function(X,y,theta){
  m <- nrow(X)
  X <- as.matrix(X)
  return( sum((X %*% theta - y)^2)/(2*m) )
}