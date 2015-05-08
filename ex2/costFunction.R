gradient <- function(theta, X, y){
  m <- nrow(X)
  X <- as.matrix(X)
  grad <- t(X %*% theta - y) %*% X/m
  return(grad)
}

cost <- function(theta, X, y) {
  m <- nrow(X)
  X <- as.matrix(X)
  J <- sum(-y * log(sigmoid(X %*% theta)) - (1-y) * log(1 - sigmoid(X %*% theta)))/m;
  return(J)
}