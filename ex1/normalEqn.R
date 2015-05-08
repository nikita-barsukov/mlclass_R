normalEqn <- function(X,y){
  X <- as.matrix(X)
  solve(t(X)%*%X) %*% t(X) %*% y
}