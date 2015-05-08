predict <- function (theta, X){
  as.numeric(theta %*% t(X) > 0)
}