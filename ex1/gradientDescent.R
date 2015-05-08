gradientDescent <- function(X, y, theta, alpha, num_iters){
  m <- length(y)
  X <- as.matrix(X)
  J_hist <- 0
  for (i in 1:num_iters){
    theta <- theta - alpha * t(X) %*% (X %*% theta - y) / m
    J_hist[i] = computeCost(X,y,theta)
  }
  return(list('theta' = theta,'J_hist'=J_hist))
}