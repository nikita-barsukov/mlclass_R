sigmoid <- function(z){
  s <- 1/(1+exp(-z))
  return(s)
}