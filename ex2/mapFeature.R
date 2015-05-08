# Initial code is here: 
# http://ygc.name/2011/10/26/machine-learning-5-2-regularized-logistic-regression/
mapFeature <- function(x1,x2,degree=6){
  out <- sapply(0:degree,function(i)
    sapply(0:i, function(j)
      x1^(i-j) * x2^j
    )
  )
  # Since sapply returns list, we need to convert it to data frame
  # http://stackoverflow.com/questions/4227223/r-list-to-data-frame
  return(data.frame(matrix(unlist(out), nrow=length(x1))))
}