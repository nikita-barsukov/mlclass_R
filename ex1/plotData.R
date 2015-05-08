# Write a function that plot data in data file.
# X and Y axes must be labeled
# Datapoints must be crosses
plotData <- function(data){
  plot(data, type='p', xlab='Population of city in 10.000s', ylab='Profit in $10.000s',pch=4)
}