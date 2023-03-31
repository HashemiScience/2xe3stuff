makeMyBarplot <- function(theSeed,n){
  set.seed(theSeed)
  theMeans = matrix(sample(10:100,6), nrow=2)
  theSD = matrix(sample(20:50,6), nrow=2)
  
  lowerBounds = theMeans - (theSD/sqrt(n))
  upperBounds = theMeans + (theSD/sqrt(n))
  bp = barplot(theMeans, beside=T, ylim=c(0,120))
  segments(bp,lowerBounds, bp, upperBounds)
}

set.seed(2023)
data1 <- rnorm(55,mean=20,sd=12)
data2 <- runif(55,min=0,max=10)
