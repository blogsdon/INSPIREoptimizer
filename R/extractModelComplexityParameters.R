extractModelComplexityParameters <- function(X,L,Z,theta){
  #get sample size
  n <- sapply(X,function(x) length(c(x)))
  n <- sum(n)

  #get model complexity
  p <- sum(theta[upper.tri(theta)]!=0)
  p <- p + nrow(theta)
  p <- p + length(c(L))
  p <- p + sum(sapply(Z,length))

  return(c(n=n,p=p))

}
