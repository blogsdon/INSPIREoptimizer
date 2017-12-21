INSPIRELogLikelihood <- function(X,L,Z,theta,lambda){
  #return the ll of the current INSPIRE model
  #nT is the number of data-sets
  nT <- length(X)
  #decomposition of theta
  foo <- svd(theta)
  bar <- 2*log(foo$d)
  SL <- (1/nT)*(t(L)%*%L)
  loglik <- (nT/2)*(sum(bar) - sum(diag(SL%*%theta)))
  loglik <- loglik - lambda*(sum(abs(theta))+sum(abs(diag(theta))))
  Lq <- INSPIRE::buildPseudoData(X,L,Z)
  err <- Reduce('-',X,Lq)
  err <- sapply(err,function(x) sum(c(x)^2))
  err <- -0.5*sum(err)
  loglik <- logLik + err
  return(loglik)
}
