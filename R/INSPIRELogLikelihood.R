INSPIRELogLikelihood <- function(X,L,Z,theta,lambda){
  #return the ll of the current INSPIRE model
  #nT is the number of data-sets
  nT <- nrow(L)
  #decomposition of theta
  logdet <- determinant(theta,logarithm=TRUE)$modulus
  SL <- (1/nT)*(t(L)%*%L)
  cat('component1:',(nT/2)*logdet,'\n')
  cat('component2:',(nT/2)*sum(diag(SL%*%theta)),'\n')
  loglik <- (nT/2)*(logdet - sum(diag(SL%*%theta)))
  loglik <- loglik - lambda*(sum(abs(theta))-sum(abs(diag(theta))))
  Lq <- INSPIREoptimizer::buildPseudoData(X,L,Z)
  err <- mapply('-',X,Lq,SIMPLIFY=F)
  err <- sapply(err,function(x) sum(c(x)^2))
  err <- -0.5*sum(err)
  cat('err:',err,'\n')
  loglik <- loglik + err
  return(loglik)
}
