INSPIREaic <- function(X,L,Z,theta,lambda){
  loglik <- INSPIREoptimizer::INSPIRELogLikelihood(X,L,Z,theta,lambda)
  modComplex <- INSPIREoptimizer::extractModelComplexityParameters(X,L,Z,theta)
  cat('logLik:',-2*loglik,'model complexity:',2*modComplex['p'],'\n')
  return(2*modComplex['p'] - 2*loglik)
}
