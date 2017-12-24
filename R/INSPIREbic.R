INSPIREbic <- function(X,L,Z,theta,lambda){
  loglik <- INSPIREoptimizer::INSPIRELogLikelihood(X,L,Z,theta,lambda)
  modComplex <- INSPIREoptimizer::extractModelComplexityParameters(X,L,Z,theta)
  cat('logLik:',-2*loglik,'model complexity:',log(modComplex['n'])*modComplex['p'],'\n')
  return(log(modComplex['n'])*modComplex['p'] - 2*loglik)
}
