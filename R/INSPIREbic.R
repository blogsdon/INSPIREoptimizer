INSPIREbic <- function(X,L,Z,theta,lambda){
  loglik <- INSPIREoptimizer::INSPIRELogLikelihood(X,L,Z,theta,lambda)
  modComplex <- INSPIREoptimizer::extractModelComplexityParameters(X,L,Z,theta)
  return(log(modComplex['n'])*modComplex['p'] - 2*loglik)
}
