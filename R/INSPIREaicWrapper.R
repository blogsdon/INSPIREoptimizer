INSPIREaicWrapper <- function(mcnt,lambda,X){
  #lambda

  res <- INSPIRE::INSPIRE(X,mcnt,lambda,maxiter=10)
  bic <-INSPIREoptimizer::INSPIREaic(X,res$L,res$Z,res$theta,lambda)
  cat('INSPIRE for',mcnt,'modules and lambda =',lambda,'with aic =',bic,'\n')
  return(bic)
}
