INSPIREbicWrapper <- function(mcnt,lambda,X){
  #lambda

  res <- INSPIRE::INSPIRE(X,mcnt,lambda)
  bic <-INSPIREoptimizer::INSPIREbic(X,res$L,res$Z,res$theta,lambda)
  cat('INSPIRE for',mcnt,'modules and lambda =',lambda,'with bic =',bic,'\n')
  return(bic)
}
