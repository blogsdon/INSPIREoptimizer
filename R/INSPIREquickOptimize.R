INSPIREquickOptimize <- function(Xq,
                                 mmin=5,
                                 mmax=50,
                                 lmin=-2,
                                 lmax=2){
  #####first search over module space
  res <- INSPIREoptimizer::INSPIREgridSearchAIC(Xq,
                                             mcnt=seq(mmin,mmax,by=5),
                                             lambda=.1)

  bestModel <- which.min(res$aic)
  if(bestModel == nrow(res) | bestModel == 1){
    cat('best module model at boundary!\n')
  }
  mcnt1 <- res$Var1[bestModel]
  #res <- dplyr::arrange(res,bic)
  #mcnt1 <- res$Var1[1]
  resLambda <- INSPIREoptimizer::INSPIREgridSearch(Xq,
                                                   mcnt=mcnt1,
                                                   lambda=10^seq(lmin,lmax,length.out=10))
  bestModel <- which.min(resLambda$bic)
  if(bestModel == nrow(resLambda) | bestModel ==1){
    cat('best lambda model at boundary!\n')
  }
  lambda1 <- resLambda$Var2[bestModel]
  result <- INSPIRE::INSPIRE(Xq,
                             mcnt = mcnt1,
                             lambda = lambda1)
  return(list(result=result,
              modres=res,
              lamres=resLambda))
}
