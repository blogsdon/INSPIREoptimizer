INSPIREgridSearch <- function(Xq,
                              mcnt = seq(10,200,by=40),
                              lambda = 10^seq(-2,3,by=1)){
  res1 <- expand.grid(mcnt,lambda)
  no_cores <- parallel::detectCores()
  cl <- parallel::makeCluster(no_cores)
  res1$bic<- parallel::clusterMap(cl,
                                  INSPIREoptimizer::INSPIREbicWrapper,
                                  res1$Var1,
                                  res1$Var2,
                                  MoreArgs = list(X=Xq),
                                  SIMPLIFY=TRUE)
  parallel::stopCluster(cl)
  return(res1)
}
