INSPIREgridSearchAIC <- function(Xq,
                              mcnt = seq(10,200,by=40),
                              lambda = 10^seq(-2,3,by=1)){
  res1 <- expand.grid(mcnt,lambda)
  res1$aic<- mapply(INSPIREoptimizer::INSPIREaicWrapper,
                                  res1$Var1,
                                  res1$Var2,
                                  MoreArgs = list(X=Xq),
                                  SIMPLIFY=TRUE)
  return(res1)
}
