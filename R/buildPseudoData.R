buildPseudoData <- function(X,L,Z){
  rn <- lapply(X,rownames)
  LList <- lapply(rn,function(y,x){return(x[y,])},L)
  resul<-mapply(function(lq,zq){return(lq[,zq])},
                L,
                Z,
                SIMPLIFY=FALSE)
  return(resul)
}
