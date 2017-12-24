library(INSPIRE)
mcnt = 250
lambda = .1
data('two_example_datasets')
X <- list(x1 = scale(log(exmp_dataset1)),
          x2 = scale(log(exmp_dataset2)))
res = INSPIRE(X2, mcnt, lambda)

pseudoData<-INSPIREoptimizer::buildPseudoData(X,res$L,res$Z)
loglik <- INSPIREoptimizer::INSPIRELogLikelihood(X,res$L,res$Z,res$theta,lambda)
modComplex <- INSPIREoptimizer::extractModelComplexityParameters(X,res$L,res$Z,res$theta)

bicTest <- INSPIREoptimizer::INSPIREbic(X,res$L,res$Z,res$theta,lambda)

optimizedINSPIRE <- INSPIREoptimizer::INSPIREgridSearch(X,mcnt = c(200,300,400),lambda = c(.1,1))
foobar<-tidyr::spread(optimizedINSPIRE,key='Var1',value='bic')


synapseClient::synapseLogin()
X <- rSynapseUtilities::loadDelimIntoList(c('syn11275934','syn11275945'))
cleanData <- function(x){
  rownames(x) <- x[,1]
  x <- x[,-1]
  x <- t(x)
  x <- apply(x,2,utilityFunctions::winsorize)
  x <- scale(x)
  return(x)
}
X <- lapply(X,cleanData)

subSampleX <- sample(1:20514,2000)
X3 <- lapply(X,function(x,y) x[,y],subSampleX)

optimizedINSPIRE <- INSPIREoptimizer::INSPIREquickOptimize(X3,mmin = 5,mmax = 120,lmin = -3,lmax = 0)
testInspire <- INSPIREoptimizer::INSPIREgridSearch(X,mcnt=c(5,10,15,20,25,30,35,40,45,50,55,60,65,70),lambda = 1)
crazy<-INSPIREoptimizer::INSPIREbicWrapper(7,1,X3)

foobar<-tidyr::spread(optimizedINSPIRE,key='Var1',value='bic')
rownames(foobar) <- foobar$Var2
foobar <- foobar[,-1]
pheatmap::pheatmap(foobar,cluster_rows = F,cluster_cols = F)
