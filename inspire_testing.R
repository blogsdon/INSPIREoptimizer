mcnt = 170
lambda = .001
data('two_example_datasets')
X2 <- list(x1 = scale(log(exmp_dataset1)),
          x2 = scale(log(exmp_dataset2)))
res = INSPIRE(X, mcnt, lambda)

pseudoData<-INSPIREoptimizer::buildPseudoData(X,res$L,res$Z)
loglik <- INSPIREoptimizer::INSPIRELogLikelihood(X,res$L,res$Z,res$theta,lambda)
modComplex <- INSPIREoptimizer::extractModelComplexityParameters(X,res$L,res$Z,res$theta)

bicTest <- INSPIREoptimizer::INSPIREbic(X,res$L,res$Z,res$theta,lambda)

optimizedINSPIRE <- INSPIREoptimizer::INSPIREgridSearch(X)
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
optimizedINSPIRE <- INSPIREoptimizer::INSPIREgridSearch(X3)

