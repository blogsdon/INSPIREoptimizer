mcnt = 10
lambda = .1
data('two_example_datasets')
X <- list(x1 = scale(log(exmp_dataset1)),
          x2 = scale(log(exmp_dataset2)))
res = INSPIRE(X, mcnt, lambda)

pseudoData<-INSPIREoptimizer::buildPseudoData(X,res$L,res$Z)
loglik <- INSPIREoptimizer::INSPIRELogLikelihood(X,res$L,res$Z,res$theta,lambda)
