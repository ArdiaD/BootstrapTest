#' Compute optimal lamba values using bootstrap approach
#' @param  pvalues   : [matrix] (N x N)
#' @param  nBoot     : [scalar] bootstrap replications (default: 250)
#' @return optLambda : [scalar] optimal lambda values

#' @author Anas Guerrouaz

f.ComputeOptLambda = function(pvalues, nBoot = 250){
  
  N = ncol(pvalues)
  vlambda  = t(seq(from = 0.3, to = 0.7, by = 0.05))
  nvlambda = length(vlambda)
  mpizero  = matrix(data = NA, nrow = nvlambda, ncol = N)
  
  for (i in 1:nvlambda) {
    mpizero[i,] = f.ComputePizero(pvalues, vlambda[i])
  }
  
  
  vminpizero = min(mpizero)
  bsunif     = matrix(data = runif(n = N*nBoot, min = 0, max = 1), nrow = N, ncol = nBoot)
  bsidx      = ceiling(bsunif * N)
  pvaluesb   = apply(bsidx, MARGIN = 2, function(x) pvalues[x] )
  mpizerob   = matrix(data = NA, nrow = nBoot, ncol = nvlambda)
  
  for (j in 1:nvlambda) {
    mpizerob[j,] = f.ComputePizero(pvaluesb, vlambda[j])
  }
  
  
  # vMSE = sum((mpizerob - matrix(data = vminpizero, nrow = nBoot, ncol = nvlambda)) ^ 2)
  idmin = apply(idmin, MARGIN = 2, function(x) which.min(x))
  optLambda  = apply(idmin, MARGIN = 2, function(x) vlambda[x])
  
  return(optLambda)
}