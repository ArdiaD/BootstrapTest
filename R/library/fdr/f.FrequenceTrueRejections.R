#' This function returns a point estimate and a 90% confidence interval for the pizero 
#' of the funds, as well as the optimal lambda (optional)
#' @param pvals : [matrix] (M x N) P-values
#' @param alpha : [scalar] 
#' @param type : [scalar] default value is 1

#' @author Anas Guerrouaz & David Ardia

f.FrequenceTrueRejections = function(pvals, alpha, type = 1){
  
  n = length(pvals)
  
  if (type == 1) {
    pizero = mean(pvals >= alpha)
    sig = sqrt(pizero * (1 - pizero) / n)
    lambda = F
  } else {    
    lambda = f.ComputeOptLambda(pvals, 1000)
    pizero = f.ComputePizero(pvals, lambda)
    
    nl  = sum(pvals > lambda)
    sig = sqrt( (nl * (n - nl)) / (n * (1 - lambda) ^ 2) )
  }
  
  lb = max(0, pizero - 1.6449 * sig)
  ub = min(1, pizero + 1.6449 * sig)
  
  
  out = list(pizero = pizero, sig = sig, lb = lb, ub = ub, lambda = lambda)
  return(out)
}