#' Given a value of lambda, this function returns the pizero of the funds
#' @param pvalues : [matrix] (M x N)
#' @param lambda  : [scalar] default 0.5
#' @author Anas Guerrouaz

f.ComputePizero = function(pvalues, lambda = 0.5){
  
  pizero = mean(pvalues >= lambda, 1) / (1 - lambda)
  pizero[pizero > 1] = 1.0
  
  return(pizero)
}