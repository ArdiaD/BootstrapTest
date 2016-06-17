#' Berkowitz test of iid (0,1) variates
#' @param mZ : [matrix] (T x d) of observations
#' @param do.chi2 : [logical] if set to TRUE, the function will return the p-values for each likelihood ratio
#' @return a list with the following elements: 
#' vLR_stat : [vector] (1 x d) with LR_Berkowitz per column
#' vLR_pvalue : [vector] (1 x d) optional output containing the p-values for each L.R. in vLR_stat
#' @note Berkowitz test for mu = 0, sigma^2 = 1, rho = 0

#' @author Anas Guerrouaz & David Ardia

.f.LR_Berkowitz = function(mZ, do.chi2 = FALSE){
  
  iNumObs    = nrow( mZ )
  iNumSeries = ncol( mZ )
  
  mX = mZ[1:(iNumObs - 1),,drop = FALSE]
  mY = mZ[2:iNumObs,,drop = FALSE]
  
  mX_demeaned = sweep(mX, 2, colMeans(mX), FUN = "-")
  mY_demeaned = sweep(mY, 2, colMeans(mY), FUN = "-")
  
  vBeta1_OLS_MLE = colSums( mX_demeaned * mY_demeaned ) / colSums( mX_demeaned * mX_demeaned )
  
  mY_demeaned_fit = mX_demeaned * matrix(data = vBeta1_OLS_MLE, nrow = iNumObs - 1, ncol = iNumSeries, byrow = TRUE)
  mOLS_Residual   = mY_demeaned - mY_demeaned_fit
  
  vSSR        = colSums( mOLS_Residual ^ 2 )
  vSigma2_MLE = vSSR / (iNumObs - 1)
  
  vLogL_H1 = -((iNumObs - 1) / 2) * log( 2*pi ) - ((iNumObs - 1) / 2) * log( vSigma2_MLE ) - ((iNumObs - 1) / 2)
  # last term in log-likelihood formula:
  # sigma2_MLE = SSR/(T-1), so that SSR/sigma2_MLE = T-1
  vLogL_H0 = -((iNumObs - 1) / 2) * log(2 * pi) - (1 / 2) * colSums( mY ^ 2 )
  
  vLR_stat = 2 * (vLogL_H1 - vLogL_H0)
  
  vLR_pvalue = vector('double', iNumSeries)
  if (do.chi2) {
    vLR_pvalue = 1 - pchisq(vLR_stat, df = 3)
  }
  
  out = list(vLR_stat = vLR_stat, vLR_pvalue = vLR_pvalue)
  return(out)
}
f.LR_Berkowitz = compiler::cmpfun(.f.LR_Berkowitz)