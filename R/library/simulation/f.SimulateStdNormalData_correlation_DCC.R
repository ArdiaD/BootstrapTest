#' Simulate standard normal variables with DCC correlation structure
#' @param dUnconditionalCorrelation : unconditionnal correlation
#' @param dAlpha : alpha parameter in Engle's DCC (2002)
#' @param dBeta : beta parameter in Engle's DCC (2002)
#' @param iNumObs : number of observations to be simulated
#' @param iNumSeries : number of series to be simulated
#' @return mZ : matrix [iNumObs*iNumSeries] containing the simulated returns

#' @author David Ardia & Anas Guerrouaz

f.SimulateStdNormalData_correlation_DCC = function(dUnconditionalCorrelation, dAlpha, dBeta, iNumObs, iNumSeries){

  mOne   = matrix(data = 1, nrow = iNumSeries, ncol = iNumSeries)
  mEye   = diag(iNumSeries)
  vMu    = rep(0, times = iNumSeries)
  
  mUnconditionalCorrelation = mOne * dUnconditionalCorrelation + mEye*(1 - dUnconditionalCorrelation)
  
  mZ     = matrix(data = 0, nrow = iNumObs, ncol = iNumSeries)
  
  # For t=1 from unconditional distribution:
  mQ     = mUnconditionalCorrelation;
  mZ[1,] = rmvnorm(n = 1, mean = vMu, sigma = mQ)
  
  mConditionalCorrelation = mOne
  
  # For t=2:iNumObs from conditional distribution given regime at t-1:
  for (t in 2:iNumObs) {
    if (iNumSeries != 1) {
      # (23) of Engle (2002)
      mQ     = mUnconditionalCorrelation*(1 - dAlpha - dBeta) + dAlpha * matrix(mZ[t - 1,]) %*% t(mZ[t - 1,]) + dBeta*mQ
      mScale = diag( (diag(mQ)) ^ (-0.5) )
      # scaling so that variances are 1
      mConditionalCorrelation = mScale %*% mQ %*% mScale     
    }
    mZ[t,]   = rmvnorm(n = 1, mean = vMu, sigma = mConditionalCorrelation)    
  }
  
  return(mZ)
}