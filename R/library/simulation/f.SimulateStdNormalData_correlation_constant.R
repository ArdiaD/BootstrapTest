#' Simulate multivariate standard normal variables with constant correlation structure
#'
#' @param dMu : mean of the simulated returns (for all series)
#' @param dCorrelationBetweenSeries :  correlation value (one cross-correlation for all the series)
#' @param iNumObs : number of observations to be simulated
#' @param iNumSeries : number of series to be simulated
#' @return mZ : matrix [iNumObs*iNumSeries] containing the simulated returns

#' @author David Ardia & Anas Guerrouaz

f.SimulateStdNormalData_correlation_constant = function(dMu, dCorrelationBetweenSeries, iNumObs, iNumSeries) {

  mOne        = matrix(data = 1, nrow = iNumSeries, ncol = iNumSeries)
  mEye        = diag(iNumSeries)
  
  mCovariance = mOne * dCorrelationBetweenSeries + mEye * (1 - dCorrelationBetweenSeries)
  
  mZ          = rmvnorm(iNumObs, mean = rep(dMu, iNumSeries), sigma = mCovariance)
  
  return(mZ)
}