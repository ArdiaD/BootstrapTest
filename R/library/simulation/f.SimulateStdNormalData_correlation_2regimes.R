#' Simulate standard normal variables with 2-regimes correlation structure
#' @param dProbStay : conditional probability of staying in the same correlation regime
#' @param dCorrelation1 : 1st regime's correlation
#' @param dCorrelation2 : 2nd regime's correlation
#' @param iNumObs : number of observations to be simulated
#' @param iNumSeries : number of series to be simulated
#' @return mZ : matrix [iNumObs*iNumSeries] containing the simulated returns

#' @author David Ardia & Anas Guerrouaz

f.SimulateStdNormalData_correlation_2regimes = function(dProbStay, dCorrelation1, dCorrelation2, iNumObs, iNumSeries){

  mOne    = matrix(data = 1, nrow = iNumSeries, ncol = iNumSeries)
  mEye    = diag(iNumSeries)
  vMu     = rep(0, times = iNumSeries)
  
  # First, simulate vector vRegime with regime (1 or 2):
  vRegime = matrix(data = 0, iNumObs, 1)
  
  # For t=1 from unconditional [0.5 0.5] distribution:
  
  dU          = runif(1)
  vRegime[1,] = 1 + (dU < 0.5) 
  
  # For t=2:iNumObs from conditional distribution given regime at t-1:
  for (t in 2:iNumObs) {
    dU    = runif(1)
    vRegime[t,] = 1 * ((vRegime[t - 1,] == 1)* (dU < dProbStay) +  (vRegime[t - 1,] == 2)* (dU >= dProbStay) ) + 
      2 * ((vRegime[t - 1,] == 2)* (dU < dProbStay) +  (vRegime[t - 1,] == 1)* (dU >= dProbStay) )
  }
  
  # Second, simulate mZ given vRegime:
  mCovariance1 = mOne * dCorrelation1 + mEye * (1 - dCorrelation1)
  mCovariance2 = mOne * dCorrelation2 + mEye * (1 - dCorrelation2)
  
  mZ1 = rmvnorm( iNumObs, mean = vMu, sigma = mCovariance1 )
  mZ2 = rmvnorm( iNumObs, mean = vMu, sigma = mCovariance2 )
  
  mZ  =  mZ2 + ((vRegime == 1) %*% t(rep(1, times = iNumSeries) ) * (mZ1 - mZ2) )
  
  return(mZ)
}