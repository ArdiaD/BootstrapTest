#' Wrapper function for the different data generating processes
#' @param objDGP : list containing important information about the DGP, including the correlation model
#' @param iNumObs : number of observations to be simulated
#' @param iNumSeries : number of series to be simulated
#' @return mZ : matrix [iNumObs*iNumSeries] containing the simulated returns

#' @author David Ardia & Anas Guerrouaz

f.SimulateDGP = function(objDGP, iNumObs, iNumSeries){
  
  if (objDGP$sType == 'CC') {
    mZ = f.SimulateStdNormalData_correlation_constant(objDGP$dMu, 
                                                      objDGP$dCorrelationBetweenSeries, 
                                                      iNumObs, iNumSeries)
  } else if (objDGP$sType == 'DCC') {
    mZ = f.SimulateStdNormalData_correlation_DCC(objDGP$dUnconditionalCorrelation, 
                                                 objDGP$dAlpha, objDGP$dBeta, 
                                                 iNumObs, iNumSeries)
  } else if (objDGP$sType == '2RC') {
    mZ = f.SimulateStdNormalData_correlation_2regimes(objDGP$dProbStay, 
                                                      objDGP$dCorrelation1, 
                                                      objDGP$dCorrelation2, 
                                                      iNumObs, iNumSeries)
  } else stop('DGP type not well defined')
  
  return(mZ)
}
