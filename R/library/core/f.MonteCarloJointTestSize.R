#' Core function for the Monte Carlo study of the size
#' @param objIN : [list] contains various inputs information
#' @param doSave : [logical] should we save the output (default = TRUE)
#' @author Anas Guerrouaz & David Ardia

.f.MonteCarloJointTestSize = function(objIN, doSave = TRUE){
  
  objDGP = list()
  objDGP$iRandseed = objIN$iRandseed
  objDGP$sType     = objIN$sType
  
  if (objDGP$sType == 'CC') {
    objDGP$dMu      = 0
    objDGP$dSigma2  = 1
    objDGP$dCorrelationBetweenSeries = 0.9; # rho
    objDGP$sParam   = paste0('\rho = ', objDGP$dCorrelationBetweenSeries)      
  } else if (objDGP$sType == 'DCC') {
    objDGP$dMu      = 0
    objDGP$dSigma2  = 1
    objDGP$dUnconditionalCorrelation = 0.5
    objDGP$dAlpha   = 0.02
    objDGP$dBeta    = 0.97
    objDGP$sParam   = paste0('\rho = ', objDGP$dUnconditionalCorrelation, 
                             ', \alpha = ', objDGP$dAlpha,
                             ', \beta = ', objDGP$dBeta) 
  }  else if (objDGP$sType == '2RC') {
    # Pr[stay in regime]      = 0.90,
    # Pr[go to other regime]  = 0.10
    # correlation_regime1     = 0.0
    # correlation_regime2     = 0.9    
    objDGP$dMu            = 0
    objDGP$dSigma2        = 1
    objDGP$dProbStay      = 0.9
    objDGP$dCorrelation1  = 0
    objDGP$dCorrelation2  = 0.9
    objDGP$sParam = paste0('\rho_1 = ', objDGP$dCorrelation1,
                           ', \rho_2 = ', objDGP$dCorrelation2,
                           ', \ prob[stay] = ', objDGP$dProbStay)
  } else {
    stop('DGP type not well defined')
  }
  
  # ================================================================================
  # MC function
  .f.mc = function(iRandseed){
    set.seed(iRandseed)
    
    # === Simulate DGP
    mZ = f.SimulateDGP(objDGP, objIN$iNumObs, objIN$iNumSeries) 
    
    # === Berkowitz test per column:
    vLR_stat_Berkowitz       = f.LR_Berkowitz( mZ )$vLR_stat
    dLR_stat_Berkowitz_total = sum(vLR_stat_Berkowitz)
    
    # joint test statistic is sum of LR test statistics per column
    # (This is not an LR statistic itself anymore, unless the variables in the columns are independent.)
    
    mZ_aux_ = f.CrossBootstrap(mZ, objDGP$dMu, objDGP$dSigma2, objIN$iNumBootDataSets)$mZ_aux
    
    mLR_stat_Berkowitz_bootstrap = matrix(data = 0, nrow = objIN$iNumSeries, ncol = objIN$iNumBootDataSets)
    
    for (i in 1:objIN$iNumSeries) {
      mZ_aux = mZ_aux_[,,i]
      
      # Each row i of mLR_stat_Berkowitz_bootstrap
      # contains iNumBootDataSets LR-statistics for series i (i= 1, ...., iNumSeries)
      mLR_stat_Berkowitz_bootstrap[i,] = f.LR_Berkowitz( mZ_aux )$vLR_stat
    }       
    
    vLR_stat_Berkowitz_total_bootstrap = apply( mLR_stat_Berkowitz_bootstrap, MARGIN = 2, FUN = sum)
    dPvalue_Berkowitz_bootstrap        = mean( vLR_stat_Berkowitz_total_bootstrap >= dLR_stat_Berkowitz_total)
    
    return(dPvalue_Berkowitz_bootstrap)
  } 
  f.mc = cmpfun(.f.mc) # compiler
  
  # ================================================================================
  # MC replications
  #browser()
  in.list = as.list(seq(from = objDGP$iRandseed, length.out = objIN$iNumExperiments))
  
  if (objIN$iNumCore == 1) {
    vPvalue_Berkowitz_bootstrap = matrix(data = 0, nrow = objIN$iNumExperiments, ncol = 1)
    for (i in 1:objIN$iNumExperiments) {
      cat(i,"\n")
      vPvalue_Berkowitz_bootstrap[i] = f.mc(in.list[[i]])
    }
  }
  
  if (objIN$iNumCore > 1) {
    require("snowfall")
    iNumCore = min(objIN$iNumCore, length(in.list))
    snowfall::sfSetMaxCPUs(number = iNumCore)
    snowfall::sfInit(parallel = TRUE, cpus = iNumCore, type = "SOCK")
    snowfall::sfSource("sourceall.R")
    snowfall::sfExport(list = list("f.mc", "objDGP", "objIN"))
    out.list = snowfall::sfClusterApplyLB(x = in.list, fun = "f.mc")
    snowfall::sfStop()
    vPvalue_Berkowitz_bootstrap = matrix(unlist(out.list), nrow = objIN$iNumExperiments, ncol = 1)
    
    # iNumCore = min(objIN$iNumCore, length(in.list))
    # cl = snow::makeCluster(rep("localhost", iNumCore), type = "SOCK")
    # snow::clusterEvalQ(cl = cl, expr = source("sourceall.R"))
    # n.env = new.env()
    # assign('f.mc', f.mc, n.env)
    # assign('objIN', objIN, n.env)
    # assign('objDGP', objDGP, n.env)
    # snow::clusterExport(cl = cl, list("f.mc", "objDGP", "objIN"), envir = n.env)
    # out.list = snow::clusterApply(cl = cl, x = in.list, fun = "f.mc")
    # snow::stopCluster(cl)
    # vPvalue_Berkowitz_bootstrap = matrix(unlist(out.list), nrow = objIN$iNumExperiments, ncol = 1)
  }
  
  # ================================================================================
  # Report results for bootstrap-simulated p-values
  objOUT = list()
  objOUT$iNumObs          = objIN$iNumObs
  objOUT$iNumSeries       = objIN$iNumSeries;
  objOUT$objDGP           = objDGP
  objOUT$iNumBootDataSets = objIN$iNumBootDataSets
  objOUT$vPvalue_Berkowitz_bootstrap = vPvalue_Berkowitz_bootstrap
  
  # Prevent values 0 and 1 for p-values (that do result from bootstrap simulation)
  vPvalue_Berkowitz_bootstrap_INSIDE_0_1 = ((vPvalue_Berkowitz_bootstrap * objIN$iNumBootDataSets) + 1) / (objIN$iNumBootDataSets + 2)
  
  # Tests
  objOUT$Berk = f.Pvalues_Histogram_and_Tests(vPvalue_Berkowitz_bootstrap_INSIDE_0_1)
  
  # Save output
  if (isTRUE(doSave)) {
    save(objOUT, file = paste0(objIN$sPath, objIN$sSave, '.RData'), compress = TRUE)
  }
  return(objOUT)
}
f.MonteCarloJointTestSize = compiler::cmpfun(.f.MonteCarloJointTestSize)
