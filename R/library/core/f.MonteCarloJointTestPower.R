#' Core function for the Monte Carlo study of the power
#' @param objIN : [list] contains various inputs
#' @author Anas Guerrouaz & David Ardia

.f.MonteCarloJointTestPower = function(objIN){
  
  objDGP = list()
  objDGP$dMu       = 0
  objDGP$dSigma2   = 1
  objDGP$dCorrelationBetweenSeries = objIN$dCorrelationBetweenSeries # rho
  objDGP$sType  = 'CC'
  objDGP$dShift = objIN$dShift
  objDGP$dScale = objIN$dScale
  objDGP$iRandseed = objIN$iRandseed
  
  # MC function
  .f.mc = function(obj){ # obj is a list containing iRandseed, scale and shift
    
    Na = ceiling(objIN$iPercentSeries * objIN$iNumSeries)
    mIdShift = cbind(matrix(data = 1, nrow = objIN$iNumObs, ncol = Na), 
                     matrix(data = 0, nrow = objIN$iNumObs, ncol = objIN$iNumSeries - Na))
    
    vPvalue_test_bootstrap = vector('double', objIN$iNumExperiments)
    
    set.seed(obj$iRandseed)
    for (j in 1:objIN$iNumExperiments) {
      cat(j,"\n")
      mZ = f.SimulateDGP(objDGP, objIN$iNumObs, objIN$iNumSeries)
      mZ = mZ * obj$scale
      mZ = mZ + (obj$shift * mIdShift)
      
      vLR_stat_test       = f.LR_Berkowitz(mZ)$vLR_stat
      dLR_stat_test_total = sum(vLR_stat_test)
      
      # ==> bootstrap test
      mZ_aux_ = f.CrossBootstrap(mZ, objDGP$dMu, objDGP$dSigma2, objIN$iNumBootDataSets)$mZ_aux
      mLR_stat_test_bootstrap = matrix(data = 0, nrow = objIN$iNumSeries, ncol = objIN$iNumBootDataSets)
      
      for (i in 1:objIN$iNumSeries) {
        mZ_aux = mZ_aux_[,,i]
        mLR_stat_test_bootstrap[i,] = f.LR_Berkowitz( mZ_aux )$vLR_stat
      }       
      
      vLR_stat_test_total_bootstrap = apply( mLR_stat_test_bootstrap, MARGIN = 2, FUN = sum)
      vPvalue_test_bootstrap[j]     = mean( vLR_stat_test_total_bootstrap >= dLR_stat_test_total)
    }
    return(vPvalue_test_bootstrap)
  }
  f.mc = compiler::cmpfun(.f.mc)
  
  # ================================================================================
  # MC replications
  idShift = length(objIN$dShift)
  
  in.list = vector('list', idShift)
  for (i in 1:idShift) {
    in.list[[i]] = list(iRandseed = i, scale = objDGP$dScale[i], shift = objDGP$dShift[i])
  }
  
  if (objIN$iNumCore == 1) {
    mPvalue_test_bootstrap = matrix(data = 0, nrow = objIN$iNumExperiments, ncol = idShift)
    for (i in 1:idShift) {
      cat(i,"\n")
      mPvalue_test_bootstrap[, i] = f.mc(obj = in.list[[i]])
    }
  } else {
    require("snowfall")
    iNumCore = min(objIN$iNumCore, idShift)
    snowfall::sfSetMaxCPUs(number = iNumCore)
    snowfall::sfInit(parallel = TRUE, cpus = iNumCore, type = "SOCK")
    snowfall::sfSource("sourceall.R")
    snowfall::sfExport(list = list("f.mc", "objDGP", "objIN"))
    out.list = snowfall::sfClusterApplyLB(x = in.list, fun = "f.mc")
    snowfall::sfStop()
    mPvalue_test_bootstrap = matrix(data = unlist(out.list), nrow = objIN$iNumExperiments, ncol = idShift)
  
    #iNumCore = min(objIN$iNumCore, idShift)
    #cl = makeCluster(rep("localhost", iNumCore), type = "SOCK")
    #clusterEvalQ(cl = cl, expr = source("sourceall.R"))
    #n.env = new.env()
    #assign('f.mc', f.mc, n.env)
    #assign('objIN', objIN, n.env)
    #assign('objDGP', objDGP, n.env)
    #clusterExport(cl = cl, list("f.mc", "objDGP", "objIN"), envir = n.env)
    #out.list = clusterApply(cl = cl, x = in.list, fun = "f.mc")
    #stopCluster(cl)
    #mPvalue_test_bootstrap = matrix(data = unlist(out.list), nrow = objIN$iNumExperiments, ncol = idShift) #1st column is Pvalue_test_bootstrap, 2nd is Pvalue_fdr
  }
  
  # Prevent values 0 and 1 for p-values (that do result from bootstrap simulation)
  mPvalue_test_bootstrap_INSIDE_0_1 = ((mPvalue_test_bootstrap * objIN$iNumBootDataSets) + 1) / (objIN$iNumBootDataSets + 2)
  
  dPower_at_10percent_nominal_power = colMeans(mPvalue_test_bootstrap_INSIDE_0_1 < 0.10)
  dPower_at_5percent_nominal_power  = colMeans(mPvalue_test_bootstrap_INSIDE_0_1 < 0.05)
  dPower_at_1percent_nominal_power  = colMeans(mPvalue_test_bootstrap_INSIDE_0_1 < 0.01)
  
  dNSE_Power_at_10percent_nominal_power = sqrt( dPower_at_10percent_nominal_power * (1 - dPower_at_10percent_nominal_power)/objIN$iNumExperiments )
  dNSE_Power_at_5percent_nominal_power  = sqrt( dPower_at_5percent_nominal_power  * (1 - dPower_at_5percent_nominal_power )/objIN$iNumExperiments )
  dNSE_Power_at_1percent_nominal_power  = sqrt( dPower_at_1percent_nominal_power  * (1 - dPower_at_1percent_nominal_power )/objIN$iNumExperiments )
  
  mInterval95_for_Power_at_10percent_nominal_power = rbind(dPower_at_10percent_nominal_power - 1.96 * dNSE_Power_at_10percent_nominal_power, 
                                                           dPower_at_10percent_nominal_power + 1.96 * dNSE_Power_at_10percent_nominal_power )
  mInterval95_for_Power_at_5percent_nominal_power = rbind(dPower_at_5percent_nominal_power - 1.96 * dNSE_Power_at_5percent_nominal_power, 
                                                          dPower_at_5percent_nominal_power + 1.96 * dNSE_Power_at_5percent_nominal_power )
  mInterval95_for_Power_at_1percent_nominal_power = rbind(dPower_at_1percent_nominal_power - 1.96 * dNSE_Power_at_1percent_nominal_power, 
                                                          dPower_at_1percent_nominal_power + 1.96 * dNSE_Power_at_1percent_nominal_power )
  
  objMCOUT = list()
  objMCOUT$iNumObs    = objIN$iNumObs 
  objMCOUT$iNumSeries = objIN$iNumSeries 
  objMCOUT$objDGP     = objDGP
  objMCOUT$iNumBootDataSets = objIN$iNumBootDataSets
  objMCOUT$mInterval95_for_Power_at_10percent_nominal_power = mInterval95_for_Power_at_10percent_nominal_power
  objMCOUT$mInterval95_for_Power_at_5percent_nominal_power  = mInterval95_for_Power_at_5percent_nominal_power
  objMCOUT$mInterval95_for_Power_at_1percent_nominal_power  = mInterval95_for_Power_at_1percent_nominal_power
  
  # Save output
  save(objMCOUT, file = paste0(objIN$sPath, objIN$sSave, '.RData'), compress = TRUE)
}
f.MonteCarloJointTestPower = compiler::cmpfun(.f.MonteCarloJointTestPower)