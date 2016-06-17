#' Compute the pscore test for a matrix of p-scores

#' @author David Ardia

f.TestPscore = function(mPscore, nBoot = 100) {
  
  mZ  = qnorm(mPscore, mean = 0, sd = 1)
  idx = apply(is.nan(mZ) | is.infinite(mZ), 1, any)
  mZ  = mZ[!idx,,drop = FALSE]
  
  iNumSeries = ncol(mZ)
  mZ_aux_ = f.CrossBootstrap(mZ, dMu = 0, dSigma2 = 1, nBoot)$mZ_aux
  
  vLR_stat = f.LR_Berkowitz(mZ)$vLR_stat
  
  mLR_stat_boot = matrix(data = 0, nrow = iNumSeries, ncol = nBoot)
  for (i in 1:iNumSeries) {
    mLR_stat_boot[i,] = f.LR_Berkowitz( mZ_aux_[,,i] )$vLR_stat   
  }    
  pval.Berk = mean(colSums(mLR_stat_boot) >= sum(vLR_stat))
  
  out = list(pval.Berk = pval.Berk)
  return(out)
}
