#' Lower tail testing: this function computes the bootstrapped p-values for
#' the lower tail test of Christoffersen & Pelletier(2004) and the 
#' conditionnal coverage test of Christoffersen(1998)
#'
#' @param mPscores : [matrix] p-scores (they ensue from the realized returns 
#' and the conditionnal model)
#' @param mRets : [matrix] asset returns
#' @param mVaR  : [matrix] VaR values (for the probability (1-p) )
#' @param p : [num] VaR level (the smaller p is, the more the tests focus 
#' on tail events)
#' @param nBoot : [num] number of bootstrap simulations
#'
#' @author David Ardia & Anas Guerrouaz

f.TestLowerTail = function(mPscores, mRets, mVaR, p, nBoot = 100) {
  
  # ==> test lower tail pscores
  #browser()
  mZ_aux_ = f.CrossBootstrap(mPscores, 0, 1, nBoot)$mZ_aux
  iNumSeries = ncol(mPscores)
  
  vLR_stat      = matrix(data = 0, nrow = 1, ncol = iNumSeries)
  vLR_pvalue    = matrix(data = 0, nrow = 1, ncol = iNumSeries)
  mLR_stat_boot = matrix(data = 0, nrow = iNumSeries, ncol = nBoot)
  
  for (i in 1:iNumSeries) {
    # true stat
    viol = mRets[,i] < mVaR[,i]
    Z_i  = qnorm(mPscores[viol,i,drop = FALSE] / p, mean = 0, sd = 1)
    
    # AG fix for p scores equal to 0/1
    Z_i[is.infinite(Z_i) & Z_i > 0] = 1000
    Z_i[is.infinite(Z_i) & Z_i < 0] = -1000
    Z_i[is.nan(Z_i)] = 0
    
    vLR_stat[i] = f.LR_Berkowitz(Z_i)$vLR_stat  
    # bootstrap test
    mLR_stat_boot[i,] = f.LR_Berkowitz(mZ_aux_[,,i])$vLR_stat    
  }    
  pval.Berk = mean(colSums(mLR_stat_boot) >= sum(vLR_stat))
  
  #  ==> test VaR
  trueVaR = qnorm(p, mean = 0, sd = 1) * matrix(data = 1, nrow = nrow(mRets), ncol = nBoot)
  mZ_aux_ = f.CrossBootstrap(mRets, 0, 1, nBoot)$mZ_aux
  
  vLR_stat      = matrix(data = 0, nrow = 1, ncol = iNumSeries)
  vLR_pvalue    = matrix(data = 0, nrow = 1, ncol = iNumSeries)
  mLR_stat_boot = matrix(data = 0, nrow = iNumSeries, ncol = nBoot)
  
  for (i in 1:iNumSeries) {
    # true stat
    vLR_stat[i] = f.LR_UC_CC_IND_(mZ = matrix(mRets[,i]), VaR = matrix(mVaR[,i]), dProb_VaRviolation_under_H0 = p)$vLR_CC
    # bootstrap test
    mLR_stat_boot[i,] = f.LR_UC_CC_IND_(mZ = matrix(mZ_aux_[,,i], ncol = nBoot), VaR = trueVaR, dProb_VaRviolation_under_H0 = p)$vLR_CC
  }  
  pval.CC = mean(colSums(mLR_stat_boot) >= sum(vLR_stat))
  
  out = list(pval.Berk = pval.Berk, pval.CC = pval.CC)
  return(out)
}

f.LR_UC_CC_IND_ = function(mZ, VaR, dProb_VaRviolation_under_H0){
  
  iNumObs   = nrow( mZ )
  iN        = iNumObs - 1  
  
  mIndicator_violations = mZ < VaR
  
  ids1 = 1:(iNumObs - 1)
  ids2 = 2:iNumObs
  
  if (ncol(mZ) == 1) {
    vN00 = sum( (mIndicator_violations[ids1, ] == 0) * (mIndicator_violations[ids2, ] == 0) )
    vN01 = sum( (mIndicator_violations[ids1, ] == 0) * (mIndicator_violations[ids2, ] == 1) )
    vN10 = sum( (mIndicator_violations[ids1, ] == 1) * (mIndicator_violations[ids2, ] == 0) )
    vN11 = sum( (mIndicator_violations[ids1, ] == 1) * (mIndicator_violations[ids2, ] == 1) )
  } else {
    vN00 = colSums( (mIndicator_violations[ids1, ] == 0) * (mIndicator_violations[ids2, ] == 0) )
    vN01 = colSums( (mIndicator_violations[ids1, ] == 0) * (mIndicator_violations[ids2, ] == 1) )
    vN10 = colSums( (mIndicator_violations[ids1, ] == 1) * (mIndicator_violations[ids2, ] == 0) )
    vN11 = colSums( (mIndicator_violations[ids1, ] == 1) * (mIndicator_violations[ids2, ] == 1) )
  }
  vN0  = vN00 + vN10
  vN1  = vN01 + vN11
  
  vLogL_H0 = vN0*log( 1 - dProb_VaRviolation_under_H0 ) + vN1*log( dProb_VaRviolation_under_H0 )
  
  vLogL_H1_UC = vN0 * log( (vN0 * (1/iN)) + (vN0 == 0) ) + vN1 * log( (vN1 * (1/iN)) + (vN1 == 0) )
  
  vLogL_H1_CC = vN00 * log( (vN00 /(vN00 + vN01 + (vN00 == 0)) ) + (vN00 == 0) ) + 
    vN01 * log( (vN01 /(vN00 + vN01 + (vN01 == 0)) ) + (vN01 == 0) ) + 
    vN10 * log( (vN10 /(vN10 + vN11 + (vN10 == 0)) ) + (vN10 == 0) ) + 
    vN11 * log( (vN11 /(vN10 + vN11 + (vN11 == 0)) ) + (vN11 == 0) )
  
  vLR_UC  = 2 * (vLogL_H1_UC - vLogL_H0)
  vLR_CC  = 2 * (vLogL_H1_CC - vLogL_H0)
  vLR_IND = vLR_CC - vLR_UC
  
  vpval_UC  = 1 - pchisq(q = vLR_UC, df = 1)
  vpval_CC  = 1 - pchisq(q = vLR_CC, df = 2)
  vpval_IND = 1 - pchisq(q = vLR_IND, df = 1)
  
  out = list(vLR_UC = vLR_UC, vLR_CC = vLR_CC, vLR_IND = vLR_IND,
             vpval_UC = vpval_UC, vpval_CC = vpval_CC, vpval_IND = vpval_IND)
  return(out)
}