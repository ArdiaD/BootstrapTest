#' Core function for the cross bootstrap test by Lennart
#' @param mZ : [matrix] (T x d) of observations
#' @param dMu     : [scalar] mean
#' @param dSigma2 : [scalar] variance
#' @param nBoot   : [scalar] number of bootstrap replications
#' @return mZ_aux  : [array] (T x nBoot x d)
#' @note For each series (there are d series in total), we create a matrix with (T x nBoot) simulated observations 
#' as Dimension d is typically smaller than nBoot (especially in simple examples), so that it is arguably
#' better (i.e., computationally faster) to have a for loop over the series than over the bootstrapped data sets.

#' @author Anas Guerrouaz & David Ardia

.f.CrossBootstrap = function(mZ, dMu, dSigma2, nBoot){
  
  T = nrow(mZ)
  d = ncol(mZ)
  
  # compute matrix of rank numbers corresponding to mZ:
  mR = apply(mZ, MARGIN = 2, FUN = rank)
  
  # T x 1 column vector of which rows we simulate the dependence (which ranking numbers):
  vDependence_of_which_rows = sample.int(T, size = T * nBoot, replace = TRUE)
  
  mZ_aux = mU_aux = array(data = NaN, dim = c(T, nBoot, d))
  
  for (i in 1:d) {
    vTemp_aux   = mR[vDependence_of_which_rows,i]
    vRndBeta    = rbeta(n = T * nBoot, shape1 = vTemp_aux, shape2 = (T + 1) - vTemp_aux)
    mU_aux[,,i] = vRndBeta
    mZ_aux[,,i] = qnorm(p = vRndBeta, mean = dMu, sd = sqrt(dSigma2)) 
  }
  
  out = list(mZ_aux = mZ_aux, mU_aux = mU_aux)
  return(out)
}
f.CrossBootstrap = compiler::cmpfun(.f.CrossBootstrap)

# f.qnorm = function(p, mu, sigma){
#   # Normal inverse function for speedup
#   sigma[sigma <= 0] = NaN;
#   p[p < 0 | 1 < p] = NaN;
#   x0 = -sqrt(2) * erfcinv(2 * p);
#   x = sigma .* x0 + mu;
#   idx = isinf(x) & x > 0;
#   x(idx) = 10;
#   idx = isinf(x) & x < 0;
#   x(idx) = -10;
# }

# 
# function [r] = fBetarnd(a, b)
# % Random arrays from beta distribution for speedup
# 
# [d1, d2] = size(a);
# 
# % Generate gamma random values and take ratio of the first to the sum.
# g1 = randg(a, d1, d2); % could be Infs or NaNs
# g2 = randg(b, d1, d2); % could be Infs or NaNs
# r = g1 ./ (g1 + g2);
# 
# % For a and b both very small, we often get 0/0.  Since the distribution is
# % essentially a Bernoulli(a/(a+b)), we can replace those NaNs.
# t = (g1 == 0 & g2 == 0);
# if any(t)
# p = a ./ (a+b);
# if ~isscalar(p), p = p(t); end
# r(t) = binornd(1,p(:),sum(t(:)),1);
# end
# 
# end

