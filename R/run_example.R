## Code for illustrating the bootstrap approach presented in the paper
## "A New Bootstrap Test for Multiple Assets Joint Risk Testing"
## by Ardia, Hoogerheide and Gatarek
## available at http://papers.ssrn.com/sol3/papers.cfm?abstract_id=2312007

# create "fake" PIT matrix for illustration
T  = 100
d  = 5
set.seed(1234)
mX = matrix(data = runif(T*d), nrow = T, ncol = d)
print(mX)

## simulation of one matrix of PIT under H0 using the boostrap approach
T  = nrow(mX)
d  = ncol(mX)
mR = apply(mX, MARGIN = 2, FUN = rank)
vV = sample(1:T, size = T, replace = TRUE)
mX_boot = matrix(data = NA, nrow = T, ncol = d)
set.seed(1234)
for (i in 1:d) {
  vV_i= mR[vV,i]
  mX_boot[,i] = rbeta(n = T, shape1 = vV_i, shape2 = (T + 1) - vV_i)
}
print(mX_boot)