
# ==> Run Monte Carlo size testing

rm(list = ls())
source("./sourceall.R")
#enableJIT(3)

iNumCore         <- 35   # Number of cores to be used
iRandseed        <- 1   # Starding seed
iNumExperiments  <- 500  # MC replications (number of p-values)
iNumBootDataSets <- 500  # Number of bootstrap replication per p-value

objIN <- list()
objIN$iNumCore         <- iNumCore    
objIN$iRandseed        <- iRandseed
objIN$iNumExperiments  <- iNumExperiments  
objIN$iNumBootDataSets <- iNumBootDataSets 
objIN$sPath <- './results/'

objIN$sSave <- 'size_CC_100_1'; objIN$sType <- 'CC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 1; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_CC_100_10'; objIN$sType <- 'CC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 10; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_CC_100_100'; objIN$sType <- 'CC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 100; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_CC_1000_1'; objIN$sType <- 'CC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 1; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_CC_1000_10'; objIN$sType <- 'CC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 10; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_CC_1000_100'; objIN$sType <- 'CC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 100; 
f.MonteCarloJointTestSize(objIN)

objIN$sSave <- 'size_DCC_100_1'; objIN$sType <- 'DCC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 1; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_DCC_100_10'; objIN$sType <- 'DCC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 10; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_DCC_100_100'; objIN$sType <- 'DCC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 100; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_DCC_1000_1'; objIN$sType <- 'DCC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 1; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_DCC_1000_10'; objIN$sType <- 'DCC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 10; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_DCC_1000_100'; objIN$sType <- 'DCC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 100; 
f.MonteCarloJointTestSize(objIN)

objIN$sSave <- 'size_2RC_100_1'; objIN$sType <- '2RC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 1; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_2RC_100_10'; objIN$sType <- '2RC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 10; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_2RC_100_100'; objIN$sType <- '2RC'; objIN$iNumObs <- 100; objIN$iNumSeries <- 100; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_2RC_1000_1'; objIN$sType <- '2RC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 1; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_2RC_1000_10'; objIN$sType <- '2RC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 10; 
f.MonteCarloJointTestSize(objIN)
objIN$sSave <- 'size_2RC_1000_100'; objIN$sType <- '2RC'; objIN$iNumObs <- 1000; objIN$iNumSeries <- 100; 
f.MonteCarloJointTestSize(objIN)

stop("break here")

#################################################################
# ==> Display results

rm(list = ls())
source("./sourceall.R")

str_ = c('./results/size_CC_100_1.rdata', 100, 1,'./results/size_CC_100_10.rdata', 100, 10,
         './results/size_CC_100_100.rdata', 100, 100,'./results/size_CC_1000_1.rdata', 1000, 1,
         './results/size_CC_1000_10.rdata', 1000, 10,'./results/size_CC_1000_100.rdata', 1000, 100)
model = 'CC'


str_ = c('./results/size_DCC_100_1.rdata', 100, 1,'./results/size_DCC_100_10.rdata', 100, 10,
         './results/size_DCC_100_100.rdata', 100, 100,'./results/size_DCC_1000_1.rdata', 1000, 1,
         './results/size_DCC_1000_10.rdata', 1000, 10,'./results/size_DCC_1000_100.rdata', 1000, 100)
model = 'DCC'


str_ = c('./results/size_2RC_100_1.rdata', 100, 1,'./results/size_2RC_100_10.rdata', 100, 10,
         './results/size_2RC_100_100.rdata', 100, 100,'./results/size_2RC_1000_1.rdata', 1000, 1,
         './results/size_2RC_1000_10.rdata', 1000, 10,'./results/size_2RC_1000_100.rdata', 1000, 100)
model = '2RC'


### Plots for size point estimates & 95% confidence intervals
# Select the appropriate str_ and model then generate the plots

f.PlotSize(str_, model)

