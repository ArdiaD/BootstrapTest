# ==> Run Monte Carlo power testing

rm(list = ls())
source("./sourceall.R")

iNumCore         <- 35    # Number of cores to be used
iNumObs          <- 100   # Number of observations
iNumExperiments  <- 500   # MC replications (number of p-values)
iNumBootDataSets <- 500   # Number of bootstrap replication per p-value
iRandseed        <- 1     

objIN <- list()
objIN$iNumCore         <- iNumCore  
objIN$iNumObs          <- iNumObs
objIN$iNumExperiments  <- iNumExperiments  
objIN$iNumBootDataSets <- iNumBootDataSets 
objIN$iRandseed        <- iRandseed 
objIN$sPath            <- './results/'

objIN$iPercentSeries <- 1

### Power with shifting
objIN$dShift = seq(from = 0, to = 0.5, length.out = 35)
objIN$dScale = rep(1, 35)

objIN$dCorrelationBetweenSeries = 0;
objIN$sSave = 'power_shift_001_00'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_010_00'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_100_00'; objIN$iNumSeries = 100; f.MonteCarloJointTestPower(objIN);

objIN$dCorrelationBetweenSeries = 0.5;
objIN$sSave = 'power_shift_001_05'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_010_05'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_100_05'; objIN$iNumSeries = 100; f.MonteCarloJointTestPower(objIN);

objIN$dCorrelationBetweenSeries = 0.7;
objIN$sSave = 'power_shift_001_07'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_010_07'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_100_07'; objIN$iNumSeries = 100; f.MonteCarloJointTestPower(objIN);

objIN$dCorrelationBetweenSeries = 0.9;
objIN$sSave = 'power_shift_001_09'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_010_09'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_100_09'; objIN$iNumSeries = 100; f.MonteCarloJointTestPower(objIN);

### Power with shifting and percentage change
objIN$iNumSeries = 10

objIN$dShift = seq(from = 0, to = 0.5, length.out = 35)
objIN$dScale = rep(1, 35)

objIN$dCorrelationBetweenSeries = 0;
objIN$sSave = 'power_shift_pct_01_00'; objIN$iPercentSeries = 0.1; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_05_00'; objIN$iPercentSeries = 0.5; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_10_00'; objIN$iPercentSeries = 1.0; f.MonteCarloJointTestPower(objIN);

objIN$dCorrelationBetweenSeries = 0.5;
objIN$sSave = 'power_shift_pct_01_05'; objIN$iPercentSeries = 0.1; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_05_05'; objIN$iPercentSeries = 0.5; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_10_05'; objIN$iPercentSeries = 1.0; f.MonteCarloJointTestPower(objIN);

objIN$dCorrelationBetweenSeries = 0.7;
objIN$sSave = 'power_shift_pct_01_07'; objIN$iPercentSeries = 0.1; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_05_07'; objIN$iPercentSeries = 0.5; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_10_07'; objIN$iPercentSeries = 1.0; f.MonteCarloJointTestPower(objIN);

objIN$dCorrelationBetweenSeries = 0.9;
objIN$sSave = 'power_shift_pct_01_09'; objIN$iPercentSeries = 0.1; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_05_09'; objIN$iPercentSeries = 0.5; f.MonteCarloJointTestPower(objIN);
objIN$sSave = 'power_shift_pct_10_09'; objIN$iPercentSeries = 1.0; f.MonteCarloJointTestPower(objIN);

########################################################
# Display results

rm(list = ls())
source("./sourceall.R")

rho = c(0.0, 0.5, 0.7, 0.9)
#rho = c(0.0)

# ==> shifting
for (i in 1:length(rho)) {
  f.PlotPower(shiftall = TRUE, rho = rho[i])
}

# ==> shifting (percentage)
for (i in 1:length(rho)) {
  f.PlotPower(shiftall = FALSE, rho = rho[i])
}

# ### Power with shifting for the table correspondance
# objIN = list()
# objIN$iNumCore         = iNumCore
# objIN$iNumExperiments  = iNumExperiments
# objIN$iNumBootDataSets = iNumBootDataSets
# objIN$sPath = './results/'
# objIN$iPercentSeries = 1
# 
# objIN$dShift = seq(from = 0, to = 0.5, length.out = 25)
# objIN$dScale = rep(1, 25)
# 
# objIN$iNumObs = 100
# objIN$dCorrelationBetweenSeries = 0
# objIN$sSave = 'power_table_0100_001_00'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_005_00'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_010_00'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_025_00'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_050_00'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# objIN$dCorrelationBetweenSeries = 0.5
# objIN$sSave = 'power_table_0100_001_05'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_005_05'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_010_05'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_025_05'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0100_050_05'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# 
# objIN$iNumObs = 250
# objIN$dCorrelationBetweenSeries = 0
# objIN$sSave = 'power_table_0250_001_00'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_005_00'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_010_00'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_025_00'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_050_00'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# objIN$dCorrelationBetweenSeries = 0.5
# objIN$sSave = 'power_table_0250_001_05'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_005_05'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_010_05'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_025_05'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0250_050_05'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# 
# objIN$iNumObs = 500
# objIN$dCorrelationBetweenSeries = 0
# objIN$sSave = 'power_table_0500_001_00'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_005_00'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_010_00'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_025_00'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_050_00'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# objIN$dCorrelationBetweenSeries = 0.5
# objIN$sSave = 'power_table_0500_001_05'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_005_05'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_010_05'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_025_05'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_0500_050_05'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# 
# objIN$iNumObs = 1000
# objIN$dCorrelationBetweenSeries = 0
# objIN$sSave = 'power_table_1000_001_00'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_005_00'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_010_00'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_025_00'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_050_00'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);
# objIN$dCorrelationBetweenSeries = 0.5
# objIN$sSave = 'power_table_1000_001_05'; objIN$iNumSeries = 001; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_005_05'; objIN$iNumSeries = 005; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_010_05'; objIN$iNumSeries = 010; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_025_05'; objIN$iNumSeries = 025; f.MonteCarloJointTestPower(objIN);
# objIN$sSave = 'power_table_1000_050_05'; objIN$iNumSeries = 050; f.MonteCarloJointTestPower(objIN);