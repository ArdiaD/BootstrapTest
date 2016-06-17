#' Point estimates and confidence intervals computing from simulated p-values
#' @param vPvalue : [vector] of p-values
#' @return objTests  : [list] contains point estimates and 95% confidence intervals for the size of 
#' the test, for various significance levels

#' @author Anas Guerrouaz & David Ardia

f.Pvalues_Histogram_and_Tests = function(vPvalue){

iNumExperiments = nrow(vPvalue)

dSize_at_10percent_nominal_size     = mean( vPvalue < 0.10 )
dSize_at_5percent_nominal_size      = mean( vPvalue < 0.05 )
dSize_at_1percent_nominal_size      = mean( vPvalue < 0.01 )

dNSE_Size_at_10percent_nominal_size = sqrt( dSize_at_10percent_nominal_size 
                                            * (1 - dSize_at_10percent_nominal_size)/iNumExperiments )
dNSE_Size_at_5percent_nominal_size  = sqrt( dSize_at_5percent_nominal_size  
                                            * (1 - dSize_at_5percent_nominal_size )/iNumExperiments )
dNSE_Size_at_1percent_nominal_size  = sqrt( dSize_at_1percent_nominal_size  
                                            * (1 - dSize_at_1percent_nominal_size )/iNumExperiments )

vInterval95_for_Size_at_10percent_nominal_size = c(dSize_at_10percent_nominal_size - 
                                                     1.96 * dNSE_Size_at_10percent_nominal_size, 
                                                   dSize_at_10percent_nominal_size + 
                                                     1.96 * dNSE_Size_at_10percent_nominal_size)

vInterval95_for_Size_at_5percent_nominal_size = c(dSize_at_5percent_nominal_size - 
                                                     1.96 * dNSE_Size_at_5percent_nominal_size, 
                                                   dSize_at_5percent_nominal_size + 
                                                     1.96 * dNSE_Size_at_5percent_nominal_size)

vInterval95_for_Size_at_1percent_nominal_size = c(dSize_at_1percent_nominal_size - 
                                                     1.96 * dNSE_Size_at_1percent_nominal_size, 
                                                   dSize_at_1percent_nominal_size + 
                                                     1.96 * dNSE_Size_at_1percent_nominal_size)


# Output
objTests = list() 
objTests$dSize_at_10percent_nominal_size                = dSize_at_10percent_nominal_size 
objTests$vInterval95_for_Size_at_10percent_nominal_size = vInterval95_for_Size_at_10percent_nominal_size 
objTests$dSize_at_5percent_nominal_size                 = dSize_at_5percent_nominal_size 
objTests$vInterval95_for_Size_at_5percent_nominal_size  = vInterval95_for_Size_at_5percent_nominal_size 
objTests$dSize_at_1percent_nominal_size                 = dSize_at_1percent_nominal_size 
objTests$vInterval95_for_Size_at_1percent_nominal_size  = vInterval95_for_Size_at_1percent_nominal_size 

return(objTests)
}