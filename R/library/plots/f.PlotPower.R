#' Main plotting function  for the power study
#' @param shiftall : [boolean] if set to TRUE (default), all series are shifted. Otherwise, only some series are shifted.
#' @param rho : correlation values for which power was computed

#' @author Anas Guerrouaz

f.PlotPower = function(rho, shiftall = TRUE){

  rho_str = gsub("\\.", "", toString(rho))
  if (rho_str == "0") {
    rho_str = "00"
  }
  
  if (isTRUE(shiftall)) {
    leg    = c('N=1', 'N=10', 'N=100')
    legloc = 'bottomright'
    pname  = paste0("power_shift_", rho_str)
    
    load(paste0('./results/power_shift_001_', rho_str, '.RData')); shift1 = objMCOUT
    load(paste0('./results/power_shift_010_', rho_str, '.RData')); shift2 = objMCOUT
    load(paste0('./results/power_shift_100_', rho_str, '.RData')); shift3 = objMCOUT
  } else {
    leg    = c('M=1', 'M=5', 'M=10')
    legloc = 'topleft'
    pname  = paste0("power_shift_pct_", rho_str)
    
    load(paste0('./results/power_shift_pct_01_', rho_str, '.RData')); shift1 = objMCOUT
    load(paste0('./results/power_shift_pct_05_', rho_str, '.RData')); shift2 = objMCOUT
    load(paste0('./results/power_shift_pct_10_', rho_str, '.RData')); shift3 = objMCOUT
  }

  cl = c('black', 'blue', 'red')
  #pdf(file = paste0(pname, ".pdf"))
  postscript(file = paste0(pname, ".eps"))
  par(mfrow = c(1,1))
  plot(0, 0, xlim = c(0,0.5), ylim = c(0,1), type = "n", xlab = expression(mu), 
       ylab =  "power", cex.lab = 1.6, xaxs = "i", yaxs = "i", bty = 'n', las = 1)
  grid(10)
  box()
  abline(h = 1, v = 0.5, lty = 'dotted', col = 'grey')
  lines(x = shift1$objDGP$dShift, y = shift1$mInterval95_for_Power_at_5percent_nominal_power[1,], col = cl[1], type = 'l')
  lines(x = shift1$objDGP$dShift, y = shift1$mInterval95_for_Power_at_5percent_nominal_power[2,], col = cl[1], type = 'l')
  lines(x = shift2$objDGP$dShift, y = shift2$mInterval95_for_Power_at_5percent_nominal_power[1,], col = cl[2], type = 'o', pch = 'x')
  lines(x = shift2$objDGP$dShift, y = shift2$mInterval95_for_Power_at_5percent_nominal_power[2,], col = cl[2], type = 'o', pch = 'x')
  lines(x = shift3$objDGP$dShift, y = shift3$mInterval95_for_Power_at_5percent_nominal_power[1,], col = cl[3], type = 'o', pch = 'o')
  lines(x = shift3$objDGP$dShift, y = shift3$mInterval95_for_Power_at_5percent_nominal_power[2,], col = cl[3], type = 'o', pch = 'o')
  title(bquote(rho == .(rho)), cex.main = 1.5)
  legend(x = legloc, legend = leg, pch = c("-", "x", "o"), col = cl, cex = 1.2)
  dev.off()
}