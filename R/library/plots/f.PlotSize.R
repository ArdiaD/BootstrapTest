#' Main plotting function  for the size study
#' @param str_ : [list] contains the file names of the size study outputs
#' @param model : [string] model used for the size study {CC, DCC, 2RC}

#' @author Anas Guerrouaz

f.PlotSize = function(str_, model){
  # plotrix : package for CI plotting capability
  require("plotrix")
  
  tbl = matrix(data = 0, nrow = 11 , ncol = length(str_)/3)
  
  for (k in 1:(length(str_)/3)) {
    load(str_[1 + (k - 1)*3])
    tmp = c(str_[2 + (k - 1)*3], str_[3 + (k - 1)*3])
    tmp = c(tmp, objOUT$Berk$dSize_at_1percent_nominal_size, objOUT$Berk$vInterval95_for_Size_at_1percent_nominal_size)
    tmp = c(tmp, objOUT$Berk$dSize_at_5percent_nominal_size, objOUT$Berk$vInterval95_for_Size_at_5percent_nominal_size)
    tmp = c(tmp, objOUT$Berk$dSize_at_10percent_nominal_size, objOUT$Berk$vInterval95_for_Size_at_10percent_nominal_size)
    tbl[ , k] = as.numeric(tmp)
  }
  tbl = t(tbl)
  
  postscript(file = paste0('size_', model, '.eps'))
  m <- rbind(c(0, 1, 0.9, 1), c(0.1, 0.4, 0, 0.9), c(0.4, 0.7, 0, 0.9), c(0.7, 1, 0, 0.9))
  split.screen(m)
  
  screen(1)
  title(paste0(model, " model"), cex.main = 1.5)
  
  # 1% confidence level
  screen(2)
  par(mar = c(2, 2, 2, 2))
  f.PlotIntervals(tbl[,3:5])
  abline(v = 0.01, col = 4, lty = 2)
  title(expression(alpha * " = 0.01"), cex.main = 1.5)
  
  # 5% confidence level
  screen(3)
  par(mar = c(2, 2, 2, 2))
  f.PlotIntervals(tbl[,6:8])
  abline(v = 0.05, col = 4, lty = 2)
  title(expression(alpha * " = 0.05"), cex.main = 1.5)
  
  # 10% confidence level
  screen(4)
  par(mar = c(2, 2, 2, 2))
  f.PlotIntervals(tbl[,9:11])
  abline(v = 0.1, col = 4, lty = 2)
  title(expression(alpha * " = 0.1"), cex.main = 1.5)
  
  close.screen(all.screens = TRUE)
  
  # Save the plots to the /results folder
  #dev.copy(jpeg, paste0('Size_', model, '.jpeg'))
  #dev.copy2eps(file = paste0('Size_', model, '.eps'))
  dev.off()
}