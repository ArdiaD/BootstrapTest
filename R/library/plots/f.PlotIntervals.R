#' Plotting function for the test size estimates
#' @param tbl : [matrix] contains point estimates for the size of the test as well as the 95% C.I. bounds
#' @note Most display adjustments are made in f.Plots

#' @author Anas Guerrouaz

f.PlotIntervals = function(tbl){
  
  #y-axis labels
  txt <- c("T=100\nN=1","T=100\nN=10","T=100\nN=100","T=1000\nN=1","T=1000\nN=10","T=1000\nN=100")
  #txt <- c(c("T=100", "N=1"),c("T=100", "N=10"),"T=100 N=100","T=1000 N=1","T=1000 N=10","T=1000 N=100")
  #x-axis scaling for optimal display
  xlim1 = round(min(tbl[,2]) - 0.15*(max(tbl[,2]) - min(tbl[,2])), digits = 2)
  xlim2 = round(max(tbl[,3]) + 0.15*(max(tbl[,3]) - min(tbl[,3])), digits = 2)
  
  y = 1:6
  
  plotrix::plotCI(tbl[,1], y, ui = tbl[,3], li = tbl[,2], xlim = c(xlim1,xlim2), ylim = c(1,6), err = "x", col = 4, yaxt = 'n', ann = FALSE)
  mtext(txt, side = 2, line = 0, at = y, adj = 1, las = 1, cex = 1)
}