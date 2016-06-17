options(digits = 4, max.print = 1000, prompt = "> ", warn = -1)

# install.packages('mvtnorm')
# install.packages("rugarch")
# install.packages("plotrix")
# install.packages("zoo")
# install.packages('gplots')
# install.packages('tseries')
# install.packages("abind")
# install.packages("snowfall")

## Load packages
#require("xts")
#require("tseries")
#require("R.matlab")
#require("rugarch")
require("compiler")
require("mvtnorm")
require("zoo")
#require("snow")

str <- c("./library/core/", "./library/distributions/", 
         "./library/tests/", "./library/simulation/","./library/plots/", "./library/data importation/", 
         "./library/backtest/", "./library/fdr/")

file.sources <- list.files(str, pattern = "*.R$", full.names = TRUE, ignore.case = TRUE)
sapply(file.sources, source, .GlobalEnv)