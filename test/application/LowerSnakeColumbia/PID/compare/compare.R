# -------------------------------------------------------------
# file: compare.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: Fri Oct 26 14:00:58 2007 by William A. Perkins <perk@bearflag.pnl.gov>
# -------------------------------------------------------------

source("/home/perk/src/R/read.R")
source("/home/perk/src/R/calcR2.R")

sim <- Read.MASS1.ts("@SIMFILE@")

obs <- Read.BC("@OBSFILE@", offset=0)

tempsim <- sim$@VAR@[sim$timestamp %in% obs$timestamp]*(@FACTOR@)
tempobs <- obs$value[obs$timestamp %in% sim$timestamp]*(@FACTOR@)

n <- length(tempsim)

if (n <= 0) q()

postscript("@OUTPS@",
           width=6.0, height=6.0,
           paper="letter", horizontal=FALSE)

par(font.main=par('font.lab'))
par(cex.main=par('cex.lab'))
par(mar=c(3,3,2,1)+0.1)
par(mgp=c(2,.75,0))
# par(oma=c(0,0,0,0))
par(cex=1.5)

plot(tempobs, tempsim,
     pch = 3, col = 2, cex = 0.5,
     xlab = "Observed @DATA@", 
     ylab = "Simulated @DATA@",
     main = "@NAME@")
abline(0,1)
dev.off()

cat("@TAG@", n,
    format(Model.R2(tempobs, tempsim), digits = 2),
    format(Model.Bias(tempobs, tempsim), digits = 2),
    format(Model.RMS(tempobs, tempsim), digits = 2),
    format(Model.AME(tempobs, tempsim), digits = 2),
    format(Model.Estddev(tempobs, tempsim), digits = 2),
    "\n", file="@STATSFILE@", append=TRUE)



