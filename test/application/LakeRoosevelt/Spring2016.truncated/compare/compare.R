# -------------------------------------------------------------
# file: compare.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: 2016-10-12 14:55:23 d3g096
# -------------------------------------------------------------

source("/home/d3g096/src/R/read.R")
source("/home/d3g096/src/R/calcR2.R")

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
sitename <- "Deadmans"
sitetag <- "Deadmans"
simfile <- "../ts130.out"

sitename <- "Gifford"
sitetag <- "Gifford"
simfile <- "../ts595.out"

sitename <- "Snag Cove"
sitetag <- "Snag_Cove"
simfile <- "../ts1155.out"

sitename <- "Onion Creek"
sitetag <- "Onion_Creek"
simfile <- "../ts161.out"

sitename <- "Border"
sitetag <- "Border"
simfile <- "../ts11.out"

sitename <- "French Rocks"
sitetag <- "French_Rocks"
simfile <- "../ts530.out"

sitename <- "Flat Creek"
sitetag <- "Flat_Creek"
simfile <- "../ts1104.out"

sitename <- "Little Dalles"
sitetag <- "Little_Dalles"
simfile <- "../ts169.out"


obsfile <- paste("../Observed/stage/", sitetag, "-Elev.prn", sep="")
plotfile <- paste(sitetag, "-Elev.eps", sep="")

dataname <- "Water Surface Elevation"
var <- "wselev"
factor <- 0.3048
substart <- strptime("05/01/2016 00:00:00", "%m/%d/%Y %H:%M:%S")
subend <- strptime("07/01/2016 00:00:00", "%m/%d/%Y %H:%M:%S")


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

sim <- Read.MASS1.ts(simfile)

sim <- subset(sim, ((datetime >= substart) &
                    (datetime <= subend)))

obs <- Read.BC(obsfile, offset=0,skip=0)

tempsim <- sim$wselev[sim$timestamp %in% obs$timestamp]*(factor)
tempobs <- obs$value[obs$timestamp %in% sim$timestamp]

n <- length(tempsim)

if (n <= 0) q()

postscript(plotfile, ,
           width=6.0, height=6.0,
           paper="letter", horizontal=FALSE)
par(font.main=par('font.lab'))
par(cex.main=par('cex.lab'))
par(mar=c(3,3,2,1)+0.1)
par(mgp=c(2,.75,0))
# par(oma=c(0,0,0,0))
par(cex=1.5)


plot(tempobs, tempsim,
     pch = 3, col=2, cex = 0.5,
     xlab = expression(paste("Observed ", dataname)), 
     ylab = expression(paste("Simulated ", dataname)),
     main = sitename)
abline(0,1)
dev.off()

cat(sitetag, n,
    format(Model.R2(tempobs, tempsim), digits = 2),
    format(Model.Bias(tempobs, tempsim), digits = 2),
    format(Model.RMS(tempobs, tempsim), digits = 2),
    format(Model.AME(tempobs, tempsim), digits = 2),
    format(Model.Estddev(tempobs, tempsim), digits = 2),
    "\n", append=TRUE)



