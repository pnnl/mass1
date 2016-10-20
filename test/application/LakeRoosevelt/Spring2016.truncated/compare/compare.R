# -------------------------------------------------------------
# file: compare.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: 2016-10-20 13:01:18 d3g096
# -------------------------------------------------------------

source("/home/d3g096/src/R/read.R")
source("/home/d3g096/src/R/calcR2.R")

# -------------------------------------------------------------
# scatterplot
# -------------------------------------------------------------
scatterplot <- function(plotfile, tmpobs, tmpsim, dataname, sitename) {
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
       xlab = paste("Observed ", dataname), 
       ylab = paste("Simulated ", dataname),
       main = sitename)
  abline(0,1)
  dev.off()
}

# -------------------------------------------------------------
# printstats
# -------------------------------------------------------------
printstats <- function(sitetag, tempobs, tmpsim) {
  cat(sitetag, length(tempsim),
      format(Model.R2(tempobs, tempsim), digits = 3),
      format(Model.Bias(tempobs, tempsim), digits = 3),
      format(Model.RMS(tempobs, tempsim), digits = 3),
      format(Model.AME(tempobs, tempsim), digits = 3),
      format(Model.Estddev(tempobs, tempsim), digits = 3),
      "\n", append=TRUE)
}  

# -------------------------------------------------------------
# variable initialization
# -------------------------------------------------------------
site <-
  data.frame(name <- c("Deadmans", "Gifford", "Snag_Cove",
                       "Onion Creek", "Border", "French Rocks",
                       "Flat Creek", "Little Dalles",
                       "USGS 12399500", "USGS 12399500 Auxillary"),
             tag  <- c("Deadmans", "Gifford", "Snag_Cove",
                       "Onion_Creek", "Border", "French_Rocks",
                       "Flat_Creek", "Little_Dalles",
                       "USGS_12399500", "USGS_12399500_AUX"),
             simid <- c("130", "595", "1155", "161", "11", "530", "1104", "169", "11", "110"),
             rm <- c(151.71, 87.40, 126.39, 144.07,
                     159.04, 103.81, 134.89, 142.15,
                     159.10, 156.75)
             )
site$simfile <- paste("../ts", site$simid, ".out", sep="")
site$obsstage <- paste("../Observed/stage/", site$tag, "-Elev.prn", sep="")
site$title <- paste("CCT ", site$name, ", RM ", format(site$rm, digits=2), sep="")

# order by river mile
site <- site[order(-site[,4]), ]
             
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

dataname <- "Water Surface Elevation, m"
factor <- 0.3048
substart <- strptime("04/01/2016 00:00:00", "%m/%d/%Y %H:%M:%S")
subend <- strptime("9/07/2016 00:00:00", "%m/%d/%Y %H:%M:%S")


# -------------------------------------------------------------
# main program
# -------------------------------------------------------------

nsite <- length(site$tag)
for (i in 1:nsite) {


  plotfile <- paste(site$tag[i], "-Elev.eps", sep="")

  sim <- Read.MASS1.ts(site$simfile[i])
  sim <- subset(sim, ((datetime >= substart) &
                      (datetime <= subend)))
  obs <- Read.BC(site$obsstage[i], offset=0,skip=0)

  tempsim <- sim$wselev[sim$timestamp %in% obs$timestamp]*(factor)
  tempobs <- obs$value[obs$timestamp %in% sim$timestamp]

  if (length(tempsim) > 0) {
    scatterplot(plotfile, tempobs, tempsim, dataname, site$title[i])
    printstats(as.character(site$tag[i]), tempobs, tempsim)
  }    
}


