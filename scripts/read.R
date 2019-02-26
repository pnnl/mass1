# -------------------------------------------------------------
# file: readbc.R
# -------------------------------------------------------------
# -------------------------------------------------------------
# Copyright (c) 2017 Battelle Memorial Institute
# Licensed under modified BSD License. A copy of this license can be
# found in the LICENSE file in the top level directory of this
# distribution.
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created October 25, 2001 by William A. Perkins
# Last Change: 2017-06-22 11:46:05 d3g096
# -------------------------------------------------------------

# library(chron)

# -------------------------------------------------------------
# Read.DateTime
#
# For R 1.8.x, make sure that the environment variable TZ is set to "PST"
#
# -------------------------------------------------------------
Read.DateTime <- function(obj, offset = 0) {
  # obj$datetime <- chron(obj$date, obj$time, format = c("m-d-y", "h:m:s"))
  obj$datetime <- as.POSIXct(strptime(paste(obj$date, obj$time),
                                      "%m-%d-%Y %H:%M:%S"))
  obj$datetime <- obj$datetime + offset*24.0*3600.0
#   obj$timestamp <- format(obj$datetime)
#   obj$month <- months(obj$datetime)
#   obj$day <- days(obj$datetime)
#   obj$year <- years(obj$datetime)
#   obj$julian <- julian(obj$month, obj$day, obj$year) - julian(1, 1, obj$year) + 1
  obj$timestamp <- strftime(as.POSIXlt(obj$datetime, tz=""),
                            format="%m-%d-%Y %H:%M:%S",
                            usetz=FALSE)
  obj$month <- as.integer(format(as.POSIXlt(obj$datetime), format="%m"))
  obj$day <- as.integer(format(as.POSIXlt(obj$datetime), format="%d"))
  obj$year <- as.integer(format(as.POSIXlt(obj$datetime), format="%Y"))
  obj$julian <- as.integer(format(as.POSIXlt(obj$datetime), format="%j"))
#   obj$julian <- julian(obj$month, obj$day, obj$year) - julian(1, 1, obj$year) + 1
  obj
}

# -------------------------------------------------------------
# Read.BC
# This routine is used to read a MASS1/2 formatted BC file (with a
# single value)
# -------------------------------------------------------------
Read.BC <- function(fname, col.names = c("date", "time", "value"),
                    offset = 0, skip = 1) {
  bcdat <- read.table(fname, header=FALSE, as.is = TRUE,
                    col.names = col.names, skip = skip, comment.char = "/")
  bcdat <- Read.DateTime(bcdat, offset=offset)
  # bcdat$timestamp <- paste(bcdat$date, bcdat$time, sep = " ")
  bcdat
}

# -------------------------------------------------------------
# Read.MASS1.ts
# This routine reads and stores a gage time series output from MASS1.
# -------------------------------------------------------------
Read.MASS1.ts <- function(fname, skip=1) {
  mass1names <-
    c("date",     "time",     "wselev",   "discharge",
      "vel",      "depth",    "conc",     "temp",
      "saturation",           "press",    "thalweg",
      "area",     "topwid",   "hydrad",   "froude",
      "fslope",   "bedshear")

  mass1tmp <- read.table(fname, header=FALSE, as.is = TRUE,
                         col.names=mass1names,skip=skip)
  mass1tmp <- Read.DateTime(mass1tmp)
  # mass1tmp$timestamp <- paste(mass1tmp$date, mass1tmp$time, sep = " ")
  mass1tmp
}


