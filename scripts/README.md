# MASS1 Utility Scripts

These utilities are provided as-is. There is little or no
documentation (except the code itself).  Some are still used often by the
authors; others are quite old and in probably in need of maintenance.  


* `bcsteps.py` 

  This is used to generate a stepped boundary condition file,
  typically used when simulating a series of steady state conditions
  in a single simulation. 
  
* `mass1bc` 

  Extracts a single field from [gage output](../doc/gage.md) and makes
  a correctly formatted [boundary condition](../doc/bc.md) file. 

* `mass1profile`

  Provides various statistical analyses of profile output.  

* `profile2cgns.py`

* `profile_extract.py`

   Parse [profile output](../doc/profile.md) and produce time series
   and specific locations. Results are linearly interpolated between
   profile points.  

* `read.R`

   Some [R](https://www.r-project.org/) routines to read
   [gage output](../doc/gage.md) and
   [boundary condition](../doc/bc.md) files. 

* `timeshift.py`

   Simple script to offset the time in a
   [boundary condition](../doc/bc.md) file.  


## Utilities for Tecplot

* `profile_tecplot.py`
 
  This [Python](https://www.python.org/) script parses
  [profile output](../doc/profile.md) and produces a
  [Tecplot](http://www.tecplot.com/) ASCII data file.  One zone is
  produced for each time in the input file.  
  
* `xy-looper.mcr`

  For X-Y plots.  For each *zone* in the current data set, update the
  X-Y graph to use that zone, and produce an AVI format animation.  

* `xy-looper-png.mcr`

  Same as `xy-looper.mcr`, but generates one PNG format image file for
  each zone.  
