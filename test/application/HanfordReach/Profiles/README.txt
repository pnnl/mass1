-------------------------------------------------------------
file: README.txt
-------------------------------------------------------------
-------------------------------------------------------------
Battelle Memorial Institute
Pacific Northwest Laboratory
-------------------------------------------------------------
-------------------------------------------------------------
Created July 23, 2012 by William A. Perkins
Last Change: Mon Jul 23 12:21:53 2012 by William A. Perkins <d3g096@flophouse>
-------------------------------------------------------------

MASS1 was used to simulate steady Hanford Reach discharges from 20 to
500 kcfs.  Simulated water surface elevation and discharge was
extracted at intervals of 5km and plotted to provide a stage/discharge
relationship at each location.

The files are as follows:

 - hanfordrkm5.*: a "shape file" containing the locations at which
   MASS1 data was extracted. Its coordinate system is WA South state
   plane, NAD83 datum, and units of feet.  

 - stage_discharge_###.dat: stage and discharge data for a specific
   river kilometer where ### is between 550 and 635, inclusive.  These
   file have the following fields:

   - date/time, which is meaningless
   - river mile
   - water surface elevation, ft NGVD29
   - discharge, cfs

 - stage_discharge_###.png: a plot of stage_discharge_###.dat


Bill Perkins
Research Engineer
Hydrology Group

Pacific Northwest National Laboratory
902 Battelle Boulevard
P.O. Box 999, MSIN K9-36
Richland, WA  99352 USA
Tel:  509-372-6131
Fax: 509-372-6089
william.perkins@pnnl.gov
www.pnnl.gov
 