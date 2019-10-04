# Boundary Condition (BC) Input

## BC Table Files

MASS1 boundary condition files are organized into the following
categories:

* Link BC

* Hydro BC

* Later Inflow

* Temperature

* TDG

* PID

A BC table file is used for each of these. With the exception of PID,
the name of the BC table file is specified in the
[configuration.md](configuration file). The PID BC table must be named
`pidlink.dat` and be in the same directory as the
[configuration.md](configuration file). The BC table file has a simple
format. Each line has two fields:

* BC identifier

* File name, usually in quotes

### Example

This is the link BC file from a Lake Roosevelt MASS1 application.

```
 1 "BCFiles/Border-Discharge.dat"
 2 "BCFiles/Kettle-Discharge-daily.dat"
 3 "BCFiles/Colville-Discharge-daily.dat"
 4 "BCFiles/Spokane-Discharge.dat"
 5 "BCFiles/Sanpoil-Discharge.dat"
 6 "BCFiles/BanksCanal-flow.dat"
 7 "BCFiles/GCL-Elev.dat"
 8 "BCFiles/downstream-elev.dat"
 9 "BCFiles/GCL-Discharge.dat"

```

## General BC Files

A BC file contains a series of records containing a date/time and one
or more real values that correspond to that date time.  Each record
should terminate with a slash (`/`). The first line of the file is
discarded and so can be used for documentation of the file contents.
Anything on a line after a slash (`/`) is ignored, and so can be used
for additional documentation.  MASS1 interpolates between individual
date/times. Most BC files are expected to have only one real value per
record.

### Example ###

```
# Columbia River, McNary Dam: Total Flow (cfs)
12-31-1997 23:30:00       113500.000 /(WMD-OPS   )
01-01-1998 00:30:00       139500.000 /(WMD-OPS   )
01-01-1998 01:30:00       140800.000 /(WMD-OPS   )
01-01-1998 02:30:00       139900.000 /(WMD-OPS   )
01-01-1998 03:30:00       164500.000 /(WMD-OPS   )
01-01-1998 04:30:00       152000.000 /(WMD-OPS   )
01-01-1998 05:30:00       153300.000 /(WMD-OPS   )

```
## Hydro BC ##

Hydro BC files are used to specify hydrodynamic boundary conditions
for hydroelectric dam links [link.md]((types 6 and 21)).  

