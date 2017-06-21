# Boundary Condition (BC) Input

## BC Table Files

MASS1 boundary condition file are organized into the following
categories:

* Link BC

* Hydro BC

* Later Inflow

* Temperature

* TDG

* PID

A BC table file is used for each of these.  The BC table file has a
simple format. Each line has two fields:

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

Most 

MASS1 interpolates between individual date/times. 

### Example

```

```

## Hydro BC 

Hydro BC files are used for 

