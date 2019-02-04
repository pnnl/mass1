# Time Series Fortran 90 Modules

Copyright (c) 2017 Battelle Memorial Institute
Licensed under modified BSD License. A copy of this license can be
found in the [LICENSE](LICENSE) file in the top level directory of this
distribution. See also the [disclaimer](DISCLAIMER). 

## Description

This provides several Fortran for consistent handling of time series
data.  These are used by PNNL's [MASS1](https://store.pnnl.gov/content/modular-aquatic-simulation-system-1d-mass1)
and MASS2 codes.   

The following modules are provided:

* `utility`: some utility programs
* `julian`: implements algorithm 199 of the Collected Algorithms of
  CACM to represent date/time in the Julian calendar. 
* `date_time`: conversion between `julian` dates and string
  representations of the form `mm-dd-YYYY HH:MM:SS`.
* `time_series`: a general representation of a time series table with
  a date/time as the ordinate and multiple range columns; includes
  facilities for interpolation
* `const_series`: a `time_series` wrapper used to create a "constant"
  time series (i.e. one point)
* `met_series`: a special `time_series` storing meterologic data
  required by [MASS1](https://store.pnnl.gov/content/modular-aquatic-simulation-system-1d-mass1) and MASS2. 

## Configure and Build

This code uses the
[GNU Build System](https://en.wikipedia.org/wiki/GNU_Build_System) to
configure and build.  The `configure` script is used to query the
system and determine if the compiler has sufficient capability to
compile the code.  On most systems, simply executing the `configure`
script is sufficient, but sometimes options are required. To get a
list of options,

```
./configure --help
```

A typical way to require the GNU Fortran compiler and trapping of
floating point errors:

```
./configure FC=gfortran FCFLAGS="-O2 -ffpe-trap=invalid,zero,overflow"
```

Similarly, for the Intel Fortran compiler:

```
./configure FC=ifort FCFLAGS="-O3 -fpe0"
```

After successful configuration, build with 

```
make
```
and build the test programs with

```
make check
```
