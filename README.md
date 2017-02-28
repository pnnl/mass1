# Modular Aquatic Simulation System in One Dimension (MASS1)

Home Page: http://mass1.pnnl.gov

## Description

MASS1 is a one-dimensional unsteady hydrodynamic and water quality
model capable of simulating open channel flows, water surface
elevations, dissolved gas, and water temperature. The model is
applicable to any branched channel system. Because MASS1 uses
cross-section averaging, only single values of water surface
elevation, velocity, discharge, and temperature are produced at each
cross-section location along the river course. Lateral (i.e., across
the river channel) and vertical variations of these quantities are not
simulated.  

MASS1 was developed by Pacific Northwest National Laboratory (PNNL).

## Obtaining the Code

The MASS1 source code is currently hosted on PNNL's internal Git
repository.  MASS1 shares some code with other applications, which is
included in MASS1 as a Git sub-module.  Do the following to check out
the code:

```
git clone https://stash.pnnl.gov/scm/~d3g096/mass1.git
cd mass1
git submodule update --init
```

## Requirements

### Fortran 2003 compiler

MASS1 is written in Fortran and makes use of several
[Fortran 2003](http://fortranwiki.org/fortran/show/Fortran+2003)
constructs.  Consistently, a Fortran compiler that conforms to the
2003 standard is required to build MASS1. The following compilers are
known to work:

* [GNU Fortran](https://gcc.gnu.org/fortran/), Version 4.8 or higher

* [Intel Fortran](https://software.intel.com/en-us/fortran-compilers),
  Version 14 or higher 

## Optional Software

### Gnuplot

[Gnuplot](http://www.gnuplot.info/) is used to plot MASS1 test
results.  If [Gnuplot](http://www.gnuplot.info/) is not found on the
system, tests can still be run, but the results will not be plotted.  

### Perl

Several optional [Perl](https://www.perl.org/) pre/post-processing
scripts are provided.  These use the following
[Perl](https://www.perl.org/) modules:

* `Date::Manip`

* `PDL`

* `PDL::Graphics::PGPLOT::Window`

When MASS1 code is configured, the system's
[Perl](https://www.perl.org/) installation is queried to see if the
scripts can be used. If not, the scripts are not configured or
installed.  

### Python

In addition to the Perl scripts, some optional
[Python](https://www.python.org/) pre/post-processing scripts are also
provided.  

### R

There is an optional [R](https://www.r-project.org/) script for
reading MASS1 time series output and boundary condition files. 

## Configure and Build

MASS1 uses [CMake](https://cmake.org/) cross-platform tools to
configure and build MASS1.  CMake queries  the system
and determine if the compiler has sufficient capability to compile
MASS1 and if optional software is available.  

A typical configuration:

```
mkdir build
cd build
FC=gfortran
export FC
cmake -D CMAKE_BUILD_TYPE:STRING=Release ..
```



