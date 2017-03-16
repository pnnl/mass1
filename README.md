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

MASS1 has been used primarily on UNIX, Linux, and Mac OS X
systems. The following describes how to build MASS1 on those
platforms.  It has been built and used on Windows systems, but rarely.
The following should work on Windows if [Cygwin](https://cygwin.com/)
or [MinGW](http://www.mingw.org/) is utilized.  A native Windows build
may be available in the future. 

### Fortran 90/95 compiler

MASS1 is a relatively portable, vanilla Fortran 90/95 code.  It has
been built and used on UNIX, Linux, Mac OS X, and Windows.  A Fortran
90/95 compiler is required.  MASS1 is currently developed and tested
on Mac OS X and Linux using the following compilers:

* [GNU Fortran](https://gcc.gnu.org/fortran/), Version 4.1 or higher

* [Intel Fortran](https://software.intel.com/en-us/fortran-compilers),
  Version 10 or higher

In general, the Intel compiler produces a faster executable, but for
most applications, this is not noticable. In the past, Fortran
compilers from [NAG](https://www.nag.com/nag-compiler),
[Absoft](https://www.absoft.com/),
[IBM](http://www-03.ibm.com/software/products/en/xlfortran-linux), and
others have worked.  

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

MASS1 uses the [GNU Build System](https://en.wikipedia.org/wiki/GNU_Build_System) 
to build MASS1.  The `configure` script is used to query the system
and determine if the compiler has sufficient capability to compile
MASS1.  On most systems, simply executing the `configure` script is
sufficient, but sometimes options are required. To get a list of
options, 

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



