# Meteorologic Data

If atmospheric exchange is enable temperature or TDG in the
[configuration.md](configuration file), MASS1 will open and read the
meteorologic zone table specified in record 32. Each link is assigned
to a met zone, as specified by the identifier in the [link.md](link
file).

## Meteorologic Zone Table

The meteorologic zone table contains one record for each meteorologic
zone. Each record contains from 2 to 6 fields and should terminate
with a slash (`/`). At a minimum, specifies the meteorologic
data file used for each zone.  It can also optionally specify several
coefficients that affect the atmospheric energy exchange.  The
meteorologic zone table contains the following fields:

Field | Type | Description | Default
:---: | :---: | :--- | :--- 
1 | int | Met zone identifier | (required)
2 | string | Met data file | (required)
3 | real | Wind coefficient multiplier | 0.46
4 | real | Wind coefficient offset | 9.2
5 | real | Conduction coefficient | 0.47
6 | real | Brunt coefficient | 0.65
7 | flag | Incoming Longwave Radiation included | 0

The coefficients are only optional for backward compatibility. It's
best to include the coefficients for documentation. 

If the longwave radiation flag is not set, MASS1 computes it
internally, as it did historically.

### Example ###

```
1 "../BCFiles/Weather/HOXO-Weather.dat"   0.1  6.9 0.2 0.85 / 
2 "../BCFiles/Weather/HOXO-Weather.dat"   0.1  6.9 0.2 0.85 /
3 "../BCFiles/Weather/GOLW-Weather.dat"   0.1  6.9 0.2 0.85 /
4 "../BCFiles/Weather/HERO-Weather.dat"   0.1  6.9 0.2 0.85 /
5 "../BCFiles/Weather/LEGW-Weather.dat"   0.1  6.9 0.2 0.85 /
6 "../BCFiles/Weather/LEGW-Weather.dat"   0.1  13.7 0.2 0.85 /
7 "../BCFiles/Weather/LBRW-Weather.dat"   0.1  13.7 0.2 0.85 /
8 "../BCFiles/Weather/SILW-Weather.dat"   0.1  12.2 0.2 0.85 /
9 "../BCFiles/Weather/DENI-Weather.dat"   0.1  12.2 0.2 0.85 /

```

## Meteorologic Data File


The meteorologic data file has the format of [bc.md](boundary
condition files) with 5 meteorologic data fields per record.  Each
record should terminate with a slash (`/`).  MASS1 linearly
interpolates between individual date/times.  Each record contains the
following fields:

Field | Type | Description | Units
:---: | :---: | :--- | :--- 
1 | date | Date/time of met observation | 
2 | real | Air temperature | C
3 | real | Dew point temperature | C
4 | real | Wind speed | m/s
5 | real | Atmospheric pressure | mbar
6 | real | Net incoming shortwave radiation | W/m<sup>2</sup>
7 | real | Net incoming longwave radiation | W/m<sup>2</sup>

Atmospheric pressure is used only when total dissolved gas is
simulated.  Longwave radiation is allowed if specified in the
meteorological zone table. It's inclusion is an error otherwise.  

### Example ###

An example of a meteorology time series *without* longwave radiation:

```
06-01-1990 06:00:00    8.9    2.8    4.0  739.1   20.9 /
06-01-1990 07:00:00   11.1    2.2    5.4  739.1  104.6 /
06-01-1990 08:00:00   12.2    2.2    7.2  739.1  209.2 /
06-01-1990 09:00:00   15.0    2.2    8.9  739.1  509.1 /
06-01-1990 10:00:00   16.7    2.2    9.8  738.8  676.4 /
06-01-1990 11:00:00   18.3    2.8    8.0  738.8  788.0 /
```
An example of the same meteorology time series, but *with* longwave radiation:

```
06-01-1990 06:00:00    8.9    2.8    4.0  739.1   20.9  243.2 /
06-01-1990 07:00:00   11.1    2.2    5.4  739.1  104.6  250.5 /
06-01-1990 08:00:00   12.2    2.2    7.2  739.1  209.2  254.4 /
06-01-1990 09:00:00   15.0    2.2    8.9  739.1  509.1  264.5 /
06-01-1990 10:00:00   16.7    2.2    9.8  738.8  676.4  270.8 /
06-01-1990 11:00:00   18.3    2.8    8.0  738.8  788.0  277.3 /

```
