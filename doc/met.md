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

The coefficients are only optional for backward compatibility. It's
best to include the coefficients for documentation. 

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


### Example ###

```
01-01-1998 00:00:00        0.28     0.28      0.55    760.0      2.79 /
01-01-1998 01:00:00        0.21     0.21      0.82    760.0      5.58 /
01-01-1998 02:00:00        0.23     0.23      0.82    760.0      2.79 /
01-01-1998 03:00:00       -0.23    -0.23      0.55    760.0      5.58 /
01-01-1998 04:00:00        0.08     0.08      0.55    760.0      4.18 /
01-01-1998 05:00:00        0.56     0.56      0.82    760.0      2.79 /

```
