# MASS1 Configuration File

## Format

Most lines in the configuration file contain one value. It's best to
put a slash (`/`) after the required data on each line.  Comments can
appear after the `/`, as shown in the example below.  Those items with
their description ~~struck out~~ are not available or ignored, either
because they are no longer needed or computed internally.  Items

| Line | Field | Type | Description 
| :---: | :---: | :---: | :---
| 1 | 1 | string | Run title -- for user documentation only
| 2 | 1 | flag | Enable hydrodynamics simulation
| 3 | 1 | flag | Enable lateral inflow
| 4 | 1 | flag | Enable TDG simulation
| 5 | 1 | flag | Enable temperature simulation
| 6 | 1 | flag | Enable detailed results print out
| 7 | 1 | flag | Enable gage results output
| 8 | 1 | flag | Enable profile results output
|   | 2 | flag | Average gage and profile output over output time
| 9 | 1 | flag | Enable TDG dispersion 
|10 | 1 | flag | Enable atmospheric TDG exchange (requires [met data](met.md))
|11 | 1 | flag | Enable temperature dispersion 
|12 | 1 | flag | Enable atmospheric energy exchange (requires [met data](met.md))
|13 | 1 | flag | If true, read initial conditions from a hotstart file
|   |   |      | otherwise, read [initial conditions file](init.md)
|14 | 1 | flag | Write a hotstart file at the end of simulation
|15 | 1 | int  | Units option: 1 = English, ~~2 = metric~~[^2]
|16 | 1 | int  | Time option: ~~0 = real~~[^1], 1 = date/time
|17 | 1 | int  | Units used for channel length in the [point file](point.md):
|   |   |      |  0 = feet, 1 = meters, 2 = miles, 3 = kilometers
|18 | 1 | int  | Boundary condition at outlet: 
|   |   |      |  0 = stage, 1 = discharge
|19 | 1 | int  | Maximum number of links (>= actual number of links)
|20 | 1 | int  | Maximum number of points on a link
|21 | 1 | int  | ~~Maximum number of values in a [BC file](bc.md)~~
|22 | 1 | int  | Number of transport time sub-steps (0 = automatic)
|23 | 1 | flag | Enable debugging output
|24 | 1 | string | [Link file](link.md) name
|25 | 1 | string | [Point file](point.md) name
|26 | 1 | string | [Section file](section.md) name
|27 | 1 | string | [Link BC file](bc.md) name
|28 | 1 | string | [Initial conditions file](initial.md) name
|29 | 1 | string | [Output file](output.md) name
|30 | 1 | string | [TDG BC file](bc.md) name
|31 | 1 | string | [Temperature BC file](bc.md) name
|32 | 1 | string | [Meteorologic zone table file](met.md) name
|33 | 1 | string | [Hydropower BC file](bc.md) name
|34 | 1 | string | [TDG Coefficients file](tdg_coeff.md) name
|35 | 1 | string | Restart file from which initial conditions are read
|36 | 1 | string | Hotstart file to write
|37 | 1 | string | [Gage control file](gage.md) name
|38 | 1 | string | [Profile control file](profile.md) name
|39 | 1 | string | [Lateral inflow BC file](bc.md) name
|40 | 1 | string | Simulation start date (mm-dd-yyyy)
|41 | 1 | string | Simulation start time (HH:MM:SS)
|42 | 1 | string | Simulation end date (mm-dd-yyyy)
|43 | 1 | string | Simulation end time (HH:MM:SS)
|44 | 1 | real   | Hydrodynamics time step
|   | 2 | string | Optional time step units, one of: `hr`, `min`, `day`, and `sec`. `hr` is default.
|45 | 1 | int    | Gage and profile output frequency (time steps)

## Example

```
MASS1 Configuration File - Version 0.83
1	/	Do Flow
0	/	Do lateral inflow
0	/	Do Gas
0	/	Do Temp
0	/	Do Printout
1	/	Do Gage Printout
1	/	Do Profile Printout
0	/	Do Gas Dispersion
0	/	Do Gas Air/Water Exchange
1	/	Do Temp Dispersion
1	/	Do Temp surface exchange
1	/	Do Hotstart read file
0	/	Do Restart write file
0	/	Do Print section geometry
0	/	Do write binary section geom
0	/	Do read binary section geom
0	/	units option
1	/	time option
2	/	time units
2	/	channel length untis
0	/	downstream bc type
5	/	max links
400	/	max points on a link
28	/	max bc table
60000	/	max times in a bc table
1379	/	total number of x-sections
15  /   number of transport sub time steps
0 	/	debug print flag
"../BaseFiles/prd2mcn.link"	/	link file name
"../BaseFiles/prd2mcn.point"	/	point file name nonuniform manning n
"../BaseFiles/CAN2MCN.dat" 	/	section file name
"link-bc-files.dat"	/	linkBC file name
"initial.dat" 	/	initial file name
"output.out" 	/	output file name
"./transbc-files.dat"	/	gas transport file name
"temp-bc-files.dat"	/	temperature input
"weather-files.dat"	/	weather data files for each met_zone input
"./hydrobc-init.dat" /	hydropower file name
"../Common_Files/tdg_coeff.dat" 	/	TDG Coeff file name
"../Rampdown/restart.dat" 	/	hotstart-warmup-unix.dat /	read restart file name
"hotstart.dat" /	Write restart file name
"../BaseFiles/gage-control.dat"			/	gage control file name
"../BaseFiles/profile-control.dat" 	 	/	profile file name
"../latflow-files.dat"     		/	lateral inflow bs file name
02-01-2000	/	date run begins
00:00:00	/	time run begins
01-10-2001	/	date run ends
00:00:00	/	time run ends
0.5	/	delta t in hours (0.5 for flow only; 0.02 for transport)
336	/	printout frequency
```

[^1]: Using decimal time in time series input no longer works, so
don't try it. 

[^2]: The metric units option is untested and probably doesn't work.

[^3]: Units are hours unless units are specified.  The real value can
be followed by a units label. The following are understood: `hr`,
`min`, `day`, and `sec`.  

