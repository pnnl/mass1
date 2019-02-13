# Link Input File

The link input file defines the links in the simulated network.  

## Format

Each link is described with two lines.  These lines should be ended
with a slash (`/`). The fields are described in the following
table. The field descriptions that are ~~struck out~~ are required but
are ignored and (re)computed internally.  A value still needs to be
specified (zero is fine) to maintain file format compatibility.  The
line can end prematurely if the field has a default value and there is
a slash at the end of the data. The field descriptions that are

|Line | Field | Type | Description | Default
|:----: | :----: | :----: | :---- | :----:
|1 | 1 | int | Link identifier (see below) |
|  | 2 | int | Point input option (see below) |
|  |   |     |   1 = data is supplied for each point |
|  |   |     |   2 = same data for all points |
|  | 3 | int | Number points on the link |
|  | 4 | int | ~~Link order~~ |
|  | 5 | int | Link type (see below) |
|  | 6 | int | ~~Number of upstream links~~ |
|  | 7 | int | Upstream hydrodynamic BC identifier |
|  | 8 | int | Downstream hydrodynamic BC identifier | 0
|  | 9 | int | TDG (species 2) BC identifier | 0
|  | 10 | int | Temperature (species 1) BC identifier | 0
|  | 11 | int | Meteorological zone identifier | 0
|  | 12 | int | Lateral inflow series identifier | 0
|  | 13 | int | Lateral inflow TDG series identifier | 0
|  | 14 | int | Lateral inflow temperature series identifier | 0
|  | 15 | real | LPI exponent  | 0.0
|2 | 1 | int | Downstream link identifier
|  | 2,... | int | ~~Upstream link identifier(s)~~
  
## Link Identifiers

Link identifiers are used internally as array indexes. Consequently,
they need to start with `1` and increase incrementally by `1`.  Link
definitions can be in any order, but 

## Link Type

| Type | Description 
| :---: | :--- 
| 1 | Normal fluvial link 
| 2 | Imposed upstream discharge
| 3 | Imposed  upstream stage
| 12 | Discharge following PID 
| 13 | Stage following PID 
| 20 | Normal fluvial link with upstream TDG specified as saturation rather than concentration
| 21 | Hydroelectric inflow link: like 1 but with generation and spill discharges specified in a [hydro BC](bc.md) file.  
| 4 | Imposed Downstream stage 
| 5 | Tributary inflow
| 6 | Imposed hydroelectric dam discharge
| 60 | Hydrologic routing link 

## Point Input Option

Link points can be specified in one of two ways: 

1. One record in the point file is read for each point on the link.
   Each point will potentially have its own cross section, thalweg,
   roughness, etc.  
   
2. One record in the point file is read for the link.  The specified
   number of points will be created and all points will have the same
   properties (except for thalweg).  

## Boundary Condition (BC) and Lateral Inflow Identifiers


## Example

This is from a MASS1 application to Lake Roosevelt with Canadian Dams
on the Columbia, Kootenay, and Pend d'Oreille Rivers.  It includes the
Spokane and Sanpoil River arms and Kettle and Colville River
tribuitaries.  

```
  13     1    43     1     1     0     1     1     1     1     1     0     0     0     5.0 /   Columbia          
  15                                                                                       /   
  14     1    18     2     1     0     9     0     9     9     1     0     0     0     0.0 /   Kootenay          
  15                                                                                       /   
  15     1   114     3     1     2     0     0     0     0     1     0     0     0     5.0 /   ms                
  17    13    14                                                                           /   
  16     1     5     4     1     0    10     0    10    10     1     0     0     0     0.0 /   Pend d'Oreille    
  17                                                                                       /   
  17     1     5     5     1     2     0     0     0     0     1     0     0     0     5.0 /   ms                
   1    15    16                                                                           /   
   1     1   188     6     1     1     0     0     0     0     1     0     0     0     5.0 /   ms                
   3    17                                                                                 /   
   2     1    29     7     1     0     2     0     2     2     1     0     0     0     1.0 /   Kettle            
   3                                                                                       /   
   3     1    30     8     1     2     0     0     0     0     1     0     0     0     5.0 /   ms                
   5     1     2                                                                           /   
   4     2     3     9     1     0     3     0     3     3     1     0     0     0     0.0 /   Colville          
   5                                                                                       /   
   5     1   257    10     1     2     0     0     0     0     1     0     0     0     5.0 /   ms                
   7     3     4                                                                           /   
   6     1   124    11     1     0     4     0     4     4     1     0     0     0     5.0 /   Spokane           
   7                                                                                       /   
   7     1    55    12     1     2     0     0     0     0     1     0     0     0     0.0 /   ms                
   9     5     6                                                                           /   
   8     1    32    13     1     0     5     0     5     5     1     0     0     0     5.0 /   Sanpoil           
   9                                                                                       /   
   9     1    42    14     1     2     0     0     0     0     1     0     0     0     0.0 /   ms                
  11     7     8                                                                           /   
  10     2     3    15     1     0     6     0     6     6     1     0     0     0     0.0 /   Banks Canal       
  11                                                                                       /   
  11     1     2    16     3     2     7     0     0     0     1     0     0     0     0.0 /   Grand Coulee Dam  
  12     9    10                                                                           /   
  12     2     3    17     1     1     0     8     0     0     1     0     0     0     0.0 /   ms                
   0    11                                                                                 /   
```

