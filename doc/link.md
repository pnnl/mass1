# Link Input File

## Format

Each link is described with two lines.  These lines should be ended
with a slash (`/`). The fields are described in the following
table. The field descriptions that are ~~struck out~~ are required
but are ignored and (re)computed internally.  A value still needs to
be specified (zero is fine) to maintain file format compatibility.
The field descriptions that are 

Line | Field | Type | Description
:----: | :----: | :----: | :---- 
1 | 1 | int | Link identifier (see below)
  | 2 | int | Point input option (see below)
  |   |     |   1 = data is supplied for each point
  |   |     |   2 = same data for all points 
  | 3 | int | Number points on the link
  | 4 | int | ~~Link order~~
  | 5 | int | Link type (see below)
  | 6 | int | ~~Number of upstream links~~
  | 7 | int | Upstream hydrodynamic BC identifier 
  | 8 | int | Downstream hydrodynamic BC identifier
  | 9 | int | TDG (species 2) BC identifier
  | 10 | int | Temperature (species 1) BC identifier
  | 11 | int | Meteorological zone identifier
  | 12 | int | Lateral inflow series identifier
  | 13 | int | Lateral inflow TDG series identifier
  | 14 | int | Lateral inflow temperature series identifier
  | 15 | real | LPI exponent 
2 | 1 | int | Downstream link identifier
  | 2,... | ~~Upstream link identifier(s)
  
## Link Identifiers

Link identifiers are used internally as array indexes. Consequently,
they need to start with `1` and increase incrementally by `1`.  

## Link Type

Type | Description 
:---: | :--- 
1 | Normal fluvial link 
2 | Imposed upstream discharge
3 | Imposed  upstream stage
12 | Discharge following PID
13 | Stage following PID
4 | Imposed Downstream stage 
5 | Tributary inflow
6 | Imposed hydroelectric dam discharge

## Point Input Option

Link points can be specified in one of two ways: 

1. One record in the point file is read for each point on the link.
   Each point will potentially have its own cross section, thalweg,
   roughness, etc.  
   
2. One record in the point file is read for the link.  The specified
   number of points will be created and all points will have the same
   properties (except for thalweg).  

## Boundary Condition (BC) and Lateral Inflow Identifiers


