# Initial Conditions File

The initial conditions file is used when a hotstart file is not
available.  This is typically used for the first, steady-state
simulation in an application.  

The file consist of one record per link.  Each record should be ended
with a slash (`/`).  A record for each link is necessary, but that is
not checked. If conditions are not specified for a link, they will be
undefined. There is no default. There is basically no error checking.  

Each link record has the fields described in the table below.  The
conditions are assigned to each point along the entire length of the
link. 

| Field | Type | Description 
|:----: | :----: | :----
| 1 | int  | Link identifier 
| 2 | real | Discharge, cfs
| 3 | real | Water surface elevation, ft
| 4 | real | TDG concentration, mg/l
| 5 | real | Temperature, C


