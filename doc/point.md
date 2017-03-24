# Point Input File

The point file contains information for individual points along
links.  Point data for all links is included in a single file.  It's
best to end each line with a slash (`/`).  

## Individual Point Format (Link Input Option=1)

Lines for the points of a single link should be grouped together in
the file, but do not need to be in order. 

Field | Type | Description | Typical Units
:---: | :---: | :--- | :---
1 | int | Link identifier |
2 | int | Point index |
3 | real | Point location | feet, mile
3 | int | Section identifier
4 | real | Thalweg elevation | feet
5 | real | Manning's roughness coefficient |
6 | real | Longitudinal dispersion coefficient | ft<sup>2</sup>/s
7 | real | ~~Surface transfer coefficient~~ |

The point index is as an array index internally, so it needs to start
with 1 and increment by 1. Units will depend on the
[configuration](configuration.md) units options.  The surface transfer
coefficient is no longer used and can be omitted if a `/` is appended to the line.  

## Uniform Point Format (Link Input Option=2)

Field | Type | Description | Typical Units
:---: | :---: | :--- | :---
1 | int | Link identifier |
2 | real | Length | feet, mile
3 | real | Upstream elevation | feet
4 | real | Downstream elevation | feet
5 | int | Section identifier
5 | real | Manning's roughness coefficient |
6 | real | Longitudinal dispersion coefficient | ft<sup>2</sup>/s
7 | real | ~~Surface transfer coefficient~~ |

