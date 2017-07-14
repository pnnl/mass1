# Point Input File

The point file contains information for individual points along
links.  Point data for all links is included in a single file.  It's
best to end each line with a slash (`/`).  There are two options for
specifying the points for a link.

## Individual Point Format (Link Input Option=1)

Lines for the points of a single link should be grouped together in
the file, but do not need to be in order. 

Field | Type | Description | Typical Units
:---: | :---: | :--- | :---
1 | int | Link identifier |
2 | int | Point index |
3 | real | Point location | feet, mile
3 | int | Cross-section identifier
4 | real | Thalweg elevation | feet
5 | real | Manning's roughness coefficient |
6 | real | Longitudinal dispersion coefficient | ft<sup>2</sup>/s
7 | real | ~~Surface transfer coefficient~~ |

The point index is ignored and computed internally. *It is assumed the
points are ordered correctly and the correct number are in the
file*. Points should be listed from upstream to downstream.  Location units
will depend on the [configuration](configuration.md) units options.
The surface transfer coefficient is no longer used and
can be omitted if a `/` is appended to the line.   

## Uniform Point Format (Link Input Option=2)

Field | Type | Description | Typical Units
:---: | :---: | :--- | :---
1 | int | Link identifier |
2 | real | Length | feet, mile
3 | real | Upstream elevation | feet
4 | real | Downstream elevation | feet
5 | int | Cross-section identifier
5 | real | Manning's roughness coefficient |
6 | real | Longitudinal dispersion coefficient | ft<sup>2</sup>/s
7 | real | ~~Surface transfer coefficient~~ |

