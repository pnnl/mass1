# PID Coefficients File

If any PID type links (12, 13) are specified in the
[link file](link.mp), MASS1 expects a file named `pidlink.dat` to be
in the simulation directory (same directory as the
[configuration file](configuration.md).  This file is used to specify
the coefficients for all PID-type links.  

| Field | Type | Description
|     1 | int  | PID link identifier
|     2 | real | Proportional coefficient, Kc
|     3 | real | Integral coefficient, Ti
|     4 | real | Derivative coefficient, TR
|     5 | int  | Reference BC (< 0) or link (> 0) identifier
|     6 | real | Lag for reference signal in field 5, days
|       |      | (repeat reference signal identifier / lag pairs as necessary)

## Reference Signal Identifiers

A PID link expects a 


## Example

```
 8  1.0e+05     9.0e-00      3.0e-02      1   2.6e-01     2   1.0e-01      4   7.3e-02     6   2.1e-02  /
10  6.0e+03     6.0e-00      1.0e-02      9   0.6e-01 /
16  2.0e+03     5.0e-01      8.0e-03     11   3.8e-02    12   1.7e-02 /   13   9.0e-03 /
22  2.0e+03     5.0e-01      3.0e-02     17   3.8e-02    18   3.3e-02     20   4.2e-03 / 
26  2.0e+03     5.0e-01      3.0e-02     23   2.8e-02    24   1.4e-02 / 
28  1.0e+03     6.0e-01      3.0e-02     27   2.5e-02 / 
32  1.0e+03     4.0e-01      8.0e-02     29   4.2e-02    30   1.8e-02 / 
```
