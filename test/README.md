# MASS1 Test Simulations

This directory contains several rather simple simulations
that are used to verify MASS1 operation.  

## Hydrodynamic Tests

The following hydrodynamic tests are available.  They can be run with
```runem.sh```. 

* ```flow/dam-break```: Simulation of dam break flow for comparison to
  theory (Chao et al., 1999).
* ```flow/drain```: Drain an initially submerged channel dry.
* ```flow/fill```: Fill an initially dry channel by raising the
  downstream stage
* ```flow/fill-us```: Fill an initially dry channel with upstream discharge.
* ```flow/lateral```: Simple test of lateral inflow. 
* ```flow/lateral-fill```: Fill and drain an initially dry channel
  with lateral inflow.  
* ```flow/MacDonald-1```: Comparison to the case 1 analytic solution
  from MacDonald, et al. (1997).
* ```flow/MacDonald-3```: Comparison to the case 3 analytic solution
  from MacDonald, et al. (1997).
* ```flow/MacDonald-4```:Comparison to the case 4 analytic solution
  from MacDonald, et al. (1997).
* ```flow/normal```: Subcritical, uniform flow in a rectangular channel.
* ```flow/slopebreak1```: Gradually varied subcritical flow in a
  rectangular channel 
* ```flow/slopebreak2```: Another gradually varied subcritical flow in
  a rectangular channel
* ```flow/storage```: several simple mass balance tests
* ```flow/varied```: Some tests exploring the effects of a clamped
  downstream stage boundary conditon on a discharge wave.
* ```flow/varied-pid```:  
* ```flow/varied-pool```: Some tests exploring the effects on a
  discharge wave various time steps with a clamped downstream stage
  boundary conditon .

## Transport Tests

## Temperature Tests

## References

MacDonald, I., Baines, M.J., Nichols, N.K., Samuels,
P.G., 1997. Analytic Benchmark Solutions for Open-Channel
Flows. Journal of Hydraulic Engineering 123, 1041–1045. 

Chao Wu, Guofu Huang, Yonghong Zheng, 1999. Theoretical Solution of
Dam-Break Shock Wave. doi:10.1061/(ASCE)0733-9429(1999)125:11(1210) 
