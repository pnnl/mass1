# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2019-05-30 07:44:22 d3g096
# -------------------------------------------------------------


add_subdirectory(flow/hlink-trib)
add_subdirectory(flow/hlink-upstream)
add_subdirectory(flow/hlink-lateral)
add_subdirectory(flow/hlink-slopebreak1)
add_subdirectory(flow/hlink-normal)
add_subdirectory(flow/dam-break)
add_subdirectory(flow/drain)
add_subdirectory(flow/fill)
add_subdirectory(flow/fill-us)
add_subdirectory(flow/MacDonald-4)
add_subdirectory(flow/MacDonald-3)
add_subdirectory(flow/MacDonald-1)
add_subdirectory(flow/storage)
add_subdirectory(flow/slopebreak3-metric)
add_subdirectory(flow/slopebreak2-metric)
add_subdirectory(flow/slopebreak1-metric)
add_subdirectory(flow/slopebreak3)
add_subdirectory(flow/slopebreak2)
add_subdirectory(flow/slopebreak1)
add_subdirectory(flow/lateral)
add_subdirectory(flow/normal-hydro)
add_subdirectory(flow/normal-metric)
add_subdirectory(flow/normal)

add_subdirectory(transport/advection-1-segment)
add_subdirectory(transport/lateral-inflow)
add_subdirectory(transport/pulse)
add_subdirectory(transport/hlink-lateral-inflow)
#add_subdirectory(transport/hlink-advection-1-segment)
#add_subdirectory(transport/hlink-advection-diffusion-1-segment)
#add_subdirectory(transport/hlink-pulse)
add_subdirectory(temperature/pond)
