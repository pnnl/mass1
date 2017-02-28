# -------------------------------------------------------------
# file: CMakeLists.txt
# -------------------------------------------------------------
# -------------------------------------------------------------
# Battelle Memorial Institute
# Pacific Northwest Laboratory
# -------------------------------------------------------------
# -------------------------------------------------------------
# Created January 19, 2017 by William A. Perkins
# Last Change: 2017-02-28 12:20:53 d3g096
# -------------------------------------------------------------

add_subdirectory(temperature/pond)

add_subdirectory(flow/drain)
add_subdirectory(flow/MacDonald-4)
add_subdirectory(flow/MacDonald-3)
add_subdirectory(flow/MacDonald-1)
add_subdirectory(flow/storage)
add_subdirectory(flow/slopebreak2)
add_subdirectory(flow/slopebreak1)
add_subdirectory(flow/lateral)
add_subdirectory(flow/normal)

