# CMake configuration for the SML/NJ runtime system
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

# TODO: feature detection (replace include/ml-unixdep.h, etc.)

# set compiler flags based on target, etc.

add_subdirectory(c-libs)
add_subdirectory(gc)
add_subdirectory(kernel)
add_subdirectory(memory)
