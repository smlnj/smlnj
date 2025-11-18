# CMake configuration for the NLFFI tool and libraries
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

# TODO: NLFFI configuration

if (SMLNJ_LIB_ML_NLFFI)
  ml-nlffi-lib lib c memory/memory.cm   tools/nlffi/lib
  ml-nlffi-lib lib c internals/c-int.cm tools/nlffi/lib
  ml-nlffi-lib lib c c.cm               tools/nlffi/lib
endif()

if (SMLNJ_TOOL_ML_NLFFIGEN)
  ml-nlffigen dprog ml-nlffigen - tools/nlffi/gen
  # dependencies (from config/dependencies)
  ml-nlffigen ckit smlnj-lib
endif()
