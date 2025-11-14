# CMake configuration for SML/NJ Libraries
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

if (SMLNJ_LIB_CKIT)
  add_subdirectory(ckit)
endif()

if (SMLNJ_ENABLE_CML)
  add_subdirectory(cml)
endif()

if (SMLNJ_LIB_MLRISC)
  add_subdirectory(MLRISC)
endif()

if (SMLNJ_LIB_OLD_BASIS)
  add_subdirectory(old-basis)
endif()

if (SMLNJ_LIB_PGRAPH)
  add_subdirectory(pgraph)
endif()

if (SMLNJ_LIB_TDP)
  add_subdirectory(trace-debug-profile)
endif()
