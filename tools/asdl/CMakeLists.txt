# CMake configuration for ASDL
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

# TODO: ASDL configuration

if (SMLNJ_LIB_ASDL)
  asdl    lib     asdl-lib.cm     asdl-lib.cm     tools/asdl/src/lib/sml
endif()

if (SMLNJ_PROG_ASDLGEN)
  asdl    config  tools/asdl
  asdl    dprog   asdlgen         src/asdlgen     tools/asdl
  asdl    lib     asdlgen-tool.cm asdlgen-tool.cm tools/asdl/tool
  asdl    lib     asdl-ext.cm     asdl-ext.cm     tools/asdl/tool
  # dependencies (from config/dependencies)
  asdl smlnj-lib ml-lpt-lib
endif()
