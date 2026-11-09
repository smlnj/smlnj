# CMake definition of the SML/NJ version
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# This module defines the following cache variables:
#
#       SMLNJ_VERSION_MAJOR
#       SMLNJ_VERSION_MINOR
#       SMLNJ_VERSION_PATCH
#       SMLNJ_VERSION_SUFFIX
#       SMLNJ_VERSION
#

# QUESTION:
#
# an alternative approach is to read the `config/version` file and
# parsing its contents
# file(READ "${CMAKE_CURRENT_LIST_DIR}/config/version" config_version)
# string(STRIP "${config_version}" PACKAGE_VERSION)

if(NOT DEFINED SMLNJ_VERSION_MAJOR)
  set(SMLNJ_VERSION_MAJOR 2026)
endif()
if(NOT DEFINED SMLNJ_VERSION_MINOR)
  set(SMLNJ_VERSION_MINOR 3)
endif()
if(NOT DEFINED SMLNJ_VERSION_PATCH)
  set(SMLNJ_VERSION_PATCH 0)
endif()
if(NOT DEFINED SMLNJ_VERSION_SUFFIX)
  set(SMLNJ_VERSION_SUFFIX "")
endif()
string(JOIN "." SMLNJ_VERSION
  "${SMLNJ_VERSION_MAJOR}" "${SMLNJ_VERSION_MINOR}" "${SMLNJ_VERSION_PATCH}")

if (NOT PACKAGE_VERSION)
  string(CONCAT PACKAGE_VERSION
    "${SMLNJ_VERSION}" "${SMLNJ_VERSION_SUFFIX}")
endif()

# cache the version in `config/version`
file(WRITE "${CMAKE_CURRENT_LIST_DIR}/config/version" "${PACKAGE_VERSION}")
