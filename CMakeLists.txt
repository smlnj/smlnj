# CMake configuration for SML/NJ
#
# COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

# we require CMake 3.24 so that we can use "include" in the CMakePresets.cmake
# file (requires preset version 4)
#
cmake_minimum_required(VERSION 3.24.0)

# The SML/NJ Version
#
if(NOT DEFINED SMLNJ_VERSION_MAJOR)
  set(SMLNJ_VERSION_MAJOR 2023)
endif()
if(NOT DEFINED SMLNJ_VERSION_MINOR)
  set(SMLNJ_VERSION_MINOR 1)
endif()
if(NOT DEFINED SMLNJ_VERSION_PATCH)
  set(SMLNJ_VERSION_PATCH 0)
endif()
if(NOT DEFINED SMLNJ_VERSION_SUFFIX)
  set(SMLNJ_VERSION_SUFFIX "")
endif()

if (NOT PACKAGE_VERSION)
  set(PACKAGE_VERSION
    "${SMLNJ_VERSION_MAJOR}.${SMLNJ_VERSION_MINOR}.${SMLNJ_VERSION_PATCH}${SMLNJ_VERSION_SUFFIX}")
endif()

project(smlnj
  VERSION ${SMLNJ_VERSION_MAJOR}.${SMLNJ_VERSION_MINOR}.${SMLNJ_VERSION_PATCH}
  DESCRIPTION "Standard ML of New Jersey"
  HOMEPAGE_URL "https://smlnj.org"
  LANGUAGES C CXX ASM)  # note that we need C for feature tests

# Disable builds in the source tree to prevent corruption.
#
if ("${CMAKE_SOURCE_DIR}" STREQUAL " ${CMAKE_BINARY_DIR}")
  message(FATAL_ERROR "
ERROR: builds in the source tree are not allowed.
       Create a separate directory for build files.
")
endif()

# check the build type
#
if (NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, default to Release")
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Build type (default Release)" FORCE)
endif()

# set the search path for CMake files
#
list(APPEND CMAKE_MODULE_PATH
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules")

include(smlnj-targets)
include(smlnj-actions)
