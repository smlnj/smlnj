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
include(smlnj-version)

project(smlnj
  VERSION ${SMLNJ_VERSION}
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


# the build process has the following steps:
#
#    1) download the boot files (if necessary)
#    2) build the LLVM libraries
#    3) build the runtime system
#    4) bootstrap the basic system
#    5) move stable libraries to lib directory
#    6) generate pathconfig file
#    7) ml-build?
#    8) execute the "actions"
#    9) optionally generate the docs

# add the source subdirectories
#
add_subdirectory(runtime)
add_subdirectory(cm)
add_subdirectory(smlnj-lib)
add_subdirectory(system)
add_subdirectory(libraries)
add_subdirectory(tools)

# should these be conditional?
add_subdirectory(compiler)
add_subdirectory(package)
add_subdirectory(doc)

