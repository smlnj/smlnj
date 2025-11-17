# CMake configuration for the SML/NJ Libraries
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# Note that the libraries that are used by the compiler are "installed"
# as part of the compiler's bootstrap process.
#

if (UNIX)
  add_subdirectory(Unix)
endif()

add_subdirectory(HashCons)
add_subdirectory(HTML)
add_subdirectory(HTML4)
add_subdirectory(INet)
add_subdirectory(JSON)
add_subdirectory(PP)
add_subdirectory(RegExp)
add_subdirectory(Reactive)
add_subdirectory(SExp)
add_subdirectory(UUID)
add_subdirectory(XML)
