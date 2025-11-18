# CheckSignalExists.cmake
#
# Copyright 2023 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# function to test if a Unix signal is defined (either as a C-preprocessor symbol
# or as an enum constant).
#

INCLUDE(CheckCSourceCompiles)

function(check_signal_exists signame varname)
  set(program_src)
  string(APPEND program_src "#include <signal.h>\n")
  string(APPEND program_src "
int main () {
#ifdef ${signame}
return 0;
#else
return (int)${signame};
#endif
}")
  # locally set CMAKE_REQUIRED_QUIET to avoid messages from check_c_source_compiles
  set(quiet ${CMAKE_REQUIRED_QUIET})
  set(CMAKE_REQUIRED_QUIET TRUE)
  if(NOT quiet)
    message(CHECK_START "Looking for ${signame}")
  endif()
  check_c_source_compiles("${program_src}" ${varname})
  if(${varname})
    if(NOT quiet)
      message(CHECK_PASS "found")
    endif()
    set(${varname} 1 CACHE INTERNAL "Have signal ${signame}")
  else()
    if(NOT quiet)
      message(CHECK_FAIL "not found")
    endif()
    set(${varname} "" CACHE INTERNAL "Have signal ${signame}")
  endif()
  # restore CMAKE_REQUIRED_QUIET
  set(CMAKE_REQUIRED_QUIET ${quiet})
endfunction()
