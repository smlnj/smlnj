# CheckTypeExists.cmake
#
# Copyright 2023 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# test for whether a type exists or not
#
#	check_type_exists(<type> <files> <variable>)
#

INCLUDE(CheckCSourceCompiles)

function(check_type_exists tyname files varname)
  set(program_src)
  foreach(file ${files})
    string(APPEND program_src "#include <${file}>\n")
  endforeach()
  string(APPEND program_src "int main () { ${tyname} x; return 0; }")
  check_c_source_compiles("${program_src}" ${varname})
endfunction()
