# CheckStructStatWithTimeSpec.cmake
#
# Copyright 2023 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# recent systems use `struct timespec` as the type of the
# file creation/modification/access times in the `stat` struct.
# This module tests to see if that is the case
#

INCLUDE(CheckCSourceCompiles)

function(check_stat_with_timespec varname)
  check_c_source_compiles("
#include <sys/stat.h>
#include <time.h>
int main ()
{
  struct timespec *ts;
  struct stat sbuf;
  ts = &sbuf.st_atime;
  return 0;
}" ${varname})
endfunction()
