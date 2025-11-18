# GetArchAndOpsys.cmake
#
# Copyright 2021 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#
# This module replicates the behavior of the `.arch-n-opsys` script
#
#	smlnj_arch_and_opsys ( arch_var opsys_var )
#

function( get_arch_and_opsys arch_var opsys_var )
  if ( MSVC )
    if ( CMAKE_SIZEOF_VOID_P EQUAL 8 )
      set ( opsys "win64" )
    else()
      set ( opsys "win32" )
    endif()
    # on Windows CMAKE_HOST_PROCESSOR should be "AMD64", "ARM64", or "X86"
    string ( TOLOWER ${CMAKE_HOST_PROCESSOR} arch )
  else()
    if ( CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin" )
      set ( opsys "darwin" )
      if ( CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" )
	set ( arch "amd64" )
      elseif ( CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64" )
	set ( arch "arm64" )
      else()
	message(FATAL_ERROR "unable to determine architecture: ${CMAKE_HOST_SYSTEM_PROCESSOR}")
      endif()
    elseif ( CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux" )
      set ( opsys "linux" )
      if ( CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" )
	set ( arch "amd64" )
      elseif ( CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64" )
	set ( arch "arm64" )
      elseif ( CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64" )
	set ( arch "arm64" )
      else()
	message(FATAL_ERROR "unable to determine architecture: ${CMAKE_HOST_SYSTEM_PROCESSOR}")
      endif()
# TODO: Cygwin, FreeBSD, NetBSD, OpenBSD, Solaris?
    else()
      message(FATAL_ERROR "unable to determine host operating system: ${CMAKE_HOST_SYSTEM_NAME}")
    endif()
  endif()
  set( ${arch_var} ${arch} PARENT_SCOPE )
  set( ${opsys_var} ${opsys} PARENT_SCOPE )
endfunction()
