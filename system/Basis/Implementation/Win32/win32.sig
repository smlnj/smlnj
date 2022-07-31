(* win32.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Signature for the interface to Win32.
 *)

signature WIN32 =
  sig
    structure General : WIN32_GENERAL
    structure FileSys : WIN32_FILESYS
    structure IO      : WIN32_IO
    structure Process : WIN32_PROCESS
  end
