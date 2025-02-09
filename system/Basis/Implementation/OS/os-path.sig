(* os-path.sig
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *
 * The generic interface to syntactic pathname manipulation.
 *)

signature OS_PATH =
  sig

    exception Path
    exception InvalidArc

    val parentArc  : string
    val currentArc : string

    val fromString : string -> {isAbs : bool, vol : string, arcs : string list}
    val toString   : {isAbs : bool, vol : string, arcs : string list} -> string

    val validVolume : {isAbs : bool, vol : string} -> bool

    val getVolume   : string -> string
    val getParent   : string -> string

    val splitDirFile : string -> {dir : string, file : string}
    val joinDirFile  : {dir : string, file : string} -> string
    val dir          : string -> string
    val file         : string -> string

    val splitBaseExt : string -> {base : string, ext : string option}
    val joinBaseExt  : {base : string, ext : string option} -> string
    val base         : string -> string
    val ext          : string -> string option

    val mkCanonical : string -> string
    val isCanonical : string -> bool

    val mkAbsolute  : {path : string, relativeTo : string} -> string
    val mkRelative  : {path : string, relativeTo : string} -> string
    val isAbsolute  : string -> bool
    val isRelative  : string -> bool

    val isRoot      : string -> bool

    val concat      : (string * string) -> string

    val fromUnixPath : string -> string
    val toUnixPath   : string -> string

  end; (* OS_PATH *)

