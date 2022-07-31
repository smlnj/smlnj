(* os-prim-io.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an interface to a PrimIO structure augmented with OS specific
 * functions to create readers and writers.
 *
 *)

signature OS_PRIM_IO =
  sig
    structure PrimIO : PRIM_IO

    type file_desc

    val openRd  : string -> PrimIO.reader
    val openWr  : string -> PrimIO.writer
    val openApp : string -> PrimIO.writer

    val mkReader : {
	    fd : file_desc,
	    name : string,
  	    initBlkMode : bool
	  } -> PrimIO.reader
    val mkWriter: {
	    fd : file_desc,
	    name : string,
	    appendMode : bool,
	    initBlkMode : bool,
	    chunkSize : int
	  } -> PrimIO.writer

  end
