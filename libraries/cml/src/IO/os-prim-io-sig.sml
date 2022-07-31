(* os-prim-io-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an interface to a PrimIO structure augmented with OS specific
 * functions to create readers and writers.
 *
 * This file was copied from the SML/NJ sources.
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
	    name : string
	  } -> PrimIO.reader

    val mkWriter: {
	    fd : file_desc,
	    name : string,
	    appendMode : bool,
	    chunkSize : int
	  } -> PrimIO.writer

  end

