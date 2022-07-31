(* pp-debug-fn.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A wrapper for the PPStreamFn, which dumps the current PP state prior
 * to each operation.
 *)

functor PPDebugFn (PP : sig
    include PP_STREAM
    val dump : (TextIO.outstream * stream) -> unit
  end) : sig
    include PP_STREAM
    val debugStrm : TextIO.outstream ref
  end = struct

    type device = PP.device
    type stream = PP.stream
    type token = PP.token
    type style = PP.style
    datatype indent = datatype PP.indent

    val debugStrm = ref TextIO.stdErr

    fun debug name f strm arg = (
	  TextIO.output(!debugStrm, concat["*** ", name, ": "]);
	  PP.dump (!debugStrm, strm);
	  TextIO.flushOut(!debugStrm);
	  f strm arg)
    fun debug' name f strm = (
	  TextIO.output(!debugStrm, concat["*** ", name, ": "]);
	  PP.dump (!debugStrm, strm);
	  TextIO.flushOut(!debugStrm);
	  f strm)

    val openStream = PP.openStream
    val flushStream = debug' "flushStream" PP.flushStream
    val closeStream = debug' "closeStream" PP.closeStream
    val getDevice = PP.getDevice

    val openHBox   = debug' "openHBox" PP.openHBox
    val openVBox   = debug "openVBox" PP.openVBox
    val openHVBox  = debug "openHVBox" PP.openHVBox
    val openHOVBox = debug "openHOVBox" PP.openHOVBox
    val openBox    = debug "openBox" PP.openBox
    val closeBox   = debug' "closeBox" PP.closeBox

    val token   = debug "token" PP.token
    val string  = debug "string" PP.string

    val pushStyle = PP.pushStyle
    val popStyle  = PP.popStyle

    val break   = debug "break" PP.break
    val space   = debug "space" PP.space
    val cut     = debug' "cut" PP.cut
    val newline = debug' "newline" PP.newline
    val nbSpace = debug "nbSpace" PP.nbSpace
    val control = debug "control" PP.control

  end;

