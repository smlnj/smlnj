(* text-pp.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that generates plain text; either to a TextIO.outstream
 * or to a CharBuffer.buf object.  It essentially unifies the behavior of
 * the TextIOPP and CharBufferPP structures.
 *)

structure TextPP : sig

    include PP_STREAM
      where type token = string

    val openOutstream : {dst : TextIO.outstream, wid : int} -> stream

    val openBuffer : {dst : CharBuffer.buf, wid : int} -> stream

  end = struct

    structure DevOps = struct
	datatype t = OPS of {
	    add1 : char -> unit,
	    addVec : string -> unit,
	    flush : unit -> unit
	  }
      (* no style support *)
	type style = unit
	fun sameStyle _ = true
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()
      (* output some number of spaces to the device *)
	fun space (OPS{addVec, ...}, n) = addVec (StringCvt.padLeft #" " n "")
	val indent = space
      (* output a new-line to the device *)
	fun newline (OPS{add1, ...}) = add1 #"\n"
      (* output a string/character in the current style to the device *)
	fun string (OPS{addVec, ...}, s) = addVec s
	fun char (OPS{add1, ...}, c) = add1 c
      (* flush output *)
	fun flush (OPS{flush, ...}) = flush()
      end

    structure Device = DefaultDeviceFn (DevOps)

    structure PP = PPStreamFn (
      structure Token = StringToken
      structure Device = Device)

    open PP

    fun openOutstream {dst, wid} = let
	  val dev = Device.newWithWidth (DevOps.OPS{
		  add1 = fn c => TextIO.output1 (dst, c),
		  addVec = fn c => TextIO.output (dst, c),
		  flush = fn () => TextIO.flushOut dst
		}, wid)
	  in
	    PP.openStream dev
	  end

    fun openBuffer {dst, wid} = let
	  val dev = Device.newWithWidth (DevOps.OPS{
		  add1 =  fn c => CharBuffer.add1 (dst, c),
		  addVec = fn c => CharBuffer.addVec (dst, c),
		  flush = fn () => ()
		}, wid)
	  in
	    PP.openStream dev
	  end

  end
