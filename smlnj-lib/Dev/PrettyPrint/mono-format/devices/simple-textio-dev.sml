(* simple-textio-dev.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A simple (no styles) pretty-printing device for output to TextIO outstreams.
 *)

structure SimpleTextIODev : sig

    include PP_DEVICE

    val openDev : {dst : TextIO.outstream, wid : int} -> device

  end = struct

    structure DevOps = struct
	type t = TextIO.outstream
      (* no style support *)
	type style = unit
	fun sameStyle _ = true
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()
      (* output some number of spaces to the device *)
        fun space (dst, n) = TextIO.output (dst, StringCvt.padLeft #" " n "")
	val indent = space
      (* output a new-line to the device *)
	fun newline dst = TextIO.output1 (dst, #"\n")
      (* output a string/character in the current style to the device *)
	fun string (dst, s) = TextIO.output (dst, s)
	fun char (dst, c) = TextIO.output1 (dst, c)
      (* flush output stream *)
	fun flush dst = TextIO.flushOut dst
      end

    structure Device = DefaultDeviceFn (DevOps)

    open Device

    fun openDev {dst, wid} = Device.newWithWidth (dst, wid)

  end;
