(* char-buffer-dev.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that puts its output in a CharBuffer.buf object.  There
 * are no styles and tokens are strings.
 *)

structure CharBufferDev : sig

    include PP_DEVICE

    val openDev : {dst : CharBuffer.buf, wid : int} -> device

  end = struct

    structure DevOps = struct
	type t = CharBuffer.buf
      (* no style support *)
	type style = unit
	fun sameStyle _ = true
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()
      (* output some number of spaces to the device *)
        fun space (dst, n) = CharBuffer.addVec (dst, StringCvt.padLeft #" " n "")
	val indent = space
      (* output a new-line to the device *)
	fun newline dst = CharBuffer.add1 (dst, #"\n")
      (* output a string/character in the current style to the device *)
	fun string (dst, s) = CharBuffer.addVec (dst, s)
	fun char (dst, c) = CharBuffer.add1 (dst, c)
      (* nothing to flush *)
	fun flush dst = ()
      end

    structure Device = DefaultDeviceFn (DevOps)

    open Device

    fun openDev {dst, wid} = Device.newWithWidth (dst, wid)

  end
