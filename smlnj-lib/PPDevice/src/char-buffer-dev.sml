(* char-buffer-dev.sml
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that puts its output in a CharBuffer.buf object.  There
 * are no styles and tokens are strings.
 *
 * One can extract the current contents using the `CharBuffer.contents` operation
 * and clear the buffer using the `CharBuffer.reset` function.
 * A typical function to pretty print to a string is:
 *
 *      fun ppToString wid = let
 *            val cb = CharBuffer.new 256
 *            val dev = CharBufferDev.openDev {dst = cb, wid = wid}
 *            in
 *              ... pretty print to dev ...
 *              CharBuffer.contents cb
 *            end
 *)

structure CharBufferDev : sig

    include PP_DEVICE
      where type style = unit
      where type token = string

    val openDev : {dst : CharBuffer.buf, wid : int} -> device

  end = struct

    structure DevOps = struct
	type t = CharBuffer.buf
      (* no style support *)
	type style = unit
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()
      (* trivial tokens *)
        type token = string
      (* output some number of spaces to the device *)
        fun space (dst, n) = CharBuffer.addVec (dst, StringCvt.padLeft #" " n "")
	val indent = space
      (* output a new-line to the device *)
	fun newline dst = CharBuffer.add1 (dst, #"\n")
      (* output a string in the current style to the device *)
	fun string (dst, s) = CharBuffer.addVec (dst, s)
	fun token (dst, t) = CharBuffer.addVec (dst, t)
      (* nothing to flush *)
	fun flush dst = ()
      end

    structure Device = MakePPDeviceFn (DevOps)

    open Device

    fun openDev {dst, wid} = Device.newWithWidth (dst, wid)

  end
