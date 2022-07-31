(* char-buffer-pp.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that puts its output in a CharBuffer.buf object.  There
 * are no styles and tokens are strings.  You can use this module to pretty-print
 * into a string as follows:
 *
 *	val buf = CharBuffer.new 1024
 *	val ppStrm = CharBufferPP.openBuf {dst = buf, wid = 80}
 *	.... pretty printing ....
 *	val result = CharBuffer.contents buf
 *)

structure CharBufferPP : sig

    include PP_STREAM
      where type token = string

    val openBuf : {dst : CharBuffer.buf, wid : int} -> stream

  end = struct

    structure Device = CharBufferDev

    structure PP = PPStreamFn (
      structure Token = StringToken
      structure Device = Device)

    open PP

    fun openBuf arg = PP.openStream (Device.openDev arg)

  end
