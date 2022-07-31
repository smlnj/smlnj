(* ansi-term-pp.sml
 *
 * COPYRIGHT (c) 2005 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure ANSITermPP : sig

    structure Tok : sig
	include PP_TOKEN
	  where type style = ANSITermDev.style
	val token : (ANSITermDev.style * string) -> token
      end

    include PP_STREAM
      where type device = ANSITermDev.device
      where type style = ANSITermDev.style
      where type token = Tok.token

    val openOut : {dst : TextIO.outstream, wid : int} -> stream

  end = struct

    structure Tok =
      struct
	type style = ANSITermDev.style
	datatype token = Tok of (style * string)
	fun string (Tok(sty, s)) = s
	fun style (Tok(sty, s)) = sty
	fun size (Tok(sty, s)) = String.size s
	val token = Tok
      end

    structure PP = PPStreamFn (
      structure Token = Tok
      structure Device = ANSITermDev)

    open PP

    fun openOut arg = openStream(ANSITermDev.openDev arg)

  end
