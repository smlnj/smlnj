(* sml90.sig
 *
 * COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies.
 *)

local
    structure Real = RealImp
    structure String = StringImp
in
structure SML90 :> SML90 =
  struct

    exception Io of string
    exception Sqrt
    exception Ln
    exception Ord = General.Overflow
    exception Abs = General.Overflow
    exception Prod = General.Overflow
    exception Neg = General.Overflow
    exception Sum = General.Overflow
    exception Diff = General.Overflow
    exception Floor = General.Overflow
    exception Exp = General.Overflow
    exception Quot = General.Div
    exception Mod = General.Div
    exception Interrupt

    fun sqrt x = if (x >= 0.0) then Math.sqrt x else raise Sqrt
    fun exp x = let
	  val r = Math.exp x
	  in 
	    if (Real.isFinite r) then r else raise Exp
	  end
    fun ln x = if (x > 0.0) then Math.ln x else raise Ln
    val sin = Math.sin
    val cos = Math.cos
    val arctan = Math.atan
    fun ord s = Char.ord(String.sub(s, 0))
    fun chr i = String.str(Char.chr i)
    fun explode s = CharVector.foldr (fn (c, l) => String.str c :: l) [] s
    val implode = String.concat

    type instream = TextIO.instream
    type outstream = TextIO.outstream

    fun wrapIO f x = (f x) handle ex => raise Io(ExnName.exnMessage ex)
    val std_in = TextIO.stdIn
    val open_in = wrapIO TextIO.openIn
    val input = wrapIO TextIO.inputN
    fun lookahead strm = (case wrapIO TextIO.lookahead strm
	   of NONE => raise Io "end of file"
	    | (SOME c) => String.str c
	  (* end case *))
    val close_in = wrapIO TextIO.closeIn
    val end_of_stream = wrapIO TextIO.endOfStream
    val std_out = TextIO.stdOut
    val open_out = wrapIO TextIO.openOut
    val output = wrapIO TextIO.output
    val close_out = wrapIO TextIO.closeOut

  end
end

