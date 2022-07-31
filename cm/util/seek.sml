(*
 * Seek in an instream.
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature SEEK = sig

    exception UnableToSeek

    val seek : BinIO.instream * Position.int -> unit
end

structure Seek :> SEEK = struct

    exception UnableToSeek

    val emptyVector = Word8Vector.fromList []
    
    fun seek (s, pos) = let
	val fs = BinIO.getInstream s
	val (r, _) = BinIO.StreamIO.getReader fs
	val BinPrimIO.RD { setPos, ... } = r
    in
	case setPos of
	    NONE => raise UnableToSeek
	  | SOME sp => let
		val _ = sp pos
		val fs' = BinIO.StreamIO.mkInstream (r, emptyVector)
	    in
		BinIO.setInstream (s, fs')
	    end
    end
end
