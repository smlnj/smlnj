(*
 * Re-write top-level bindings of a given symbol with corresponding bindings
 * of CoreSym.coreSym (i.e., "structure _Core").
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
local
    structure A = Ast
in
    structure CoreHack : sig
	val rewrite : A.dec * Symbol.symbol -> A.dec
    end = struct
        fun rewrite (d, esy) = let
	    fun strb (x as A.Strb { name, def, constraint }) =
		if Symbol.eq (name, esy) then
		    A.Strb { name = CoreSym.coreSym, def = def,
			     constraint = constraint }
		else x
	      | strb (A.MarkStrb (x, r)) = A.MarkStrb (strb x, r)
	    fun dec (A.StrDec l) = A.StrDec (map strb l)
	      | dec (A.LocalDec (d1, d2)) = A.LocalDec (d1, dec d2)
	      | dec (A.SeqDec l) = A.SeqDec (map dec l)
	      | dec (A.MarkDec (d, r)) = A.MarkDec (dec d, r)
	      | dec d = d
	in
	    dec d
	end
    end
end
