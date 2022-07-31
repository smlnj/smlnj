(*
 * Version numbering for CM libraries.
 *
 * (C) 2000 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature VERSION = sig

    type t

    val fromString : string -> t option
    val toString: t -> string
    val compare : t * t -> order

    val nextMajor : t -> t

    val zero: t
end

structure Version :> VERSION = struct

    type t = { major: int, minor: int list }

    fun fromString s =
	let fun cvt (_, NONE) = NONE
	      | cvt (s, SOME l) =
		(case Int.fromString s of
		     SOME i => SOME (i :: l)
		   | NONE => NONE)
	in
	    case foldr cvt (SOME []) (String.fields (fn c => c = #".") s) of
		SOME (maj :: min) => SOME { major = maj, minor = min }
	      | _ => NONE
	end

    fun toString { major, minor } =
	concat (Int.toString major ::
		foldr (fn (i, l) => "." :: Int.toString i :: l)
		      [] minor)

    fun compare (v1: t, v2: t) = let
	fun lcmp ([], []) = EQUAL
	  | lcmp ([], _) = LESS
	  | lcmp (_, []) = GREATER
	  | lcmp (h :: t, h' :: t') =
	    (case Int.compare (h, h') of
		 EQUAL => lcmp (t, t')
	       | unequal => unequal)
    in
	lcmp (#major v1 :: #minor v1, #major v2 :: #minor v2)
    end

    fun nextMajor (v: t) = { major = #major v + 1, minor = [] }

    val zero = { major = 0, minor = [] }
end
