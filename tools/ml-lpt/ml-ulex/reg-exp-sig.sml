(* reg-exp-sig.sml
 *
 * COPYRIGHT (c) 2005 
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (adrassi@gmail.com)
 * All rights reserved.
 *
 * RE representation and manipulation
 *)

signature REG_EXP = 
  sig

    structure Sym : INTERVAL_DOMAIN
    structure SymSet : INTERVAL_SET

    type symbol = UTF8.wchar
    type sym_set = SymSet.set
    type re

    val any	  : re	(* wildcard *)
    val none	  : re	(* EMPTY language *)
    val epsilon	  : re	(* the nil character (of length 0) *)

    val mkSym     : symbol -> re
    val mkSymSet  : sym_set -> re

    val mkOr      : re * re -> re
    val mkAnd     : re * re -> re
    val mkXor     : re * re -> re
    val mkNot     : re -> re
    val mkConcat  : re * re -> re
    val mkClosure : re -> re
    val mkOpt     : re -> re
    val mkRep     : re * int * int -> re
    val mkAtLeast : re * int -> re

    val isNone    : re -> bool
    val nullable  : re -> bool
    val derivative : symbol -> re -> re
    val derivatives : re Vector.vector -> 
		      ((re Vector.vector) * sym_set) list

    val symToString : symbol -> string
    val toString  : re -> string
    val compare   : re * re -> order

  end
