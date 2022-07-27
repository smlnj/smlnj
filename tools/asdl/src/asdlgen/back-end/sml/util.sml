(* util.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utility functions for generating SML code.
 *
 * TODO:
 *	more name mangling (structures, values, etc.)
 *	identify keywords and pervasive identifiers and add a ' on those names
 *)

structure Util : sig

  (* convert a module name to a signature name by converting all characters to upper
   * case and inserting "_" at case-change points.  The second argument is an optional
   * suffix. For example, "FooBar" ==> "FOO_BAR" or "FOO_BAR_SUFFIX"
   *)
    val sigName : string * string option -> string

  (* the unqualified names for the picker/unpickler functions *)
    val picklerName : AST.TypeId.t -> string
    val unpicklerName : AST.TypeId.t -> string

  end = struct

  (* character classification *)
    datatype cc = UC | LC | OTHER

    fun classify c = if Char.isUpper c then UC
	  else if Char.isLower c then LC
	  else OTHER

    fun sigName (name, suffix) = let
	  fun f (preCC, c2::cs, acc) = (case (preCC, classify c2)
		 of (UC, LC) => f (LC, cs, Char.toUpper c2 :: acc)
		  | (LC, UC) => f (UC, cs, c2 :: #"_" :: acc)
		  | (OTHER, UC) => f (UC, cs, c2 :: #"_" :: acc)
		  | (_, cc) => f (cc, cs, Char.toUpper c2 :: acc)
		(* end case *))
	    | f (_, [], acc) = let
		  val name' = String.implodeRev acc
		  in
		    case suffix
		     of NONE => name'
		      | SOME s => concat[name', "_", s]
		    (* end case *)
		  end
	  in
	    case String.explode name
	     of c::cs => f (classify c, cs, [Char.toUpper c])
	      | [] => raise Fail "empty module name"
	    (* end case *)
	  end

  (* the unqualified names for the picker/unpickler functions *)
    fun picklerName ty = (case SMLView.Type.getWriter ty
	   of NONE => "write_" ^ AST.TypeId.nameOf ty
	    | SOME wr => wr
	  (* end case *))

    fun unpicklerName ty = (case SMLView.Type.getReader ty
	   of NONE => "read_" ^ AST.TypeId.nameOf ty
	    | SOME rd => rd
	  (* end case *))

  end
