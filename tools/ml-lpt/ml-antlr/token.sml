(* token.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Utility functions for the token datatype.
 *)

structure Token =
  struct

    datatype token = datatype LLKSpec.token

    fun name (T{name, ...}) = Atom.toString name
    fun ty (T{ty, ...}) = ty
    fun hasTy (T{ty = SOME _, ...}) = true
      | hasTy _ = false
    fun isKW (T{keyword, ...}) = keyword
    fun default (T{default, ...}) = default

    fun toString (T{abbrev = SOME a, ...}) = Atom.toString a
      | toString tok = name tok

    fun quoted (T{abbrev = SOME a, ...}) = Atom.toString a
      | quoted tok = String.concat["\"", name tok, "\""]

    fun def tok = (case ty tok
           of NONE => name tok
            | SOME ty => String.concat[name tok, " of ", ty]
          (* end case *))

    fun compare (T{id=a, ...}, T{id=b, ...}) = Int.compare(a, b)
    fun lexCompare (T{name=a, ...}, T{name=b, ...}) =
	  String.compare(Atom.toString a, Atom.toString b)
    fun same (T{id=a, ...}, T{id=b, ...}) = (a = b)

    structure Set = RedBlackSetFn (
      struct
	type ord_key = token
	val compare = compare
      end)

    structure Map = RedBlackMapFn (
      struct
	type ord_key = token
	val compare = compare
      end)

    fun setToString s = let
	(* simple insertion sort to lexically order the set *)
	  fun ins (tok, []) = [tok]
	    | ins (tok, tok'::toks) = (case lexCompare(tok, tok')
		 of LESS => tok::tok'::toks
		  | _ => tok'::ins(tok, toks)
		(* end case *))
	  val toks = Set.foldl ins [] s
	  in
	    String.concat[
		"{", String.concatWith "," (List.map toString toks), "}"
	      ]
	  end

  end
