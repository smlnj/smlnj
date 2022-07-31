(* keyword-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This functor is meant to be used as part of a scanner, where identifiers
 * and keywords are scanned using the same lexical rules and are then
 * further analyzed.
 *)

functor KeywordFn (KW : sig
    type token
    type pos
    val ident : (Atom.atom * pos * pos) -> token
    val keywords : (string * ((pos * pos) -> token)) list
  end) : sig
    type token
    type pos
    val keyword : (string * pos * pos) -> token
  end = struct

    structure A = Atom
    structure Tbl = AtomTable

    type token = KW.token
    type pos = KW.pos

  (* the keyword hash table *)
    exception Keyword
    val kwTbl : ((pos * pos) -> token) Tbl.hash_table =
	  Tbl.mkTable(List.length KW.keywords, Keyword)

  (* insert the reserved words into the keyword hash table *)
    val _ = let
	  val insert = Tbl.insert kwTbl
	  fun ins (s, item) = insert (A.atom s, item)
	  in
	    app ins KW.keywords
	  end

    fun keyword (s, p1, p2) = let
	  val name = A.atom s
	  in
	    case (Tbl.find kwTbl name)
	     of (SOME tokFn) => tokFn(p1, p2)
	      | NONE => KW.ident(name, p1, p2)
	    (* end case *)
	  end

  end;

