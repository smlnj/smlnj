(* util.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utilities for processing simple styled text
 *)

structure Util : sig

    val style : string -> string
    val clean : string -> string

  end = struct

  (* handle some simple style notations found in the Asciidoctor title text.
   * We handle code "`" and bold "*" markup.  We assume that these are _not_
   * nested.
   *)
    fun style s = let
	  fun bold (#"*" :: #"*" :: r, acc) = (r, "</b>" :: acc)
	    | bold (#"*" :: r, acc) = (r, "</b>" :: acc)
	    | bold (c :: r, acc) = bold (r, str c :: acc)
	    | bold arg = arg
	  and code (#"`" :: #"`" :: r, acc) = (r, "</code>" :: acc)
	    | code (#"`" :: r, acc) = (r, "</code>" :: acc)
	    | code (c :: r, acc) = code (r, str c :: acc)
	    | code arg = arg
	  and text (#"*" :: #"*" :: r, acc) = text (bold (r, "<b>" :: acc))
	    | text (#"*" :: r, acc) = text (bold (r, "<b>" :: acc))
	    | text (#"`" :: #"`" :: r, acc) = text (code (r, "<code>" :: acc))
	    | text (#"`" :: r, acc) = text (code (r, "<code>" :: acc))
	    | text (c :: r, acc) = text (r, str c :: acc)
	    | text ([], acc) = acc
	  in
	    String.concat (List.rev (text (explode s, [])))
	  end

  (* strip code and bold markup *)
    fun clean s = String.translate (fn #"`" => "" | #"*" => "" | c => str c) s

  end
