(* char-map-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Fast, read-only, maps from characters to values.
 *)

signature CHAR_MAP =
  sig

    type 'a char_map
	(* a finite map from characters to 'a *)

    val mkCharMap : {default : 'a, bindings : (string * 'a) list} -> 'a char_map
	(* make a character map which maps the bound characters to their
	 * bindings and maps everything else to the default value.
	 *)

    val mapChr : 'a char_map -> char -> 'a
	(* map the given character *)
    val mapStrChr : 'a char_map -> (string * int) -> 'a
	(* (mapStrChr c (s, i)) is equivalent to (mapChr c (String.sub(s, i))) *)

  end (* CHAR_MAP *)

