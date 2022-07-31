(* char-map.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Fast, read-only, maps from characters to values.
 *)

structure CharMap :> CHAR_MAP =
  struct

  (* we can use unchecked array operations, since the indices are always
   * in range.
   *)
    val sub = Unsafe.Array.sub
    val update = Unsafe.Array.update

  (* a finite map from characters to 'a *)
    type 'a char_map = 'a Array.array

  (* make a character map which maps the bound characters to their
   * bindings and maps everything else to the default value.
   *)
    fun mkCharMap {default, bindings} = let
	(* this array maps characters to indices in the valMap *)
	  val arr = Array.array (Char.maxOrd, default)
	  fun doBinding (s, v) =
		CharVector.app (fn c => update(arr, Char.ord c, v)) s
	  in
	    List.map doBinding bindings;
	    arr
	  end

  (* map the given character ordinal *)
    fun mapChr cm i = sub(cm, Char.ord i)

  (* (mapStrChr c (s, i)) is equivalent to (mapChr c (String.sub(s, i))) *)
    fun mapStrChr cm (s, i) = sub(cm, Char.ord(String.sub(s, i)))

  end (* CharMap *)
