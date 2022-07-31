(* label.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

structure Label :> LABEL =
  struct

    datatype label_kind = GLOB of string | LOCAL of string | ANON

    type label = {
	id : word,
	addr : int ref,
	kind : label_kind
      }

    local
      val cnt = ref 0w0
    in
    fun reset () = cnt := 0w0
    fun mkLab k = let val id = !cnt in cnt := id+0w1; {id=id, addr=ref ~1, kind=k} end
    end (* local *)

  (* make a global label *)
    fun global name = mkLab(GLOB name)

  (* make a label generator; note that if the prefix string is "", then
   * the standard prefix "L" will be used.
   *)
    fun label "" = label "L"
      | label prefix = let
	  val kind = LOCAL prefix
	  in
	    fn () => mkLab kind
	  end

  (* make an anonymous label *)
    fun anon () = mkLab ANON

  (* label equality, comparisons, and hashing *)
    fun same (l1 : label, l2 : label) = (#id l1 = #id l2)
    fun compare (l1 : label, l2 : label) = Word.compare(#id l1, #id l2)
    fun hash (l : label) = #id l

  (* label addresses *)
    exception GlobalLabel
    fun setAddr ({id, addr, kind=GLOB _}, _) = raise GlobalLabel
      | setAddr ({id, addr, kind}, a) = addr := a

    fun addrOf {id, addr, kind=GLOB _} = raise GlobalLabel
      | addrOf {id, addr, kind} = !addr

  (* return a string representation of the label; this function is meant for
   * debugging; use the fmt function for assembly output.
   *)
    fun toString {id, addr, kind=GLOB name} = name
      | toString {id, addr, kind=LOCAL prefix} = prefix ^ Word.toString id
      | toString {id, addr, kind=ANON} = ".L" ^ Word.toString id

  (* format a label for assembly output.  The gPrefix argument is the target
   * ABI's prefix for global symbols (e.g., "_" or "") and the aPrefix is
   * the target assembler's prefix for anonymous labels.  Local labels are
   * emitted using their specified prefxix.
   *)
    fun fmt {gPrefix, aPrefix} = let
	  fun toStr {id, addr, kind=GLOB name} = gPrefix ^ name
	    | toStr {id, addr, kind=LOCAL prefix} = prefix ^ Word.toString id
	    | toStr {id, addr, kind=ANON} = aPrefix ^ Word.toString id
	  in
	    toStr
	  end

  end
