(* os-path.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *
 * This is the UNIX implementation of the generic OS.Path structure.
 *)

structure OS_Path = OS_PathFn (
  struct

    exception Path

    datatype arc_kind = Null | Parent | Current | Arc of string

    fun classify "" = Null
      | classify "." = Current
      | classify ".." = Parent
      | classify a = Arc a

    val parentArc = ".."

    val currentArc = "."

  (* any character other than "/" and "\000" is allowed by the POSIX spec.  Strictly
   * speaking, we should also check that the length is less than NAME_MAX characters.
   *)
    fun validArc arc = let
	  fun ok #"/" = false
	    | ok #"\000" = false
	    | ok c = true
	  in
	    CharVector.all ok arc
	  end

    fun validVolume (_, vol)= Substring.isEmpty vol

    val volSS = Substring.full ""

  (* Note: we are guaranteed that this is never called with "" *)
    fun splitVolPath s = if (InlineT.CharVector.sub(s, 0) = #"/")
	  then (true, volSS, Substring.triml 1 (Substring.full s))
	  else (false, volSS, Substring.full s)

    fun joinVolPath (true, "", "") = "/"
      | joinVolPath (true, "", s) = "/" ^ s
      | joinVolPath (false, "", s) = s
      | joinVolPath _ = raise Path (* invalid volume *)

    val arcSepChar = #"/"

    fun sameVol (v1, v2: string) = v1 = v2

  end);

