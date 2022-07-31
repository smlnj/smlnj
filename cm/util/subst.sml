(*
 * A string substitution facility.
 *
 *   (C) 2001 Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure Subst :> sig

    type substitution = Substring.substring -> string option

    (* Given the spec (1st argument), scan the second argument for
     * all occurences of prefixes and try the substitutions (left-to-right)
     * associated with the matching prefix until one matches. *)
    val substitute :
	{ prefix : string, substitutions: substitution list } list ->
	string -> string

    (* a simple string replacement substitution *)
    val subfor : string -> string -> substitution

    (* given prefix length and stop character, use a given substitution
     * on the subslice without prefix and stop char *)
    val submap : int * char -> substitution -> substitution

    (* a list selection substitution:
     *   The first argument is the general spec, the second argument is the
     *   list to select from.  The general spec consists of:
     *  - the length of the prefix (the prefix will be ignored),
     *  - the stop character (which will also be ignored
     *  - a selector that extracts the substitution string from a list element
     *  - a separator string used when n = 0 or missing (the whole list gets
     *    inserted in this case, with the separator string separating elements)
     *)
    val subnsel : int * char * ('a -> string) * string -> 'a list -> substitution

end = struct

    structure SS = Substring

    type substitution = SS.substring -> string option

    fun substitute rules = let
	val rules =
	    map (fn { prefix, substitutions } =>
		    { prefix = SS.full prefix, substitutions = substitutions })
		rules
	fun doit s = let
	    val len = size s
	    fun loop (i0, i, acc) = let
		fun matchingrule { prefix, substitutions } = let
		    val plen = SS.size prefix
		in
		    i + plen <= len andalso
		    SS.compare (prefix, SS.substring (s, i, plen)) = EQUAL
		end
		fun findrule () =
		    Option.map #substitutions (List.find matchingrule rules)
		fun newacc k =
		    if k > i0 then SS.substring (s, i0, k - i0) :: acc else acc
	    in
		if i >= len then SS.concat (rev (newacc len))
		else case findrule () of
			 NONE => loop (i0, i + 1, acc)
		       | SOME substitutions => let
			     val acc = newacc i
			     fun dosubst j = let
				 fun finddosubst [] = dosubst (j + 1)
				   | finddosubst (replace :: sl) = let
					 val ss = SS.substring (s, i, j - i)
				     in
					 case replace ss of
					     NONE => finddosubst sl
					   | SOME r =>
					     loop (j, j, SS.full r :: acc)
				     end
			     in
				 if j > len then loop (i, len, acc)
				 else finddosubst substitutions
			     end
			 in
			     dosubst (i + 1)
			 end
	    end
	in
	    loop (0, 0, [])
	end
    in
	doit
    end

    fun subfor p r ss =
	if Substring.compare (Substring.full p, ss) = EQUAL then SOME r
	else NONE

    fun submap (plen, stopchar) m ss = let
	val sslen = SS.size ss
    in
	if SS.sub (ss, sslen - 1) = stopchar then
	    m (SS.slice (ss, plen, SOME (sslen - plen - 1)))
	else NONE
    end

    fun subnsel (plen, stopchar, sel, sep) l ss = let
	fun m numslice = let
	    val nums = SS.string numslice
	    fun all () = SOME (String.concatWith sep (map sel l))
	    fun seli i =
		SOME (sel (List.nth (l, i))
		      handle General.Subscript => SS.string ss)
	in
	    if nums = "" then all ()
	    else case Int.fromString nums of
		     SOME 0 => all ()
		   | SOME i => seli (i - 1)
		   | NONE => SOME (SS.string ss)
	end
    in
	submap (plen, stopchar) m ss
    end

end
