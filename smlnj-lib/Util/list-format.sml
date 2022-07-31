(* list-format.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ListFormat : LIST_FORMAT =
  struct

  (* given an initial string (init), a separator (sep), a terminating
   * string (final), and an item formating function (fmt), return a list
   * formatting function.  The list ``[a, b, ..., c]'' gets formated as
   * ``init ^ (fmt a) ^ sep ^ (fmt b) ^ sep ^ ... ^ sep ^ (fmt c) ^ final.''
   *)
    fun fmt {init, sep, final, fmt} = let
	  fun format [] = init ^ final
	    | format [x] = concat[init, fmt x, final]
	    | format (x::r) = let
		fun f ([], l) = concat(rev(final::l))
		  | f (x::r, l) = f (r, (fmt x) :: sep :: l)
		in
		  f (r, [fmt x, init])
		end
	  in
	    format
	  end (* formatList *)

    fun listToString f = fmt {init="[", sep=",", final="]", fmt=f}

  (* given an expected initial string, a separator, a terminating
   * string, and an item scanning function, return a function that
   * scans a string for a list of items.  Whitespace is ignored.
   * If the input string has the incorrect syntax, then the exception
   * ScanList is raised with the position of the first error.
   *)
    fun scan {init, sep, final, scan} getc strm = let
	  val skipWS = StringCvt.skipWS getc
	  val scanItem = scan getc
	  fun eat "" = (fn strm => (true, skipWS strm))
	    | eat s = let
		val n = size s
		fun isPrefix (i, strm) =
		      if (i = n) then SOME strm
		      else (case getc strm
			 of (SOME(c, strm)) => if (String.sub(s, i) = c)
			      then isPrefix(i+1, strm)
			      else NONE
			  | NONE => NONE
			(* end case *))
		fun eat' strm = (
		      case isPrefix (0, skipWS strm)
		       of (SOME strm) => (true, strm)
			| NONE => (false, strm)
		      (* end case *))
		in
		  eat'
		end
	  val isInit = eat init
	  val isSep = eat sep
	  val isFinal = eat final
	  fun scan (strm, l) = (case (isFinal strm)
		 of (true, strm) => SOME(rev l, strm)
		  | (false, strm) => (case isSep strm
		       of (true, strm) => (case scanItem (skipWS strm)
			     of (SOME(x, strm)) => scan (strm, x::l)
			      | NONE => NONE
			    (* end case *))
			| _ => NONE
		      (* end case *))
		(* end case *))
	  in
	    case (isInit strm)
	     of (true, strm) => (case (isFinal strm)
		   of (true, strm) => SOME([], strm)
		    | (false, strm) => (case scanItem (skipWS strm)
			 of (SOME(x, strm)) => scan (strm, [x])
			  | NONE => NONE
			(* end case *))
		  (* end case *))
	      | (false, i) => NONE
	    (* end case *)
	  end (* scan *)

  end; (* ListFormat *)
