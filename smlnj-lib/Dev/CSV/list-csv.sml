(* list-csv.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Support for reading and writing comma-separated-value files.  The
 * syntax is based on RFC 4180 (http://tools.ietf.org/html/rfc4180),
 * except that "\n" is allowed as a line terminator and we don't handle
 * fields that are split across lines.
 *)

structure ListCSV : CSV where type 'a seq = 'a list =
  struct

    structure SS = Substring

    type 'a seq = 'a list

    exception Error

    fun parse ln = let
	(* scan the next field *)
	  fun splitNext start = let
	      (* scan an unquoted field *)
		fun scan ss = (case SS.getc ss
		       of NONE => (extract ss, ss)
			| SOME(#"\r", ss') => (case SS.getc ss'
			     of NONE => (extract ss, ss')
			      | SOME(#"\n", ss'') => if SS.isEmpty ss''
				  then (extract ss, ss'')
				  else raise Error
			      | _ => raise Error
			    (* end case *))
			| SOME(#"\n", ss') => if SS.isEmpty ss'
			    then (extract ss, ss')
			    else raise Error
			| SOME(#",", ss') => (extract ss, ss')
			| SOME(#"\"", _) => raise Error
			| SOME(_, ss') => scan ss'
		      (* end case *))
		and extract ss = SS.string(SS.trimr (SS.size ss) start)
	      (* scan a quoted field *)
		fun scanQ (ss, chrs) = (case SS.getc ss
		       of NONE => raise Error
			| SOME(#"\r", _) => raise Error
			| SOME(#"\n", _) => raise Error
			| SOME(#"\"", ss') => (case SS.getc ss'
			     of NONE => (extractQ chrs, ss')
			      | SOME(#"\r", ss') => (case SS.getc ss'
				   of NONE => (extractQ chrs, ss')
				    | SOME(#"\n", ss'') => if SS.isEmpty ss''
					then (extractQ chrs, ss'')
					else raise Error
				    | _ => raise Error
				  (* end case *))
			      | SOME(#"\n", ss') => if SS.isEmpty ss'
				  then (extractQ chrs, ss')
				  else raise Error
			      | SOME(#"\"", ss'') => scanQ(ss'', #"\"" :: chrs)
			      | SOME(#",", ss'') => (extractQ chrs, ss'')
			      | _ => raise Error
			    (* end case *))
			| SOME(c, ss') => scanQ (ss', c::chrs)
		      (* end case *))
		and extractQ chrs = String.implode(List.rev chrs)
		in
		  case SS.getc start
		   of SOME(#"\"", ss') => scanQ(ss', [])
		    | _ => scan start
		  (* end case *)
		end
	  and scanLine (ss, fields) = if SS.isEmpty ss
		then List.rev fields
		else let
		  val (fld, ss) = splitNext ss
		  in
		    scanLine (ss, fld::fields)
		  end
	  in
	    SOME(scanLine (SS.full ln, []))
	      handle Error => NONE
	  end

  (* convert a CSV line to a sequence of its fields; returns NONE on error *)
    fun fromString s = parse s

    val hasComma = CharVector.exists (fn #"," => true | _ => false)

    fun cvtField s = if hasComma s
	  then concat["\"", s, "\""]  (* what if it has a quote? *)
	  else s

  (* convert a sequence to a string *)
    fun toString flds = String.concatWith "," (List.map cvtField flds)

    fun fmt cvt = let
	  fun cvt' x = cvtField (cvt x)
	  in
	    fn flds => String.concatWith "," (List.map cvt' flds)
	  end

  end
