(* user.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common user decls from the two different lexers.
 *)

structure UserDeclarations =
struct

local

   structure SR = Source
   structure SM = SourceMap
   structure EM = ErrorMsg

in

    type pos = int

    type arg = {
	    comLevel : int ref,
	    source : SR.source,
	    charlist : string list ref,
	    stringtype : bool ref,
	    stringstart : int ref, (* start of current string or comment*)
	    brack_stack : int ref list ref, (* for frags *)
	    err : (int * int) -> EM.complainer
	  }

    (* eof : arg -> pos *)
    (* common code to handle EOF *)
    fun eof ({comLevel, err, charlist, stringstart, source = {sourceMap, ...}, ...} : arg) =
	let val pos = Int.max (!stringstart+2, SM.lastLineStartPos (!sourceMap))
	 in if !comLevel > 0
	    then err (!stringstart, pos)
		     EM.COMPLAIN "unclosed comment" EM.nullErrorBody
	    else (case !charlist
		    of nil => ()
		     | _ => err (!stringstart, pos)
				EM.COMPLAIN
				"unclosed string, character, or quotation"
				EM.nullErrorBody);
	    pos
	  end

  (* support for string literals *)
    fun addString (charlist, s:string) = charlist := s :: (!charlist)
    fun addChar (charlist, c:char) = addString (charlist, String.str c)
    fun makeString charlist = concat (rev (!charlist)) before (charlist := nil)

end (* top local *)
end (* UserDeclarations *)
