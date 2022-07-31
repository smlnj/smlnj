(* user.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common user decls from the two different lexers.
 *)

structure UserDeclarations =
  struct

    type pos = int

    type arg = {
	    comLevel : int ref,
	    sourceMap : SourceMap.sourcemap,
	    charlist : string list ref,
	    stringtype : bool ref,
	    stringstart : int ref, (* start of current string or comment*)
	    brack_stack : int ref list ref, (* for frags *)
	    err : pos*pos -> ErrorMsg.complainer
	  }

  (* common code to handle EOF *)
    fun eof ({comLevel,err,charlist,stringstart,sourceMap, ...} : arg) = let
	  val pos = Int.max(!stringstart+2, SourceMap.lastLinePos sourceMap)
	  in
	    if !comLevel > 0
	      then err (!stringstart, pos) ErrorMsg.COMPLAIN
		"unclosed comment" ErrorMsg.nullErrorBody
	    else if !charlist <> []
	      then err (!stringstart, pos) ErrorMsg.COMPLAIN
		"unclosed string, character, or quotation" ErrorMsg.nullErrorBody
	      else ();
	    pos
	  end

  (* support for string literals *)
    fun addString (charlist,s:string) = charlist := s :: (!charlist)
    fun addChar (charlist, c:char) = addString(charlist, String.str c)
    fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

  end
