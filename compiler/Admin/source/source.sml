(* Basics/source/source.sml
 *
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Source : SOURCE =
struct

  type charpos = int  (* INVARIANT: charpos > 0 *)
  type lineno = int   (* INVARIANT: lineno > 0 *)

  type sourceMap = (charpos * lineno) list

(* The representation of sourceMap is a list of (charpos, lineno) pairs, each
   representing a line in the source stream (a text file or interactive input stream).
   Each line pair in sourceMap consists of:
     - charpos : the position (base 1) of the first character in the line, or 1 + the
       position of the newline character that ended the previous line if no
       characters have been produced after the last newline.
     - lineno: the line number (base 1) of the current line, which starts at
       charpos 1 if it is the first line, or the charpos after a newline that
       terminates the previous line.
   The charpos components of successive lines are in decreasing order, i.e. new lines
   are added to the front of the list. The last elementin the sourceMap list is
   always (1, 1), representing the first line in the file, (1,1), since lines are
   numbered from 1, and the first character position is 1.

   The representation satisfies these invariants:
     * The line list is never empty (initialized to [(1,1)], and thereafter lines are only added).
     * Initial positions are strictly decreasing accross the line list,
       even for "empty" lines, which have length at least 1 because we count the newline char.
     * The last element in the line list contains the smallest valid starting charpos (namely 1).

   INVARIANTS for sourceMaps:
    (1) length (sourceMap) > 0 (initialized to [(1,1)] and only grows)
    (2) (initial) charpos components of sourceMap  are strictly decreasing,
         ending in initial charpos 1 for the last element, which represents the 1st line.
    (3) Last element of a sourceMap represents the first line: (1, 1).

   NOTE: the source map does not store the _contents_ of the lines, only the starting character
   positions of the lines.
*)

  type source =
       {sourceMap: sourceMap ref,      (* map of the "lines" of the source stream *)
        fileOpened: string,            (* what is it if the source is not from a file? "stdIn"? *)
        interactive: bool,             (* true if interactive (stdIn), thus not from a file *)
        sourceStream: TextIO.instream, (* the actual source of characters *)
        content: string option ref,    (* memo of content of "full" source *)
        anyErrors: bool ref}           (* records occurrence of errors while processing source *)

  (* Note that the sourceMap component is a ref, which is updated at each newline encountered in
   * the input stream. Inplicitly, this means that the input character count is maintained to 
   * produce the charpos of the beginning of each line. *)

  val initSourceMap : sourceMap = [(1,1)]

  (* newSource : string * TextIO.instream * bool -> source *)
  fun newSource (fileName, sourceStream, interactive) =
      {sourceMap = ref initSourceMap,
       fileOpened = fileName,  (* should perhaps be a string option: NONE for interactive source?  *)
       interactive = interactive,
       sourceStream = sourceStream,
       content = ref NONE,
       anyErrors = ref false}

  (* closeSource : source -> unit *)
  fun closeSource ({interactive=true, ...} : source) = ()
    | closeSource ({sourceStream, ...}) =
        (TextIO.closeIn sourceStream handle IO.Io _ => ())

  (* newline : source * charpos -> unit
   * pos is the position of the newline character, so the next line
   * starts at the succeeding character position, pos+1. *)
  fun newline ({sourceMap, ...}: source, pos: charpos) : unit =
      case !sourceMap
        of lines as ((_,line) :: _) =>  sourceMap := (pos+1, line+1) :: lines
         | nil => ()  (* BUG! sourcemap invariant (1) violated, not reported as bug *)

  (* getContent : source -> string option *)
  fun getContent ({fileOpened,interactive,content,...}: source) : string option =
      case !content
        of NONE =>  (* not previously captured *)
            if interactive then NONE
              (* would need to record history of all interactive input (from some point?) *)
	    else (let val s = TextIO.inputAll(TextIO.openIn fileOpened)
		   in content := SOME s;
		      !content
		  end handle IO.Io _ => NONE)
         | s => s  (* already captured *)

  (* sourceName : source -> string *)
  (* returns contents of fileOpened field *)
  fun sourceName ({fileOpened,...}: source) = fileOpened

end (* structure Source *)

(* Notes:
  
1. DBM: Why is _anyErrors_ a component of the source?
   What about the former "errConsumer" component?
   if not global state, where would it go? Defacto, it is in Control.Print! (Control.Print.say) 
   Perhaps "anyErrors" belongs (only) in a "compilation unit" state record, such as compInfo?
   compInfo currently contains source, and has been trimmed down to eliminate redundancy with
   source.

2. dummySource is introduced to provide a dummy argument to error in place of having a special
   errorNoSource file.
*)
