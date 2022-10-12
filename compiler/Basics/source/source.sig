(* Basics/source/source.sig
 * COPYRIGHT (c) 2022 The Fellowship of SML/NJ
 *)

signature SOURCE =
sig

  type charpos = int  (* INVARIANT: charpos > 0 *)
  type lineno = int   (* INVARIANT: lineno > 0 *)
  type sourceMap = (charpos * lineno) list

  type source =
       {sourceMap: sourceMap ref,
	fileOpened: string,
	interactive: bool,
	sourceStream: TextIO.instream,
	content: string option ref,
	anyErrors: bool ref}

  val initSourceMap : sourceMap  (* ever needed? - used in sourceMap *)

  val newSource : (string * TextIO.instream * bool) -> source
  (* args are fileOpened, sourceStream, and interactive *)

  val closeSource: source -> unit
  (* close the "fileOpened" if not interactive *)

  val newline : source * charpos -> unit
  (* records that newline char occurred at charpos, updating sourceMap field *)

  val getContent : source -> string option
  (* return NONE if source interactive, otherwise SOME of complete contents of
   * fileOpened as string *)

  val sourceName : source -> string
  (* contents of fileOpened field of source *)

end (* signature SOURCE *)

(* filepos replaced by SourceMap.charposToLocation *)
(* regionContent moved to SourceMap.regionContent *)
(* type inputSource renamed source *)

