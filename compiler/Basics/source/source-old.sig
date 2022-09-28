(* source.sig
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ
 *)

signature SOURCE =
  sig
    type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        interactive: bool,
        sourceStream: TextIO.instream,
        content: string option ref,
        anyErrors: bool ref,
        errConsumer: PrettyPrint.device
      }

    val newSource : (string * TextIO.instream * bool * PrettyPrint.device)
          -> inputSource

    val closeSource: inputSource -> unit

    val filepos: inputSource -> SourceMap.charpos -> SourceMap.sourceloc
    (* simply calls SourceMap.filepos on the sourceMap component of inputSource,
     * provided for convenience. *)

    val getContent : inputSource -> string option

    val regionContent : inputSource * SourceMap.region ->
			(string * SourceMap.region * int) option

    val sourceName : inputSource -> string
    (* returns contents of fileOpened field *)

end (* signature SOURCE *)

(*
The fileOpened field contains the name of the file that was opened to
produce a particular inputSource.  It is used to derive related
file names (for example, see CompileF.codeopt and CompileF.parse
in build/compile.sml.). It is also used when we need to access the content
of the sourcefile for error messages (getContent).  This assumes that the
current directory remains fixed if the file name is a relative path.

newSource takes as argument a file name, the corresponding instream of the
opened file, a boolean flag indicating whether the source is interactive
(i.e. stdIn), and a prettyPrint device. (Note: Formerly newSource also took
an additional int argument representing the initial line number, but this
argument was always 1).

getContent only works if the source is a single file (no #line directives
changing the source file), and it won't work for an interactive source.
[This needs to be fixed.]

*)

