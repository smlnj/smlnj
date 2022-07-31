(* source.sml
 *
 * COPYRIGHT (c) 2012 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Source : SOURCE =
struct

  type inputSource = {
        sourceMap: SourceMap.sourcemap,
        fileOpened: string,
        interactive: bool,
        sourceStream: TextIO.instream,
        content: string option ref,
        anyErrors: bool ref,
        errConsumer: PrettyPrint.device
      }

(* -- inactive diagnostic printing
  fun say (msg : string) = Control_Print.say msg
*)

  fun newSource(fileName, sourceStream, interactive, errConsumer) =
      {sourceMap=SourceMap.newSourceMap fileName,
       fileOpened=fileName,
       interactive=interactive,
       sourceStream=sourceStream,
       content=ref NONE,
       anyErrors=ref false,
       errConsumer=errConsumer}

  fun closeSource ({interactive=true, ...} : inputSource) = ()
    | closeSource ({sourceStream, ...}) = (
        (* app say ["[closing ", (Pathnames.trim fileName), "]\n"];*)
        TextIO.closeIn sourceStream handle IO.Io _ => ())

  fun filepos ({sourceMap,...}: inputSource) pos = SourceMap.filepos sourceMap pos

  fun getContent ({fileOpened,interactive,content,...}: inputSource) : string option =
      case !content
        of NONE =>
            if interactive then NONE
              (* need to capture history of interactive input *)
	    else (let val s = TextIO.inputAll(TextIO.openIn fileOpened)
		   in content := SOME s;
		      !content
		  end handle IO.Io _ => NONE)
         | s => s

  (* regionContent:
          inputSource * SourceMap.region
       -> (SourceMap.region * string * int) option *)
  fun regionContent (src as {sourceMap,...}: inputSource, region) =
      case getContent src
        of NONE => NONE
         | SOME content =>
           let val wideregion as (lo,hi) = SourceMap.widenToLines sourceMap region
	       val content = substring(content, lo-1, hi-lo)
	       val ({line,...},_)::_ = SourceMap.fileregion sourceMap wideregion
	    in SOME(content, wideregion, line)
	   end

  (* sourceName : inputSource -> string *)
  (* returns contents of fileOpened field *)
  fun sourceName ({fileOpened,...}: inputSource) = fileOpened

end (* structure Source *)
