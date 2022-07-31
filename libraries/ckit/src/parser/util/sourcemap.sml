structure SourceMap : SOURCE_MAP = 
struct
  structure F = Format

  type charpos = int

  type region = charpos * charpos

  datatype location
    = LOC of
        {srcFile   : string,
	 beginLine : int,
	 beginCol  : int,
	 endLine   : int,
	 endCol    : int}
    | UNKNOWN

  datatype sourcemap
    = SOURCEMAP of
       { linePos  : charpos list ref,
         filePos  : {linePos : charpos list, 
                     line    : int,
                     srcFile : string} list ref,
         lineNum  : int ref}

  (* DBM: the filePos is a stack of records, but it doesn't get popped, so
   * it looks like filePos could just be a ref of the record *)

  fun newmap{srcFile} = SOURCEMAP
      { linePos = ref [1], (* this compensates for lex bug : yypos off by 2 *)
        filePos = ref [{linePos=[],line=1,srcFile=srcFile}],
        lineNum = ref 1
      }

  fun newline (SOURCEMAP{linePos,lineNum,...}) pos =
      (linePos := pos :: !linePos; lineNum := 1 + !lineNum)

  fun resynch (SOURCEMAP{linePos,filePos,lineNum,...}) {pos,srcFile,line} =
      (filePos := {linePos= !linePos,
                   line= !lineNum,
		   srcFile=
                   (case srcFile of 
		      SOME srcFile => srcFile
		    | NONE => 
			let val fpl = !filePos
			in case fpl of
			  nil => ""
			| x :: _ => #srcFile x
			end)
                  } :: !filePos;
       linePos := [pos];
       lineNum := line
      )

  fun parseDirective sourceMap (pos,directive) =
      let fun sep #" "  = true
            | sep #"\"" = true
            | sep #"#"  = true
            | sep #"\n" = true
            | sep _     = false
	  fun proc{line, srcOpt} = 
              (case Int.fromString line
                 of SOME line => 
                     resynch sourceMap {pos=pos,srcFile=srcOpt,line=line}
		  |  _ => newline sourceMap pos)
       in if Config.ParseControl.parseDirective then
	    case String.tokens sep directive
	      of ("line" :: line :: srcFile :: _) =>
		   proc{line=line, srcOpt=SOME srcFile}
	       | line::srcFile::_ => proc{line=line, srcOpt=SOME srcFile}
	       | line :: _ => proc{line=line, srcOpt=NONE}
	       | _ => newline sourceMap pos
	  else newline sourceMap pos
      end

  fun currPos(SOURCEMAP{linePos,...}) = hd (!linePos)


  fun location(SOURCEMAP{linePos,filePos,lineNum,...}) (x,y) =
  let fun findPos(p,currPos,currFile,pos::rest,filePos,line) =
           if p > pos then
              {srcFile=currFile,line=line,column=p - pos}
           else findPos(p,pos,currFile,rest,filePos,line-1)
        | findPos(p,currPos,currFile,[],{linePos,line,srcFile}::filePos,_) =
           findPos(p,currPos,#srcFile(hd filePos),linePos,filePos,line)
              (* NOTE: very confusing...
                  filePos stack contains previous line info and srcFile of current file *)
        | findPos(p,currPos,currFile,[],[],line) = 
            {srcFile=currFile,line=line,column=0}

      val {srcFile=currFile,...} = hd(!filePos)
      val {srcFile,line=l1,column=c1} = 
             findPos(x,x,currFile,!linePos,!filePos,!lineNum)
      val {srcFile,line=l2,column=c2} = 
             findPos(y,y,currFile,!linePos,!filePos,!lineNum)
  in  LOC{srcFile   = srcFile,
          beginLine = l1,
          beginCol  = c1,
          endLine   = l2,
          endCol    = c2
         }
  end

  (* return a string representing a location *)
  fun locToString UNKNOWN = "\"???\""
    | locToString (LOC{srcFile,beginLine,beginCol,endLine,endCol}) = let
	val srcFile = srcFile
	val p1line  = beginLine
	val p1pos   = beginCol
	val p2line  = endLine
	val p2pos   = endCol
	in
	  if (beginLine = endLine)
	    then if (p1pos < p2pos)
	      then F.format "\"%s\":%d.%d-%d" [
		  F.STR srcFile, F.INT p1line, F.INT p1pos, F.INT p2pos
		]
	      else F.format "\"%s\":%d.%d" [
		  F.STR srcFile, F.INT p1line, F.INT p1pos
		]
	    else F.format "\"%s\":%d.%d-%d.%d" [
		F.STR srcFile, F.INT p1line, F.INT p1pos,
		F.INT p2line, F.INT p2pos
	      ]
	end (* locToString *)

end


