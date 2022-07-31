structure SourceMapping :> SOURCE_MAPPING =
struct

  type charpos = int

  type region = charpos * charpos

  datatype location = LOC of {srcFile   : UniqueSymbol.symbol,
                              beginLine : int,
                              beginCol  : int,
                              endLine   : int,
                              endCol    : int
                             }
  datatype state = STATE of {lineNum : int,
                             file    : UniqueSymbol.symbol, 
                             charPos : charpos
                            }

  datatype sourcemap = SOURCEMAP of
          { linePos  : charpos list ref,
            filePos  : {linePos:charpos list, 
                        line   :int,
                        srcFile:UniqueSymbol.symbol} list ref,
            lineNum  : int ref
          }

  val dummyLoc = LOC{srcFile=UniqueSymbol.fromString "???", 
                     beginLine=1,beginCol=1,
                     endLine=1,endCol=1}
  fun newmap{srcFile} = SOURCEMAP
         { linePos = ref [0],
           filePos = ref [{linePos=[],line=1,
                           srcFile=UniqueSymbol.fromString srcFile}],
           lineNum = ref 1
         }

  fun newline (SOURCEMAP{linePos,lineNum,...}) pos =
      (linePos := pos :: !linePos; lineNum := 1 + !lineNum)

  fun state(SOURCEMAP{linePos,lineNum,filePos, ...}) =
      let val {srcFile, ...} = hd(! filePos)
          val charPos        = hd(! linePos)
          val lineNum        = ! lineNum
      in  STATE{file=srcFile, charPos=charPos, lineNum=lineNum} end

  fun resynch (SOURCEMAP{linePos,filePos,lineNum,...}) {pos,srcFile,line} =
      (filePos := {linePos= !linePos,
                   line= !lineNum,
                   srcFile=UniqueSymbol.fromString srcFile
                  } :: !filePos;
       linePos := [pos];
       lineNum := line
      )
  fun reset srcMap (STATE{file, lineNum, charPos}) =
     (print(UniqueSymbol.toString file^" "^Int.toString lineNum^"\n");
      resynch srcMap {pos=charPos,
                      srcFile=UniqueSymbol.toString file, line=lineNum} 
     )

  fun parseDirective sourceMap (pos,directive) =
      let fun sep #" "  = true
            | sep #"\"" = true
            | sep #"#"  = true
            | sep #"\n" = true
            | sep _     = false
      in  case String.tokens sep directive of
            line::srcFile::_ =>
             (case Int.fromString line of
                 SOME line => 
                    resynch sourceMap {pos=pos,srcFile=srcFile,line=line}
              |  _ => newline sourceMap pos
             )
         | _ => newline sourceMap pos
      end

  fun currPos(SOURCEMAP{linePos,...}) = hd (!linePos)

  fun location(SOURCEMAP{linePos,filePos,lineNum,...}) (x,y) =
  let fun findPos(p,currPos,currFile,pos::rest,filePos,line) =
           if p > pos then
              {srcFile=currFile,line=line,column=p - pos}
           else findPos(p,pos,currFile,rest,filePos,line-1)
        | findPos(p,currPos,currFile,[],{linePos,line,srcFile}::filePos,_) =
           findPos(p,currPos,srcFile,linePos,filePos,line)
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

  fun toString(LOC{srcFile,beginLine,beginCol,endLine,endCol}) =
  let val int = Int.toString
  in  UniqueSymbol.toString srcFile^":"^int beginLine^"."^int beginCol^
           (if beginLine = endLine andalso beginCol = endCol then ""
            else "-"^int endLine^"."^int endCol)
  end

  fun directive(LOC{srcFile,beginLine,beginCol,endLine,endCol}) =
  let val int = Int.toString
  in  "(*#line "^int beginLine^"."^int beginCol^" \""^
        UniqueSymbol.toString srcFile^"\"*)"
  end

end

