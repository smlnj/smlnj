structure MDLError : MDL_ERROR =
struct

   val loc = ref SourceMapping.dummyLoc
   val errorCount   = ref 0
   val warningCount = ref 0

   fun init() = 
        (errorCount := 0; warningCount := 0; loc := SourceMapping.dummyLoc)

   fun status() =
   let fun pr(0,s,t) = ("no "^s,t)
         | pr(1,s,t) = ("one "^s,t)
         | pr(n,s,t) = (Int.toString n^" "^s^"s","are")
       val (e, t) = pr(!errorCount,"error","is")
       val (w, t) = pr(!warningCount,"warning",t)
   in  "There "^t^" "^e^" and "^w
   end
  
   val logFileName = ref "" 
   val logFileStream = ref NONE : TextIO.outstream option ref

   fun closeLogFile() =
       case !logFileStream of
         SOME s => 
           (TextIO.closeOut s; logFileStream := NONE; logFileName := "")
       | NONE => ()
   fun openLogFile filename =
       (closeLogFile();
        logFileName := filename;
        logFileStream := SOME(TextIO.openOut filename)
       )
   fun logfile() = !logFileName

   fun printToLog text = 
       case !logFileStream of
         NONE => ()
       | SOME s => TextIO.output(s,text)

   exception Error

   fun setLoc l = loc := l

   fun withLoc l f x =
   let val p = !loc
       (* val _ = print(SourceMapping.toString l^"\n") *)
       val _ = setLoc l
       val y = f x
   in  setLoc p;
       y
   end

   fun log msg = 
   let val text = msg^"\n"
   in  TextIO.output(TextIO.stdErr,text);
       printToLog text
   end

   fun error msg = (log(SourceMapping.toString (!loc)^": "^msg); 
                    errorCount := !errorCount + 1)
   fun errorPos(l, msg) = (setLoc l; error msg)
   fun warning msg = (log(SourceMapping.toString (!loc)^": warning: "^msg);
                      warningCount := !warningCount + 1) 
   fun warningPos(l, msg) = (setLoc l; warning msg)
   fun fail msg = (error msg; raise Error)
end
