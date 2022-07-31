structure GenFile : GENERATE_FILE =
struct

   open MDLError

   val bufsize = 1024*1024
           
   fun gen {trans, program, fileSuffix} (_, [infile]) = 
       (let val _ = init()
            val {base, ext} = OS.Path.splitBaseExt infile
            val outfile = OS.Path.joinBaseExt{base=base, ext=SOME fileSuffix}
            val _ = if infile = outfile then
                       fail("input and output file the same name!")
                    else
                       ()
            val text = trans infile
            fun changed() =
            let val s = TextIO.openIn outfile
                val t = TextIO.inputN(s, bufsize)
            in  TextIO.closeIn s;
                t <> text
            end handle _ => true 
        in  if !errorCount > 0 then
               (print("[Result not written to "^outfile^"]\n"); 1)
            else if changed() then
               (print("[Generating "^outfile^"]\n");
                let val s = TextIO.openOut outfile
                in TextIO.output(s, text);
                   TextIO.closeOut s;
                   0
                end
               )
            else
               (print("[No change to "^outfile^"]\n"); 0)
        end
        handle Error => 1
             | exn => fail("Uncaught exception "^exnName exn)
       )
     | gen {program,...} _ = fail("usage: "^program^" <filename>")
      

end
