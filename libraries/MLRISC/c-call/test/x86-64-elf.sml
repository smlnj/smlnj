structure X86_64Elf =
  struct

  (* get all the bytes off the input stream *)
    fun input inStrm = let	    
	    fun lp (line, lines) = 
		  if BinIO.endOfStream inStrm
		  then List.rev lines
		  else lp (BinIO.inputN(inStrm, 8), line :: lines)
	    val lines = lp(BinIO.inputN(inStrm, 8), [])
            in
	        BinIO.closeIn inStrm;
		lines
	    end

  (* input the entire file *)
    val inputFile = input o BinIO.openIn 

  (* extract the text segment from an obj file *)
    fun extractTextSegment bytes = let
	    val bytesBeforeInsn = List.take(bytes, 8)
	    val insn = List.nth(bytes, 8)
	    val bytesAfterInsn = List.drop(bytes, 9)
            in
	       (bytesBeforeInsn, insn, bytesAfterInsn)
	    end

  (* send a list of bytes to the output stream *)
    fun output (bytes, outStrm) = let
	    fun lp [] = BinIO.closeOut outStrm
	      | lp (l :: ls) = (
		  BinIO.output(outStrm, l);
		  lp ls)
            in
	        lp bytes
	    end

  (* take a byte-encoding of an instruction and pad it out to 8 bytes *)
    fun encodeInstr bs = 
	  Word8Vector.fromList(bs @ List.tabulate(8-(List.length bs), fn _ => 0wx00))

    fun instrToElf (instr, obj) = let
	(* copy in bytes from the object file *)
	  val bytes = inputFile obj 
	  val (bs1, _, bs2) = extractTextSegment bytes	  
          in
	      bs1 @ [encodeInstr instr] @ bs2
          end

  (* copy bytes to an object file *)
    fun outputObjFile (objOut, objBytes) = output(objBytes, BinIO.openOut objOut)

  (* takes an elf object file and an instruction, and outputs that instruction to its own elf object file *)
    fun outputInstrToObj (elfObj, instr) =
	  outputObjFile("out."^elfObj, instrToElf(instr, elfObj))

    fun doit () = outputInstrToObj("asm1.o", [0wxff, 0wxc2])

  end
