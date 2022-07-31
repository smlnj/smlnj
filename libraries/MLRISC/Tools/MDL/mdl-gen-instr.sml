
(*
 * Generate the <arch>Instr signature and functor.
 * This structure contains the definition of the instruction set.
 *)

functor MDLGenInstr(Comp : MDL_COMPILE) : MDL_GEN_MODULE =
struct

   structure Ast  = Comp.Ast
   structure Comp = Comp

   open Ast Comp.Util

   val toLower = String.map Char.toLower

   val instructionDatatype = 
   $["and instruction =",
     "  LIVE of {regs: C.cellset, spilled: C.cellset}",
     "| KILL of {regs: C.cellset, spilled: C.cellset}",
     "| COPY of {k: CellsBasis.cellkind, ",
     "           sz: int,          (* in bits *)", 
     "           dst: CellsBasis.cell list,",
     "           src: CellsBasis.cell list,", 
     "           tmp: ea option (* NONE if |dst| = {src| = 1 *)}",
     "| ANNOTATION of {i:instruction, a:Annotations.annotation}",
     "| INSTR of instr"
    ]

   fun gen md =
   let (* name of the structure/signature *)
       val strName = Comp.strname md "Instr"  
       val sigName = Comp.signame md "INSTR"

       (* The datatype that defines the instruction set *)
       val instructions = Comp.instructions md
       val instrDatatype =
           DATATYPEdecl([DATATYPE("instr",[],instructions)],[])

       (* Arguments to the instruction functor *)
       val args = ["T: MLTREE"] 

       (* the shorthand functions *)
       val instrTy = IDty(IDENT([],"instruction"))
       val shortHandSig = 
           map (fn CONSbind{id,ty=NONE,...} => 
                    VALSIGdecl([toLower id],instrTy) 
                 | CONSbind{id,ty=SOME ty, ...} =>
                    VALSIGdecl([toLower id],FUNty(ty,instrTy))) 
               instructions
       val shortHandFuns = 
           VALdecl(
           map (fn CONSbind{id,ty=NONE,...} => 
                     VALbind(IDpat(toLower id), APP("INSTR",ID id))
                 | CONSbind{id,ty=SOME _,...} => 
                     VALbind(IDpat(toLower id),
                         APP("o",TUPLEexp[ID "INSTR",ID id])))
               instructions)

       (* The signature *)
       val sigBody =
          [$ ["structure C : "^Comp.signame md "CELLS",
	      "structure CB : CELLS_BASIS = CellsBasis",
              "structure T : MLTREE",
              "structure Constant: CONSTANT",
              "structure Region : REGION",
              "   sharing Constant = T.Constant",
              "   sharing Region = T.Region"
              ],
           Comp.typeOf md "Instruction",
           instrDatatype,
           instructionDatatype
          ] @ shortHandSig

       (* The functor *)
       val strBody = 
           [$ ["structure C = "^Comp.strname md "Cells",
               "structure CB = CellsBasis",
               "structure T = T",
               "structure Region = T.Region",
               "structure Constant = T.Constant"
              ],
            Comp.declOf md "Instruction",
            instrDatatype,
            instructionDatatype,
            shortHandFuns
           ] 

       val _ = Comp.require md "Instruction"
                  {types =["ea","operand", "addressing_mode"],
                   values=[]
                  }

   in  Comp.codegen md "instructions/Instr"
         [Comp.mkSig md "INSTR" (map Comp.Trans.stripMarks sigBody),
          Comp.mkFct md "Instr" args sigName 
                (map Comp.Trans.stripMarks strBody)
         ]
   end
end