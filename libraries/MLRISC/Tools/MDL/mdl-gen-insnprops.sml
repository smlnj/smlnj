(*
 * Generate the <arch>Props functor.
 * This structure extracts information about the instruction set.
 *)

functor MDLGenInsnProps(RTLComp : MDL_RTL_COMP) : MDL_GEN_MODULE2 =
struct

   structure RTLComp  = RTLComp
   structure Comp     = RTLComp.Comp
   structure Ast      = Comp.Ast
   structure M        = RTLComp.MLRiscTypes

   open Ast Comp.Util

   val typeDefs = 
       $ ["(* classify instructions *)",
          "datatype kind = IK_JUMP   (* branches, including returns *)",
          "  | IK_NOP    (* no ops *)",
          "  | IK_INSTR  (* normal instructions *)",
          "  | IK_COPY   (* parallel copy *)",
          "  | IK_CALL   (* call instructions *)",
          "  | IK_PHI    (* A phi node (SSA) *)",
          "  | IK_SINK   (* A sink node (SSA) *)",
          "  | IK_SOURCE (* A source node (SSA) *)",
          "",
          "datatype target = LABELLED of Label.label",
          "                | FALLTHROUGH" ,
          "                | ESCAPES",
          "",
          "exception NegateCondtional",
          ""
         ]

   val funDefs = 
       $ ["fun getAnnotations(I.ANNOTATION{i,a}) =",
          "    let val (i,an) = getAnnotations i in (i,a::an) end",
          "  | getAnnotations i = (i,[])",
          "fun annotate(i,a) = I.ANNOTATION{i=i,a=a}"
         ]
 
   fun gen compiled_rtls =
   let val md      = RTLComp.md compiled_rtls

       (* name of the structure/signature *)
       val strName = Comp.strname md "Props"  
       val sigName = "INSN_PROPERTIES"

       (* The instructions *)
       val instructions = Comp.instructions md

       (* Arguments to the instruction functor *)
       val args =
           ["I : "^Comp.signame md "INSTR"
           ]

       (* Function that determines the type of an instruction *)
       val instrKind  = DUMMYfun "instrKind"

       (* Functions for dealing with parallel copies *)
       val moveInstr  = DUMMYfun "moveInstr"
       val moveTmpR   = DUMMYfun "moveTmpR"
       val moveDstSrc = DUMMYfun "moveDstSrc"

       val nop        = DUMMYfun "nop"
       val jump       = DUMMYfun "jump"

       val loadImmed  = DUMMYfun "loadImmed"

       val branchTargets = DUMMYfun "branchTargets"
       val setTargets    = DUMMYfun "setTargets"

       val negateConditional  = DUMMYfun "negateConditional"
       val immedRange    = DUMMYfun "immedRange"
       val loadOperand   = DUMMYfun "loadOperand"

       val eqOpn         = DUMMYfun "eqOpn"
       val hashOpn       = DUMMYfun "hashOpn"

       fun mkDefUse(cellKind as CELLdecl{id, ...}) = 
       let val {get, decl} = M.getOpnd
                [("int", M.IGNORE),
                 ("int32", M.IGNORE),
                 ("intinf", M.IGNORE),
                 ("word", M.IGNORE),
                 ("word32", M.IGNORE),
                 ("label", M.IGNORE),
                 ("cells", M.MULTI "x"),
                 ("cell", M.CONV "x"),
                 ("cellset", M.MULTI("C.cellSet.get C."^id^" x")),
                 ("operand", M.IGNORE) (* XXX *)
                ]

           fun defUse(x,exp,L) = 
               if M.ofCellKind(exp,cellKind) then SOME(get(x,exp,L))
               else NONE

       in  RTLComp.mkDefUseQuery compiled_rtls 
             {name="defUse"^id,
              decls=[decl],
              args=[["instr"]],
              namedArguments=false,
              def=defUse,
              use=defUse
             }
       end

       val defUseFuns = SEQdecl(Comp.forallUserCellKinds md mkDefUse)
       val defUse     = Comp.mkQueryByCellKind md "defUse"

       (* The functor *)
       val strBody = 
           [$ ["structure I  = I",
               "structure C  = I.C",
               "structure LE = LabelExp",
               "",
               "exception NegateConditional",
               ""
              ],
            Comp.errorHandler md "Props",
            typeDefs,
            instrKind,
            moveInstr,
            moveTmpR,
            moveDstSrc,
            nop,
            jump,
            loadImmed,
            branchTargets,
            setTargets,
            negateConditional,
            immedRange,
            loadOperand,
            eqOpn,
            hashOpn,
            defUseFuns,
            defUse,
            funDefs
           ]

   in  Comp.codegen md "instructions/Props2"
         [Comp.mkFct md "Props2" args sigName strBody
         ]
   end
end
