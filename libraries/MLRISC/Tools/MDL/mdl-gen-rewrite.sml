(*
 * Generate the <arch>Rewrite functor.
 * which performs register renaming.
 *)

functor MDLGenRewrite(RTLComp : MDL_RTL_COMP) : MDL_GEN_MODULE2 =
struct

   structure RTLComp = RTLComp
   structure Comp    = RTLComp.Comp
   structure Ast     = Comp.Ast
   structure Env     = Comp.Env
   structure Trans   = Comp.Trans
   structure RTL     = RTLComp.RTL
   structure T       = RTL.T
   structure C       = CellsBasis
   structure MLRisc  = RTLComp.MLRiscTypes

   open Ast Comp.Util Comp.Error

   (* Change these definitions if the cell type has changed *)
   fun howToRename cellkind = 
      $["fun rename r = if regmap r = rs then rt else r",
        "fun renamecellset cellset =",
        "    CellsBasis.CellSet.map C."^C.cellkindToString cellkind^" rename cellset"
       ]

   (* Main function *)
   fun gen compiled_rtls =
   let val md      = RTLComp.md compiled_rtls

        (* name of the structure/signature *)
       val strName = Comp.strname md "Rewrite"  
       val sigName = "REWRITE_INSTRUCTIONS"

       (* The instructions *)
       val instructions = Comp.instructions md

       (* The Instruction environment *)
       val env = Env.lookupStr (Comp.env md) (IDENT([],"Instruction"))

       (* Arguments to the instruction functor *)
       val args =
           ["Instr : "^Comp.signame md "INSTR"
           ]

       datatype defUse = DEF | USE

       (*
        * Make a rewrite function of type:
        *   regmap * instruction * fromReg * toReg -> instruction
        * 
        *)
       fun mkFun(funName, rwOpnd, cellKind, defUse) =
       let fun mkRewriteBody{instr, rtl, const} =
           let fun apply(f,x) = SOME(APP(f,ID x))
               fun rewrite(x,ty,T.$(_,c,_)) = 
                     if c = cellKind then apply("rename",x) else NONE
                 | rewrite(x,ty,T.ARG(_,ref(rep as T.REP k),_)) = 
                     if MLRisc.isConst rep then NONE 
                     else apply("rename"^k,x)
                 | rewrite(x,ty,_) = fail("bad argument "^x)
               fun nonRtlArg _ = NONE
               fun rtlArg(name, ty, exp, RTL.IN _) = 
                    if defUse = USE then rewrite(name,ty,exp) else NONE
                 | rtlArg(name, ty, exp, RTL.OUT _) =
                    if defUse = DEF then rewrite(name,ty,exp) else NONE
                 | rtlArg(name, ty, exp, RTL.IO _) = 
                    rewrite(name,ty,exp)
               val exp = RTLComp.mapInstr{instr=instr,
                                          rtl=rtl,
                                          nonRtlArg=nonRtlArg,
                                          rtlArg=rtlArg}
           in  {exp=exp, casePats=[]}
           end
           val decls = 
               [$["fun rewriteoperand opnd = "^rwOpnd^"(regmap,rs,rt,opnd)"
                 ],
                howToRename cellKind,
                RTLComp.simpleErrorHandler funName
               ]
       in  RTLComp.mkQuery compiled_rtls
             {name          = funName,
              namedArguments= false,
              args          = [["regmap","instr","rs","rt"]],
              decls         = decls,
              caseArgs      = [],
              body          = mkRewriteBody
             }
       end

       (* The functor *)
       val strBody = 
           [$ ["structure I  = Instr",
               "structure C  = I.C",
               ""
              ],
            Comp.errorHandler md "Rewrite",
            Comp.declOf md "Rewrite",
            mkFun("rewriteDef","rewriteOperandDef", C.GP, DEF),
            mkFun("rewriteUse","rewriteOperandUse", C.GP, USE),
            mkFun("frewriteDef","frewriteOperandDef", C.FP, DEF),
            mkFun("frewriteUse","frewriteOperandUse", C.FP, USE)
           ]

       val _ = Comp.require md "Rewrite"
                  {values=["rewriteOperandDef",
                           "rewriteOperandUse",
                           "frewriteOperandDef",
                           "frewriteOperandUse"],
                   types=[]
                  }

   in  Comp.codegen md "ra/Rewrite2"
         [Comp.mkFct md "Rewrite2" args sigName strBody
         ]
   end
end
