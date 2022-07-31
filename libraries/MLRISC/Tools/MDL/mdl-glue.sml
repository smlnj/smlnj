(*
 * This file just links everything together
 *)

structure MDLAstUtil = MDLAstUtil(MDLAst)

structure MDLPP = MDLAstPrettyPrinter(MDLAstUtil)

structure MDLTypeUtils = MDLTypeUtils(MDLPP)

structure MDLEnv = MDLEnv(MDLTypeUtils)

structure MDLRewriter = MDLAstRewriter(MDLAst)
structure MDLTrans = MDLAstTranslation
     (structure AstPP       = MDLPP
      structure AstRewriter = MDLRewriter
     )

structure MDLCompile = MDLCompile
   (structure AstPP    = MDLPP
    structure Env      = MDLEnv
    structure AstUtil  = MDLAstUtil
    structure Rewriter = MDLRewriter
    structure Trans    = MDLTrans
    structure Consts   = MDLAstConstants(MDLAst)
    structure AstUtil  = MDLAstUtil
    structure TypeUtils= MDLTypeUtils
   )

structure MDLTyping = MDLTyping
   (structure Env      = MDLEnv
    structure TypeUtil = MDLTypeUtils
    structure AstUtil  = MDLAstUtil
    structure AstPP    = MDLPP
    structure Comp     = MDLCompile
   )

structure MDLRTLTools = MDLRTLTools
   (structure AstUtil   = MDLAstUtil
    structure MLTreeRTL = MDLMLTreeRTL
   )

structure MLRiscTypes = MLRiscTypes
   (structure Comp = MDLCompile
    structure RTL  = MDLMLTreeRTL
   )

structure MDLRTLComp = MDLRTLComp
   (structure Comp = MDLCompile
    structure Typing = MDLTyping
    structure RTLTools = MDLRTLTools
    structure MLRiscTypes = MLRiscTypes
   )

structure MDLParser = 
   MDLParserDriver
      (structure AstPP = MDLPP
       val MDLmode = true
       open MDLAst
       fun newCell(id,nickname) = 
            CELLdecl{id=id,nickname=nickname,
                     from=ref 0,to=ref ~1, alias=NONE, count=NONE,
                     bits=0, 
                     print=LAMBDAexp[CLAUSE([WILDpat],NONE,
                                            LITexp(STRINGlit id))],
                     aggregable=false,
                     defaults=[]
                    }
       val extraCells = 
            [newCell("CELLSET","cellset")
            ]
      )

structure MDLGen = MDLGen
(  structure Comp       = MDLCompile
   structure Parser     = MDLParser
   structure Cells      = MDLGenCells(MDLCompile)
   structure Instr      = MDLGenInstr(MDLCompile)
   structure Shuffle    = MDLDummyGen(MDLCompile)
   structure Asm        = MDLGenAsm(MDLCompile)
   structure MC         = MDLGenMC(MDLCompile)
   structure Jumps      = MDLDummyGen(MDLCompile)
   structure Dasm       = MDLDummyGen(MDLCompile)
   structure Props      = MDLGenInsnProps(MDLRTLComp)
   structure Rewrite    = MDLGenRewrite(MDLRTLComp)
   structure RTLComp    = MDLRTLComp
   structure RTLProps   = MDLGenRTLProps(MDLRTLComp)
   structure SSAProps   = MDLGenSSAProps(MDLRTLComp)
  (* structure DelaySlots = MDLDelaySlots(MDLCompile)
   structure SchedProps = MDLSchedProps(MDLRTLComp)
   *)
)
