(*
 * Generate the <arch>SSAProps functor.
 * This structure extracts semantics and dependence 
 * information about the instruction set needed for SSA optimizations.
 *)

functor MDLGenSSAProps(RTLComp : MDL_RTL_COMP) : MDL_GEN_MODULE2 =
struct

   structure RTLComp = RTLComp
   structure Comp    = RTLComp.Comp
   structure Ast     = Comp.Ast
   structure Env     = Comp.Env
   structure RTL     = RTLComp.RTL
   structure T       = RTL.T
   structure C       = CellsBasis
   structure M       = RTLComp.MLRiscTypes

   open Ast Comp.Util Comp.Error

     (* Insert copies *)

   fun copyFuns hasImpl = 
   let val (implInit,implPat,implCopy) = 
              if hasImpl then
                 ("impl=ref NONE,","impl,", "impl=impl,")
              else 
                 ("", "", "")
   in
    $["fun copies cps =",
      "let fun f([],id,is,fd,fs) = (id,is,fd,fs)",
      "      | f({dst,src}::cps,id,is,fd,fs) =",
      "        if C.sameColor(dst,src) then f(cps,id,is,fd,fs)",
      "        else case C.cellkind dst of",
      "             C.GP   => f(cps,dst::id,src::is,fd,fs)",
      "          |  C.FP   => f(cps,id,is,dst::fd,src::fs)",
      "          |  C.MEM  => f(cps,id,is,fd,fs)",
      "          |  C.CTRL => f(cps,id,is,fd,fs)",
      "          |  kind   => error(\"copies: \"^C.cellkindToString kind^",
      "                             \" dst=\"^C.toString dst^",
      "                             \" src=\"^C.toString src)",
      " val (id,is,fd,fs) = f(cps,[],[],[],[])",
      " val icopy = case id of",
      "               []  => []",
      "             | [_] => [I.COPY{src=is,dst=id,"^implInit^"tmp=NONE}]",
      "             | _   => [I.COPY{src=is,dst=id,"^implInit,
      "                              tmp=SOME(I.Direct(C.newReg()))}]",
      " val fcopy = case fd of",
      "               []  => []",
      "             | [_] => [I.FCOPY{src=fs,dst=fd,"^implInit^"tmp=NONE}]",
      "             | _   => [I.FCOPY{src=fs,dst=fd,"^implInit,
      "                               tmp=SOME(I.FDirect(C.newFreg()))}]",
      "in icopy @ fcopy end"
     ]
   end

   (* Expressions building utilities *)
   fun consexp(x,LISTexp(a,b)) = LISTexp(x::a,b)
     | consexp(x,y) = LISTexp([x],SOME y)
   val nilexp = LISTexp([],NONE)
   fun conspat(x,LISTpat(a,b)) = LISTpat(x::a,b)
     | conspat(x,y) = LISTpat([x],SOME y)
   val nilpat = LISTpat([],NONE)

   fun gen compiled_rtls =
   let (* The machine description *)
       val md = RTLComp.md compiled_rtls

       (* name of the structure/signature *)
       val strName = Comp.strname md "SSAProps"  
       val sigName = "MLRISC_SSA_PROPERTIES"
 
       (* query function *)
       val mkQuery = RTLComp.mkQuery compiled_rtls

       fun In x = "in_"^x
       fun Out x = "out_"^x


       (* Function for extracting naming constraints from an RTL *)
       val namingConstraints =
       let 
           fun body{instr,rtl,const} = 
           let fun ignore p = conspat(WILDpat,p)
               fun cell(k,r) = 
                   const(
                      APPexp(APPexp(IDexp(IDENT(["C"],"Reg")),
                          IDexp(IDENT(["C"],C.cellkindToString k))),
                          INTexp(IntInf.toInt r)))

               fun addSrc(id,r,(d,u,C)) = 
                     (d,
                      conspat(IDpat(In id),u),
                      APP("USE",RECORDexp[("var",ID(In id)),("color",r)])::C
                     ) 

               fun addDst(id,r,(d,u,C)) = 
                     (conspat(IDpat(Out id),d),
                      u,
                      APP("DEF",RECORDexp[("var",ID(Out id)),("color",r)])::C
                     ) 

               fun addDstSrc(id,(d,u,C)) = 
                     (conspat(IDpat(Out id),d),
                      conspat(IDpat(In id),u),
                      APP("SAME",RECORDexp[("x",ID(Out id)),("y",ID(In id))])::
                          C
                     )

               fun ignoreUse(d,u,C) = (d, conspat(WILDpat,u), C)

               fun ignoreDef(d,u,C) = (conspat(WILDpat,d), u, C)
 
               fun f(id,ty,T.$(_,k,T.LI r),RTL.IN _,x) = 
                      addSrc(id,cell(k,r),x)
                 | f(id,ty,T.$(_,k,T.LI r),RTL.OUT _,x) = 
                      addDst(id,cell(k,r),x)
                 | f(id,ty,_,RTL.IO _,x) = addDstSrc(id, x)
                 | f(id,ty,_,RTL.IN _,x) = ignoreUse x
                 | f(id,ty,_,RTL.OUT _,x) = ignoreDef x

               fun g(id,ty,x) = x

               val (d,u,C) = 
                 RTLComp.forallArgs
                  {instr=instr,rtl=rtl,rtlArg=f,nonRtlArg=g} (nilpat,nilpat,[])
           in  {exp=LISTexp(C,NONE), casePats=[d,u]}
           end
    
           val decls=[RTLComp.complexErrorHandler "namingConstraints",
                      $["val dst_list = dst and src_list = src"]
                     ]
       in  mkQuery
             {name           = "namingConstraints",   
              namedArguments = true,
              args           = [["instr","src","dst"]],
              caseArgs       = ["dst_list","src_list"],
              decls          = decls,
              body           = body
             }
       end

       (* Function for rewriting the operands of an RTL *)
       val substituteOperands =
       let fun body {instr,rtl,const} = 
           let fun Ignore p = conspat(WILDpat, p)

               fun add(RTL.IN _,x,d,u)  = (d,conspat(IDpat(In x),u))
                 | add(RTL.OUT _,x,d,u) = (conspat(IDpat(Out x),d),u)
                 | add(RTL.IO _,x,d,u) = (conspat(IDpat(Out x),d),Ignore u)

               fun nochange(d,u) = (Ignore d,Ignore u)

               fun f(id,ty,T.$(_,k,T.LI r),pos,(d,u)) = nochange(d,u)
                 | f(id,ty,exp,pos,(d,u)) = add(pos,id,d,u)

               fun g(id,ty,(d,u)) = (Ignore d,Ignore u)

               fun arg(T.$(_,k,_),name) = 
                    if C.cellkindToString k = "CELLSET" then NONE
                    else SOME(ID name)
                 | arg(T.ARG _,name) = SOME(APP("get_operand",ID name))

               fun f'(id,ty,T.$(_,k,T.LI r),pos) = NONE
                 | f'(id,ty,exp,RTL.IN _) = arg(exp,In id)
                 | f'(id,ty,exp,RTL.OUT _) = arg(exp,Out id)
                 | f'(id,ty,exp,RTL.IO _) = arg(exp,Out id)

               fun g' _ = NONE

               val (d, u) =
                   RTLComp.forallArgs{instr=instr,rtl=rtl,rtlArg=f,nonRtlArg=g}
                     (nilpat,nilpat)
               val exp = 
                  RTLComp.mapInstr{instr=instr,rtl=rtl,rtlArg=f',nonRtlArg=g'} 
           in  {exp=exp, casePats=[d, u]}
           end
                              
           val decls=[RTLComp.complexErrorHandler "substituteOperands",
                      $["fun get_operand x = error \"get_operand\"",
                        "val dst_list = dst and src_list = src"
                       ]
                     ]
       in  mkQuery
             {name           = "substituteOperands",
              namedArguments = true,
              args           = [["const"],["instr","dst","src"]],
              caseArgs       = ["dst_list","src_list"],
              decls          = decls,
              body           = body
             }
       end

       (* Arguments to the instruction functor *)
       val args =
           ["structure Instr : "^Comp.signame md "INSTR",
            "structure RegionProps : REGION_PROPERTIES ",
            "structure RTLProps : RTL_PROPERTIES where I = Instr",
            "structure InsnProps : INSN_PROPERTIES where I = Instr",
            "structure Asm : INSTRUCTION_EMITTER where I = Instr", 
            "structure OperandTable : OPERAND_TABLE where I = Instr",
            "  sharing RegionProps.Region = Instr.Region",
            "val volatile     : Instr.C.cell list",
            "val pinnedDef    : Instr.C.cell list",
            "val pinnedUse    : Instr.C.cell list",
            "val dedicatedDef : Instr.C.cell list",
            "val dedicatedUse : Instr.C.cell list"
           ]

       (* The functor *)
       val strBody = 
           [$ ["structure I         = Instr",
               "structure C         = I.C",
               "structure RTLProps  = RTLProps",
               "structure InsnProps = InsnProps",
               "structure RTL       = RTLProps.RTL",
               "structure T         = RTL.T",
               "structure OT        = OperandTable",
               "structure RP        = RegionProps",
               "",
               "datatype const = datatype OT.const",
               "datatype constraint =",
               "  DEF  of {var:C.cell, color:C.cell}",
               "| USE  of {var:C.cell, color:C.cell}",
               "| SAME of {x:C.cell, y:C.cell}",
               ""
              ],
            Comp.errorHandler md "SSAProps",
            RTLComp.complexErrorHandlerDef (),
            $ ["",
               "val volatile = volatile",
               "val dedicatedDef = dedicatedDef",
               "val dedicatedUse = dedicatedUse",
               "val pinnedDef  = pinnedDef",
               "val pinnedUse  = pinnedUse",
               "val source = I.SOURCE{}",
               "val sink   = I.SINK{}",
               "val phi    = I.PHI{}",
               ""
              ],
            namingConstraints,
            substituteOperands,
            copyFuns (Comp.hasCopyImpl md),
            Comp.declOf md "SSA"
           ]

   in  Comp.codegen md "SSA/SSAProps"
         [Comp.mkFct md "SSAProps" args sigName 
              strBody
             (* (map Comp.Trans.simplifyDecl strBody) *)
         ]
   end

end
