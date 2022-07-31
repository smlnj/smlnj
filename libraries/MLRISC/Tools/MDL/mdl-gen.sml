(*
 * Generate machine code modules from machine description
 *)
functor MDLGen
   (structure Comp       : MDL_COMPILE
    structure Parser     : MDL_PARSER_DRIVER
    structure Cells      : MDL_GEN_MODULE
    structure Shuffle    : MDL_GEN_MODULE
    structure Instr      : MDL_GEN_MODULE
    structure Asm        : MDL_GEN_MODULE
    structure MC         : MDL_GEN_MODULE
    structure RTLComp    : MDL_RTL_COMP
    structure Jumps      : MDL_GEN_MODULE
    structure Dasm       : MDL_GEN_MODULE
    structure Props      : MDL_GEN_MODULE2
    structure Rewrite    : MDL_GEN_MODULE2
    structure RTLProps   : MDL_GEN_MODULE2
    structure SSAProps   : MDL_GEN_MODULE2
  (*structure DelaySlots : MD_GEN_MODULE
    structure SchedProps : MD_GEN_MODULE2 *)
      sharing Comp = 
              Cells.Comp = 
              Shuffle.Comp =
              Instr.Comp = 
              Asm.Comp = 
              MC.Comp = 
              Dasm.Comp = 
              Props.Comp =
              Jumps.Comp = 
              Rewrite.Comp =
              RTLComp.Comp =
              RTLProps.Comp (* =
              DelaySlots.Comp =  
              SSAProps.Comp =
              SchedProps.Comp*)
      sharing Parser.Ast = Comp.Ast
      sharing RTLComp = Rewrite.RTLComp = 
              RTLProps.RTLComp = SSAProps.RTLComp = Props.RTLComp
   ) : MDL_GEN =
struct

   fun doIt f x = 
       if !Comp.Error.errorCount = 0 then 
          (f x handle Comp.Error.Error => ()) 
       else ()

   (* Generate code! *)
   fun codegen md =
      (Comp.Error.openLogFile(Comp.pathName md "" ".log");
       Cells.gen md;
       Instr.gen md;
       Shuffle.gen md;
       Asm.gen md;
       MC.gen md;
       Dasm.gen md;
       Jumps.gen md; 
       (* DelaySlots.gen md; *)
       (*
       let val compiled_rtls = RTLComp.compile md
       in  doIt RTLComp.gen compiled_rtls;
           doIt Rewrite.gen compiled_rtls;
           doIt Props.gen compiled_rtls;
           doIt RTLProps.gen compiled_rtls;
           doIt SSAProps.gen compiled_rtls; 
           doIt SchedProps.gen compiled_rtls; 
           RTLComp.dumpLog compiled_rtls
       end;  *)
       Comp.Error.log(Comp.Error.status());
       Comp.Error.closeLogFile()
      )

   fun gen file = 
       (print("[Processing "^file^"]\n");
        Comp.Error.init();
        codegen(Comp.compile(file,Parser.load file)) (* build ast *)
       )

   fun exit() = if !Comp.Error.errorCount > 0 then 
                     OS.Process.failure 
                else OS.Process.success 

end
