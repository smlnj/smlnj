signature MDL_GEN_MODULE =
sig

   structure Ast  : MDL_AST
   structure Comp : MDL_COMPILE
     sharing Ast = Comp.Ast

   val gen : Comp.md -> unit

end   

signature MDL_GEN_MODULE2 =
sig

   structure Ast      : MDL_AST
   structure Comp     : MDL_COMPILE
   structure RTLComp  : MDL_RTL_COMP
     sharing Ast = Comp.Ast = RTLComp.Ast 
     sharing Comp = RTLComp.Comp 

   val gen : RTLComp.compiled_rtls -> unit

end 

signature MDL_GEN =
sig
   val gen : string -> unit
end
