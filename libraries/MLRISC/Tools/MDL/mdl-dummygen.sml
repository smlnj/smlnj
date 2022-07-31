(*
 * Placeholder for undefined modules
 *)
functor MDLDummyGen(Comp : MDL_COMPILE) : MDL_GEN_MODULE =
struct
   structure Ast  = Comp.Ast
   structure Comp = Comp
   fun gen _ = ()
end
