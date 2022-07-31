(*
 * MLRISC specific things are abstracted out here in this module.
 *)

functor MLRiscDefs(Ast : MDL_AST) : MLRISC_DEFS =
struct
   structure Ast = Ast

   val predefinedKinds = ["GP","FP","CC","MEM","CTRL"]
   fun isPredefinedCellKind x = List.exists (fn k => x=k) predefinedKinds

   val pseudoKinds = ["MEM","CTRL","CELLSET"]
   fun isPseudoCellKind x = List.exists (fn k => x=k) pseudoKinds

end
