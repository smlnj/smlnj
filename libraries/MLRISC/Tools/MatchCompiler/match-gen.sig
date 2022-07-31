(*
 * Interface with the match compiler to generate ML code
 *)
signature MATCH_GEN =
sig
   structure Ast : MDL_AST
   structure MC  : MATCH_COMPILER
   structure LitMap : ORD_MAP where type Key.ord_key = Ast.literal 

   datatype conrep = CONREP of Ast.id list * Ast.consbind * Ast.datatypebind
                   | EXN of Ast.id list * Ast.id * Ast.ty option 
   
   structure Env :
   sig
      type env
      val insertSig  : env * Ast.id * env -> env
      val insertCons : env * Ast.id * conrep -> env
      val lookupSig  : env * Ast.id -> env option
      val lookupCons : env * Ast.id -> conrep option
      val empty      : env
   end

   type compiled_type_info = Env.env

   val init   : unit -> unit

   val compileTypes : Ast.decl list -> compiled_type_info

   val compile : compiled_type_info -> Ast.clause list -> MC.compiled_dfa

   val report : {warning : string -> unit,
                 error   : string -> unit, 
                 log     : string -> unit, 
                 dfa     : MC.compiled_dfa,
                 rules   : Ast.clause list
                } -> unit

   val codeGen : {root : Ast.exp,
                  dfa  : MC.compiled_dfa,
                  fail : unit -> Ast.exp,
                  literals : Ast.id LitMap.map ref 
                 } -> Ast.exp     

   val isComplex : Ast.clause list -> bool

end
