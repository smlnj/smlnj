(*
 * Process rtl descriptions
 *)
signature MDL_RTL_COMP =
sig
   structure Ast  : MDL_AST
   structure Comp : MDL_COMPILE
   structure RTL  : MLTREE_RTL
   structure MLRiscTypes : MLRISC_TYPES
     sharing Comp.Ast = MLRiscTypes.Ast = Ast
     sharing MLRiscTypes.RTL = RTL

   type compiled_rtls (* rtl in digested form *)

   datatype rtl_def = RTLDEF of {id:Ast.id,args:Ast.id list,rtl:RTL.rtl}

   (* current set of rtls *)
   val current_rtls : rtl_def list ref

   (* process the rtl *)
   val compile : Comp.md -> compiled_rtls

   (* extract the md component *)
   val md      : compiled_rtls -> Comp.md

   (* extract the rtls *)
   val rtls    : compiled_rtls -> rtl_def list

   (* emit and execute code for building the rtls *)
   val gen     : compiled_rtls -> unit 

   (* dump out all the rtl definitions *)
   val dumpLog : compiled_rtls -> unit 

   (* A generic combinator for generate query functions.
    * Use this method if you want to create new query routines.
    *)
   val mkQuery : compiled_rtls ->
       { name     : Ast.id,           (* name of function *)
         namedArguments : bool,       (* use record arguments? *)
         args     : Ast.id list list, (* labeled arguments, may be curried *)
         decls    : Ast.decl list,    (* local definitions *)
         caseArgs : Ast.id list,      (* extra arguments to the case exp *)
                 (* Callback to generate the actual code. 
                  * An instruction constructor may represent a set of
                  * different instructions with different rtl templates.
                  * We enumerate all of them and let you decide 
                  * how to generate the code.
                  *)
         body  : {instr : Ast.consbind,      (* current instruction *)
                  rtl   : rtl_def,           (* rtl for this instruction *)
                  const : Ast.exp -> Ast.exp (* callback for making constants*)
                 } ->
                 { casePats : Ast.pat list, (* extra patterns *) 
                   exp      : Ast.exp       (* and clause *)
                 }
       } -> Ast.decl

  (*
   * A generic routine for generating def/use like queries
   *)
  val mkDefUseQuery : compiled_rtls ->
       { name  : Ast.id,           (* name of function *)
         args  : Ast.id list list,
         namedArguments : bool,
         decls : Ast.decl list,    (* local definitions *)
         def   : Ast.exp * RTL.exp * Ast.exp -> Ast.exp option,
         use   : Ast.exp * RTL.exp * Ast.exp -> Ast.exp option
       } -> Ast.decl



  (*
   * Analyze all the arguments in an expression according to its 
   * rtl definition.
   *)
  val forallArgs : {instr     : Ast.consbind, (* current instruction *)
                    rtl       : rtl_def,      (* current rtl *)
                    rtlArg    : Ast.id * Ast.ty * RTL.exp * RTL.pos * 'a -> 'a,
                    nonRtlArg : Ast.id * Ast.ty * 'a -> 'a
                   } -> 'a -> 'a
  (*
   * Analyze all the arguments in an expression according to its 
   * rtl definition, create an expression that recreate that instruction.
   *)
  val mapInstr : {instr     : Ast.consbind, (* current instruction *)
                  rtl       : rtl_def,      (* current rtl *)
                  rtlArg    : Ast.id * Ast.ty * RTL.exp * RTL.pos -> 
                                 Ast.exp option,
                  nonRtlArg : Ast.id * Ast.ty -> Ast.exp option
                 } -> Ast.exp
  (*
   * Make an error handler 
   *)
  val simpleErrorHandler : string -> Ast.decl
  val complexErrorHandler : string -> Ast.decl
  val complexErrorHandlerDef : unit -> Ast.decl

end
