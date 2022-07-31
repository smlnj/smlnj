(*
 * Compile the machine description into an internal digestable form
 *)
signature MDL_COMPILE =
sig

   structure Ast       : MDL_AST
   structure Env       : MDL_ENV
   structure Util      : MDL_AST_UTIL
   structure Trans     : MDL_AST_TRANSLATION
   structure Consts    : MDL_AST_CONSTANTS
   structure Rewriter  : MDL_AST_REWRITER
   structure AstPP     : MDL_AST_PRETTY_PRINTER
   structure TypeUtils : MDL_TYPE_UTILS
   structure Error     : MDL_ERROR
      sharing Ast = Env.Ast = Util.Ast = Rewriter.Ast = Consts.Ast =
              Trans.Ast = AstPP.Ast = TypeUtils.Ast

   type md  (* machine description *)
   type filename = string

   (* Extract info from a machine description *)
   val endianess : md -> Ast.endianess    (* endianess *)
   val archKind  : md -> Ast.archKind     (* kind of architecture *)
   val asmCase   : md -> Ast.assemblycase (* assembly case *)
   val name      : md -> string           (* name of description *)
   val filename  : md -> string           (* filename *)
   val env       : md -> Env.env          (* environment *)
   val cells     : md -> Ast.storagedecl list 
(*
   val cellSets  : md -> Ast.storagedecl list (* all cellkinds with cellsets *)
   val cellSetsAliases : md -> Ast.storagedecl list (* include all aliases *)
 *)
   val locations : md -> Ast.locbind list 
   val formats   : md -> (int option * Ast.formatbind) list 
   val debugging : md -> string -> bool
   val lookupCellKind : md -> string -> Ast.storagedecl 
   val lookupDatatype : md -> string -> Ast.datatypebind 
   val hasCopyImpl : md -> bool

   val resources : md -> Ast.id list
   val pipelines : md -> Ast.pipelinebind list
   val cpus      : md -> Ast.cpubind list
   val latencies : md -> Ast.latencybind list

   (* Compile an AST into a machine description *)
   val compile   : filename * Ast.decl list -> md

   (* Extract info from the environment *)
   val declOf    : md -> string -> Ast.decl  (* body of structure *)
   val fctArgOf  : md -> string -> Ast.decl  (* functor argument *)
   val typeOf    : md -> string -> Ast.decl  (* type definitions *)  
   val instructions : md -> Ast.consbind list 

   (* Require the definitions of these things *)
   val require  : md -> string -> {values:Ast.id list,types:Ast.id list} 
                    -> unit

   (* Code generation functions *)
   type module = string
   type arguments = string list
   type signatureName = string

   val mkQueryByCellKind : md -> string -> Ast.decl
   val forallUserCellKinds : md -> (Ast.storagedecl -> 'a) -> 'a list

   val errorHandler : md -> string -> Ast.decl
   val signame   : md -> module -> string
   val strname   : md -> module -> string
   val mkCode    : Ast.decl list -> PP.pp 
   val mkStr     : md -> string -> signatureName -> Ast.decl list -> PP.pp
   val mkSig     : md -> module -> Ast.decl list -> PP.pp
   val mkFct     : md -> module -> arguments -> signatureName -> 
                    Ast.decl list -> PP.pp
   val mkFct'    : md -> module -> Ast.decl -> signatureName ->  
                    Ast.decl list -> PP.pp
   val outfile   : md -> module -> string -> string -> unit
   val pathName  : md -> module -> string -> string
   val codegen   : md -> module -> PP.pp list -> unit

end
