(*
 * This signature describes the internal RTL representation.
 * The internal representation differs from the user representation it that
 * it is lambda-lifted, i.e., instead of having references like REG(32,123),
 * it has references like PARAM i, which refers to the ith parameter.
 * 
 * This representation is chosen so that multiple instructions can
 * share the same rtl template.  Also, so that the templates can be
 * created once before compilation begins. 
 *)
signature MLTREE_RTL =
sig
   structure T       : MLTREE
   structure Util    : MLTREE_UTILS 
   structure Rewrite : MLTREE_REWRITE
   structure Fold    : MLTREE_FOLD
      sharing Util.T = Rewrite.T = Fold.T = T

   type ty   = T.ty
   type var  = T.var
   type rtl  = T.stm
   type exp  = T.rexp
   type cond = T.ccexp
   type div_rounding_mode = T.div_rounding_mode

   datatype pos = IN of int | OUT of int | IO of int * int (* def/use *)

   (*-----------------------------------------------------------------------
    * Basic Operations
    *-----------------------------------------------------------------------*)
   val showRTL : {def:int->string, 
                  use:int->string, 
                  regionDef:T.Region.region->string, 
                  regionUse:T.Region.region->string} -> T.printer
   val rtlToString : rtl -> string
   val expToString : exp -> string
   val hashRTL     : rtl -> word
   val eqRTL       : rtl * rtl -> bool 

   (*-----------------------------------------------------------------------
    * Construction 
    *-----------------------------------------------------------------------*)
   val newOp  : {name:string, attribs:T.Basis.attribs} -> T.Basis.misc_op
   val new    : rtl -> rtl 
   val pin    : rtl -> rtl
   val COPY   : rtl
   val JMP    : rtl 

   (*-----------------------------------------------------------------------
    * Type queries 
    *-----------------------------------------------------------------------*)
   val isConditionalBranch : rtl -> bool
   val isJump              : rtl -> bool
   val isLooker            : rtl -> bool

   (*-----------------------------------------------------------------------
    * Def/use queries.
    *-----------------------------------------------------------------------*)
   val defUse              : rtl -> exp list * exp list

   (*-----------------------------------------------------------------------
    * Assign positions to all arguments
    *-----------------------------------------------------------------------*)
   val argPos              : rtl -> (exp * int) list * (exp * int) list
   exception NotAnArgument
   val argOf               : rtl -> string -> (exp * pos)

   (*-----------------------------------------------------------------------
    * Number of arguments that an rtl maps into
    *-----------------------------------------------------------------------*)
   datatype arity = ZERO | ONE | MANY
   val arity               : exp -> arity (* number of values *)
   val nonConstArity       : exp -> arity (* number of non-constant values *)
 
   (*-----------------------------------------------------------------------
    * Extract naming constraints, if any
    *-----------------------------------------------------------------------*)
   val namingConstraints   : exp list * exp list ->
       { fixedDefs  : (exp * int) list, (* these define fixed locations *)
         fixedUses  : (exp * int) list, (* these define fixed locations *)
         twoAddress : exp list          (* these are both src and dst *)
       }

   (*-----------------------------------------------------------------------
    * Code motion queries 
    *-----------------------------------------------------------------------*)
   val can'tMoveUp    : rtl -> bool
   val can'tMoveDown  : rtl -> bool
   val pinned         : rtl -> bool
   val hasSideEffect  : rtl -> bool
   val can'tBeRemoved : rtl -> bool

end 
