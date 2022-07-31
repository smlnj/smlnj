(*
 * How to build primitive RTL operators 
 *)
signature RTL_BUILD =
sig
   structure T : MLTREE
   type ty    = T.ty
   type cond  = T.cond
   type fcond = T.fcond

   type effect 
   type region 
   type exp
   type bool
   type div_rounding_mode
   
   val intConst  : ty -> int -> exp          (* integer constant *)
   val wordConst : ty -> Word32.word -> exp  (* word constant *)
   val ???       : ty -> exp                 (* an undefined value *)

   val newOp     : string -> exp list -> exp   (* create new operator *)

   val immed     : ty -> exp -> exp          (* immediate value *)
   val operand   : ty -> exp -> exp          
   val label     : ty -> exp -> exp
   val $         : CellsBasis.cellkind * ty -> exp -> exp   
   val Mem       : CellsBasis.cellkind * ty -> exp * region -> exp
   val Arg       : ty * string * string -> exp

   (* Signed/unsigned promotion *)
   val sx  : ty * ty -> exp -> exp
   val zx  : ty * ty -> exp -> exp

   (* Integer operators *)
   val ~     : ty -> exp -> exp
   val +     : ty -> exp * exp -> exp
   val -     : ty -> exp * exp -> exp
   val muls  : ty -> exp * exp -> exp 
   val mulu  : ty -> exp * exp -> exp 
   val divs  : ty -> div_rounding_mode * exp * exp -> exp 
   val divu  : ty -> exp * exp -> exp 
   val rems  : ty -> div_rounding_mode * exp * exp -> exp 
   val remu  : ty -> exp * exp -> exp 

   val addt  : ty -> exp * exp -> exp 
   val subt  : ty -> exp * exp -> exp 
   val mult  : ty -> exp * exp -> exp 
   val divt  : ty -> div_rounding_mode * exp * exp -> exp 

   val notb  : ty -> exp -> exp
   val andb  : ty -> exp * exp -> exp 
   val orb   : ty -> exp * exp -> exp 
   val xorb  : ty -> exp * exp -> exp 
   val eqvb  : ty -> exp * exp -> exp 
   val <<    : ty -> exp * exp -> exp 
   val >>    : ty -> exp * exp -> exp 
   val ~>>   : ty -> exp * exp -> exp 
   val BitSlice : ty -> (int * int) list -> exp -> exp

   (* Boolean operators *)
   (* val Cond    : ty -> bool * exp * exp -> exp *)
   val False    : bool
   val True     : bool
   val Not      : bool -> bool
   val And      : bool * bool -> bool
   val Or       : bool * bool -> bool
   val Cond     : ty -> bool * exp * exp -> exp 

   (* Integer comparisons *)
   val ==      : ty -> exp * exp -> bool 
   val <>      : ty -> exp * exp -> bool 
   val <       : ty -> exp * exp -> bool 
   val >       : ty -> exp * exp -> bool 
   val <=      : ty -> exp * exp -> bool 
   val >=      : ty -> exp * exp -> bool 
   val ltu     : ty -> exp * exp -> bool 
   val leu     : ty -> exp * exp -> bool 
   val gtu     : ty -> exp * exp -> bool 
   val geu     : ty -> exp * exp -> bool 
   val setcc   : ty -> exp * exp -> bool
   val getcc   : ty -> exp * T.cond -> bool

   (* Floating point operators *)
   val fadd    : ty -> exp * exp -> exp
   val fsub    : ty -> exp * exp -> exp
   val fmul    : ty -> exp * exp -> exp
   val fdiv    : ty -> exp * exp -> exp
   val fcopysign : ty -> exp * exp -> exp
   val fabs    : ty -> exp -> exp
   val fneg    : ty -> exp -> exp
   val fsqrt   : ty -> exp -> exp

   (* Floating point comparisons *)
   val |?|     : ty -> exp * exp -> bool
   val |==|    : ty -> exp * exp -> bool
   val |?=|    : ty -> exp * exp -> bool
   val |<|     : ty -> exp * exp -> bool
   val |?<|    : ty -> exp * exp -> bool
   val |<=|    : ty -> exp * exp -> bool
   val |?<=|   : ty -> exp * exp -> bool
   val |>|     : ty -> exp * exp -> bool
   val |?>|    : ty -> exp * exp -> bool
   val |>=|    : ty -> exp * exp -> bool
   val |?>=|   : ty -> exp * exp -> bool
   val |<>|    : ty -> exp * exp -> bool
   val |<=>|   : ty -> exp * exp -> bool
   val |?<>|   : ty -> exp * exp -> bool
   val setfcc  : ty -> exp * exp -> bool
   val getfcc  : ty -> exp * T.fcond -> bool

   (* Effect combinators *)
   val :=    : ty -> exp * exp -> effect
   val Par   : effect * effect -> effect  (* parallel effects *)
   val Nop   : effect                     (* empty effect *)
   val Jmp   : ty -> exp -> effect        (* jump to address *)
   val Call  : ty -> exp -> effect        (* call address *)
   val Ret   : effect                     (* return *)
   val If    : bool * effect * effect -> effect (* if/then/else *)

   val map    : ty -> ('a -> 'b) -> 'a list -> 'b list

   val getNewOps   : unit -> T.Basis.misc_op list
   val clearNewOps : unit -> unit

end
