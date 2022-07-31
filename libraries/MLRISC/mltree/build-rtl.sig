(*
 * Functions for building an RTL.
 *)
signature BUILD_RTL =
sig
   structure RTL : MLTREE_RTL
   type ty = int

   val map   : int -> ('a -> 'b) -> 'a list -> 'b list 

   val fetch : ty -> RTL.loc -> RTL.exp
   val :=    : ty -> RTL.loc * RTL.exp -> RTL.action
   val aggb  : ty * ty -> RTL.cell -> RTL.loc 
   val aggl  : ty * ty -> RTL.cell -> RTL.loc
   val idaggr: ty -> RTL.cell -> RTL.loc
   val copy  : ty -> 'a * 'a -> RTL.action
   val !     : ty * string * string -> RTL.exp
   val $     : string * ty -> RTL.exp -> RTL.cell 
   val $$    : string * ty -> RTL.exp * RTL.exp -> RTL.cell 

   val intConst   : ty -> int -> RTL.exp
   val wordConst  : ty -> Word32.word -> RTL.exp

   val newOp : string -> RTL.exp list -> RTL.exp
   val newCond : string -> RTL.exp list -> RTL.exp


   val immed   : ty -> RTL.exp -> RTL.exp
   val operand : ty -> RTL.exp -> RTL.exp
   val label   : ty -> RTL.exp -> RTL.exp
   val forall  : ty -> RTL.exp -> RTL.exp
   val ?       : ty -> RTL.exp

   val not   : RTL.cond -> RTL.cond
   val False : RTL.cond
   val True  : RTL.cond

   val sx    : ty * ty -> RTL.exp -> RTL.exp
   val zx    : ty * ty -> RTL.exp -> RTL.exp
   val bitslice : ty -> (int * int) list -> RTL.exp -> RTL.exp

   (* Integer operators *)
   val ~     : ty -> RTL.exp -> RTL.exp
   val +     : ty -> RTL.exp * RTL.exp -> RTL.exp
   val -     : ty -> RTL.exp * RTL.exp -> RTL.exp
   val muls  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val mulu  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val divs  : ty -> RTL.div_rounding_mode * RTL.exp * RTL.exp -> RTL.exp
   val divu  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val rems  : ty -> RTL.div_rounding_mode * RTL.exp * RTL.exp -> RTL.exp
   val remu  : ty -> RTL.exp * RTL.exp -> RTL.exp

   val andb  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val orb   : ty -> RTL.exp * RTL.exp -> RTL.exp
   val xorb  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val eqvb  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val notb  : ty -> RTL.exp -> RTL.exp
   val <<    : ty -> RTL.exp * RTL.exp -> RTL.exp
   val >>    : ty -> RTL.exp * RTL.exp -> RTL.exp
   val ~>>   : ty -> RTL.exp * RTL.exp -> RTL.exp  

   (* Trapping operators *)
   val addt  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val subt  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val mult  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val divt  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val remt  : ty -> RTL.exp * RTL.exp -> RTL.exp

   val cond  : ty -> RTL.cond * RTL.exp * RTL.exp -> RTL.exp

   (* Integer comparisons *)
   val ==    : ty -> RTL.exp * RTL.exp -> RTL.cond
   val <>    : ty -> RTL.exp * RTL.exp -> RTL.cond
   val >     : ty -> RTL.exp * RTL.exp -> RTL.cond
   val <     : ty -> RTL.exp * RTL.exp -> RTL.cond
   val <=    : ty -> RTL.exp * RTL.exp -> RTL.cond
   val >=    : ty -> RTL.exp * RTL.exp -> RTL.cond
   val ltu   : ty -> RTL.exp * RTL.exp -> RTL.cond
   val leu   : ty -> RTL.exp * RTL.exp -> RTL.cond
   val gtu   : ty -> RTL.exp * RTL.exp -> RTL.cond
   val geu   : ty -> RTL.exp * RTL.exp -> RTL.cond

   (* Floating point operators *)
   val fadd  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val fsub  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val fmul  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val fdiv  : ty -> RTL.exp * RTL.exp -> RTL.exp
   val fabs  : ty -> RTL.exp -> RTL.exp
   val fneg  : ty -> RTL.exp -> RTL.exp
   val fsqrt : ty -> RTL.exp -> RTL.exp

   (* Floating point comparisons *)
   val |?|     : ty -> RTL.exp * RTL.exp -> RTL.cond  
   val |!<=>|  : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |==|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |?=|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!<>|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!?>=|  : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |<|     : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |?<|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!>=|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!?>|   : ty -> RTL.exp * RTL.exp -> RTL.cond
   val |<=|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |?<=|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!>|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!?<=|  : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |>|     : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |?>|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!<=|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!?<|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |>=|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |?>=|   : ty -> RTL.exp * RTL.exp -> RTL.cond
   val |!<|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!?=|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |<>|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!=|    : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |!?|    : ty -> RTL.exp * RTL.exp -> RTL.cond
   val |<=>|   : ty -> RTL.exp * RTL.exp -> RTL.cond 
   val |?<>|   : ty -> RTL.exp * RTL.exp -> RTL.cond

   (* Action combinators *)
   val ||    : RTL.action * RTL.action -> RTL.action   (* parallel RTL.actions *)
   val Nop   : RTL.action                    (* empty RTL.action *)
   val Jmp   : int -> RTL.exp -> RTL.action    (* jump to address *)
   val Call  : int -> RTL.exp -> RTL.action    (* call address *)
   val Ret   : RTL.action                    (* return *)
   val If    : RTL.cond * RTL.action * RTL.action -> RTL.action (* if/then/else *)
end
