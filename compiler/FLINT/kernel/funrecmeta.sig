(* function and record metadata *)

signature FUN_REC_META =
sig
(* depends on Lty (for fflag and lty) *)
    
(* specifies what kind of inlining behavior is desired for the function *)
datatype ilhint
  = IH_SAFE				(* only inline if trivially size-safe *)
  | IH_ALWAYS			(* inline whenever possible *)
  | IH_UNROLL			(* only inline once within itself *)
(* call-site dependent inlining:
 *     #1 < sum (map2 (fn (a,w) => (known a) * w) (actuals, #2)
 *)
  | IH_MAYBE of int * int list

(* what kind of recursive function (aka loop) is this *)
(* the distinction between LK_LOOP and LK_UNKNOWN is not clear
 * and might get dropped so that we only need `tail:bool'
 *)
datatype loopkind
  = LK_UNKNOWN			(* something else *)
  | LK_LOOP				(* loop wrapped in a preheader *)
  | LK_TAIL				(* like LK_LOOP but tail-recursive *)

(* functor or ordinary function *)
datatype cconv
  = CC_FCT				(* it's a functor *)
  | CC_FUN of Lty.fflag			(* it's a function *)

(** classifying various kinds of functions *)
type fkind =
  {inline: ilhint,			(* when should it be inlined *)
   known : bool,			(* are all the call sites known *)
   cconv : cconv,			(* calling convention *)
   isrec : (Lty.lty list * loopkind) option} (* is it recursive *)

(* additional attributes for type abstractions *)
type tfkind = {inline: ilhint}

(** classifying various kinds of records *)
datatype rkind
  = RK_VECTOR of Lty.tyc   (* vector: all elements have same type *)
  | RK_STRUCT              (* module: elements may be polymorphic *)
  | RK_TUPLE               (* tuple: all fields are monomorphic *)

end (* signature FUN_REC_META *)
