(* FLINT/kernel/funrecmeta.sml *)

(**************************************************************************
 *                    FUNCTION/RECORD METADATA TYPES                      *
 **************************************************************************)
(* these types were moved here from FLINT/flint/flint.sml to avoid having LtyExtern
 * depend on FLINT. They are used in FLINT and LtyExtern. *)

structure FunRecMeta : FUN_REC_META =
struct
(* depends on Lty *)

(* what kind of inlining behavior is desired for the function *)
datatype ilhint
  = IH_SAFE			(* only if trivially size-safe *)
  | IH_ALWAYS			(* inline whenever possible *)
  | IH_UNROLL			(* only inline once within itself *)
  | IH_MAYBE of int * int list
    (* call-site dependent inlining:
     *   #1 < sum (map2 (fn (a,w) => (known a) * w) (actuals, #2) *)

(* what kind of recursive function (aka loop) is this *)
datatype loopkind
  = LK_LOOP			(* loop wrapped in a preheader *)
  | LK_TAIL			(* properly tail-recursive *)
  | LK_UNKNOWN			(* something else *)

(* calling convention *)
datatype cconv
  = CC_FCT			(* it's a functor *)
  | CC_FUN of Lty.fflag		(* it's a function, characterized by an fflag *)

(** classifying various kinds of functions *)
type fkind =
  {inline: ilhint,	  (* when should it be inlined? *)
   known : bool,	  (* are all the call sites statically known? *)
   cconv : cconv,	  (* its calling convention *)
   isrec : (Lty.lty list * loopkind) option}
      (* recursive?; if so, lty list are the return types *)

(* additional attributes for type abstractions *)
type tfkind = {inline: ilhint}

(** classifying various kinds of records *)
datatype rkind
  = RK_VECTOR of Lty.tyc  (* vector: all elements have same type, tyc *)
  | RK_STRUCT             (* module: elements may be polymorphic *)
  | RK_TUPLE              (* tuple: all elements are monomorphic *)

end (* structure FunRecMeta *)
