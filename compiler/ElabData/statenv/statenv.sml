(* statenv.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)
structure StaticEnv : STATICENV =
struct

local
  structure B = Bindings
  structure S = Symbol
  structure E = Env
  structure M = Modules
in

type binding = B.binding
type real_binding = binding * M.modtree option
type staticEnv = real_binding E.env

exception Unbound = E.Unbound

(* aug : binding -> real_binding *)
fun aug x = (x, NONE)

(* strip : real_binding -> binding *)
fun strip ((b,_): real_binding) = b

val empty : staticEnv = E.empty

(* look : staticEnv * S.symbol -> binding *)
fun look (e, s) = strip (E.look (e, s))

(* bind : S.symbol * binding * staticEnv -> staticEnv *)
fun bind (s, b, e) = E.bind (s, aug b, e)

(* bindRB : S.symbol * real_binding * staticEnv *)
fun bindRB (s: S.symbol, rb: real_binding, e: staticEnv) =
    E.bind (s, rb, e)

fun special (mkb, mks) = E.special (aug o mkb, mks)

(* atop : staticEnv * staticEnv -> staticEnv *)
val atop = E.atop

(* consolidate : staticEnv -> staticEnv *)
val consolidate = E.consolidate

(* consolidateLazy : staticEnv -> staticEnv *)
val consolidateLazy = E.consolidateLazy

(* app : (S.symbol * binding -> unit) -> staticEnv -> unit *)
fun app f e = E.app (fn (s, b) => f (s, strip b)) e

(* map : (binding -> binding) -> staticEnv -> staticEnv *)
fun map f e = E.map (aug o f o strip) e

(* fold: ((Symbol.symbol * binding) * 'a -> 'a) -> 'a -> staticEnv -> 'a *)
fun fold f x e = E.fold (fn ((s, b), y) => f ((s, strip b), y)) x e

(* realfold : ((Symbol.symbol * real_binding) * 'a -> 'a) -> 'a -> staticEnv -> 'a *)
val realfold = E.fold

(* symbols : staticEnv -> S.symbol list *)
val symbols = E.symbols

(* foldOverElems: ((Symbol.symbol * binding) * 'a -> 'a) * 'a * staticEnv * Symbol.symbol list -> 'a
 *  fold but only over the elements in the environment with the keys
 *  given in the key list (last parameter). This functions allows
 *  us to compute folds in arbitrary order over a consolidated list.
 *  In particular, this function is currently used in extractSig in
 *  elabmod to keep the inferred signature specs in the same order as
 *  the original structure decls. *)
fun foldOverElems(f, x0, env, []) = x0
  | foldOverElems(f, x0, env, elem::rest) =
      foldOverElems(f, f((elem, look(env,elem)), x0), env, rest)

(* sort: staticEnv -> (Symbol.symbol * binding) list
 *  sort the bindings in an environment.
 *
 * This is used for the assignment of dynamic access slots in structure
 * elaborate, for printing, and for other purposes.
 * The bindings are sorted in the following order:
 *
 *   values
 *   constructors
 *   types
 *   signatures
 *   structures
 *   funsigs
 *   functors
 *   fixity declarations
 *
 * It is only correct to sort environments which have no duplicate bindings.
 * All routines which build structure environments maintain this
 * invariant, so it is ok to sort any structure environment using
 * this function. *)
fun sort env = ListMergeSort.sort B.binderGt (fold (op ::) nil env)

(* filter : staticEnv * Symbol.symbol list -> staticEnv *)
fun filter (e, l) =
    let fun add (sy, e') = bind (sy, look (e, sy), e') handle Unbound => e'
    in foldl add empty l
    end

end (* local *)
end (* structure StaticEnv *)
