(*
 * How to represent a predicate so that various logical operations can
 * be performed.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
functor SSAPredicate(T : MLTREE) : SSA_PREDICATE =
struct
   structure T = T

   datatype expr = FALSE | TRUE | IF of T.ty * T.Basis.cond * expr * expr

   fun toString   : expr -> string

   fun And        : expr * expr -> expr
   fun Or         : expr * expr -> expr
   fun Not        : expr -> expr

   fun isDisjoint : expr * expr -> bool
   fun implies    : expr * expr -> bool

end
