(*
 * How to represent a predicate so that various logical operations can
 * be performed.
 *
 * -- Allen (leunga@cs.nyu.edu)
 *)
signature SSA_PREDICATE =
sig
   structure T : MLTREE

   datatype expr = FALSE | TRUE | IF of T.ty * T.Basis.cond * expr * expr

   val toString   : expr -> string

   val And        : expr * expr -> expr
   val Or         : expr * expr -> expr
   val Not        : expr -> expr

   val isDisjoint : expr * expr -> bool
   val implies    : expr * expr -> bool

end
