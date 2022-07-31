(*
 * This generic module is used for computing static single assignment form.
 * Actually only the renaming and iterated dominance frontiers computation
 * is implemented here.  
 *
 * -- Allen
 *)
signature STATIC_SINGLE_ASSIGNMENT_FORM =
sig

   structure Dom : DOMINATOR_TREE

   type var     = int 
   type phi  = var * var * var list (* orig def/def/uses *)
   type renamer = {defs : var list, uses: var list} ->
                  {defs : var list, uses: var list}
   type copy    = {dst : var list, src: var list} -> unit

         (* 
          * Given a set of definitions for each block,
          * Compute the set of phi nodes.
          *)
   val compute_ssa : 
       ('n,'e,'g) Dom.dominator_tree ->
       { max_var      : var,  
         defs         : 'n Graph.node -> var list,
         is_live      : var * int -> bool,
         rename_var   : var -> var,
         rename_stmt  : {rename:renamer,copy:copy} -> 'n Graph.node -> unit,
         insert_phi   : {block    : 'n Graph.node,
                         in_edges : 'e Graph.edge list,
                         phis     : phi list 
                        } -> unit
       } -> unit
end

