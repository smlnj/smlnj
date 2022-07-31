(*
 * This is Reif and Tarjan's algorithm (SIAM J Computing 1981) 
 * for computing approximate birthpoints for expressions.   
 * For each basic block B,
 *   idef(x) = { defs(v_i) | i = 1 ... n in all paths 
 *                           idom(x) v_1 v_2 ... v_n x where n >= 1 and
 *                                   v_i <> idom(x) for all 1 <= i <= n
 *             }
 * -- Allen
 *)
signature IDEFS =
sig

   type var = int

   val compute_idefs : 
       {def_use : 'n Graph.node -> var list * var list,
        cfg     : ('n,'e,'g) Graph.graph
       } ->
       { idefuse      : unit -> (int list * int list) Array.array,
         ipostdefuse  : unit -> (int list * int list) Array.array
       }

end

