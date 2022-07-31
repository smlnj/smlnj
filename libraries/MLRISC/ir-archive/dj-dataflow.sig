(*
 * Perform elimination based dataflow analysis (from Sreedhar's work)
 *)
signature DJ_DATAFLOW = 
sig

   structure Dom : DOMINATOR_TREE

   val analyze : 
       { closure   : {y:Graph.node_id} -> unit,
         var_elim  : {y:Graph.node_id, z:Graph.node_id} -> unit,
         fixpoint  : {scc:Graph.node_id list} -> unit,
         compute   : {y:Graph.node_id, z:Graph.node_id} -> unit
       } -> ('n,'e,'g) Dom.dominator_tree -> unit

end
