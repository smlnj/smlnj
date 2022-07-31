(*
 * This module tests for reducibility of a loop
 *
 * -- Allen
 *)
signature REDUCIBILITY = 
sig
    structure Loop : LOOP_STRUCTURE

    val is_reducible : ('n,'e,'g) Loop.loop_structure -> Graph.node_id -> bool
end
