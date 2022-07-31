(*
 * Abstract signature of an optimization phase
 *)
signature MLRISC_OPTIMIZATION =
sig
   type flowgraph                     (* representation is abstract *)
   val name : string                  (* name of optimization *)
   val run  : flowgraph -> flowgraph  (* run optimization *)
end
