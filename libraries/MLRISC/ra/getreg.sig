(*
 * A simple round robin based register allocator.
 * Now with the ability to get register pairs.
 * -- Allen
 *)
signature GETREG = 
sig 
   exception GetReg

     (* get a register, unconstrained but with optional preference *)
     (* if sub(proh,r) = stamp that means the register is prohibited *)
   val getreg : {pref:int list, stamp:int, proh:int Array.array} -> int 

     (* get a register pair, must be an even/odd pair, returns the
      * even register (i.e. the smaller one)
      *)
   val getpair : {pref:int list, stamp:int, proh:int Array.array} -> int

     (* reset the state *)
   val reset : unit -> unit
end
