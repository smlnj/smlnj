(* getreg.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** A simple round robin register allocator **)
functor GetReg(val first : int  (* start from ``first'' *)
               val nRegs : int  (* n registers *)
	       val available : int list) : GETREG =
struct
  exception GetReg
  val size = first+nRegs
  val allRegs = Array.array(size,false)
  val preferred = Array.array(size,~1)

  val lastReg = ref first

  fun reset () = (lastReg := first; Array.modify(fn _ => ~1) preferred)

  val _ = app (fn r => Array.update(allRegs,r,true)) available

  fun getreg{pref,stamp:int,proh} = 
  let (* use preferred registers whenever possible *)
      fun checkPreferred [] = find(!lastReg)
        | checkPreferred(r::rs) = 
           if Array.sub(proh,r) <> stamp andalso 
              Array.sub(allRegs,r) then r 
           else checkPreferred rs

      (* if not, use the round robin scheme to look for a register *)
      and find(start) =
          let val limit = Array.length allRegs
              fun search r = 
              if Array.sub(proh,r) <> stamp andalso
                 Array.sub(allRegs,r) then r 
              else let val r = r+1
                       val r = if r >= limit then first else r
                   in  if r = start then raise GetReg
                       else search r
                   end
              val found = search(start)
              val next = found + 1
              val next = if next >= limit then first else next 
          in  lastReg := next;
              found
          end
  in  checkPreferred pref 
  end

  val lastRegPair = ref first

  fun getpair{pref, stamp:int, proh} = let
      (* if not, use the round robin scheme to look for a register *)
      fun find(start) = let
          val limit = Array.length allRegs
          fun search r = 
              if Array.sub(proh,r) <> stamp 
		    andalso Array.sub(proh,r+1) <> stamp 
		    andalso Array.sub(allRegs,r) 
		    andalso Array.sub(allRegs,r+1) then r 
              else let 
		      val nxt = r+1
                      val nxtR = if nxt+1 >= limit then first else nxt
                   in 
		      if nxtR = start then raise GetReg else search nxtR
                   end
              val found = search(start)
              val next = found + 1
              val next = if next+1 >= limit then first else next
      in  
	  lastRegPair := next;
          found
      end
  in  find(!lastRegPair) 
  end

end


    

