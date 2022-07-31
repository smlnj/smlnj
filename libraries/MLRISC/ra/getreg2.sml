(* getreg2.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

(** A simple first come/first serve register allocator **)
functor GetReg2(val first : int  (* start from ``first'' *)
                val nRegs : int  (* n registers *)
	        val available : int list) : GETREG =
struct
  exception GetReg
  val size = first+nRegs
  val allRegs = Array.array(size,false)
  val preferred = Array.array(size,~1)

  fun reset () = Array.modify(fn _ => ~1) preferred

  val _ = app (fn r => Array.update(allRegs,r,true)) available

  fun getreg{pref,stamp:int,proh} = 
  let (* use preferred registers whenever possible *)
      fun checkPreferred [] = find(first)
        | checkPreferred(r::rs) = 
           if Array.sub(proh,r) <> stamp andalso 
              Array.sub(allRegs,r) then r 
           else checkPreferred rs

      and find(start) =
          let val limit = Array.length allRegs
              fun search r = 
              if Array.sub(proh,r) <> stamp andalso
                 Array.sub(allRegs,r) then r 
              else let val r = r+1
                   in  if r >= limit then raise GetReg else search r
                   end
          in  search start
          end
  in  checkPreferred pref end

  fun getpair{pref,stamp:int,proh} = raise GetReg (* unimplemented *)

end


    

