(* iterate.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

structure Iterate : ITERATE =
  struct

    fun badArg (f,msg) = LibBase.failure {module="Iterate",func=f,msg=msg}

    fun iterate f cnt init = let 
          fun iter (0,v) = v
            | iter (n,v) = iter(n-1,f v)
          in
            if cnt < 0 
              then badArg ("iterate","count < 0")
              else iter (cnt,init)
          end
        
    fun repeat f cnt init = let 
          fun iter (n,v) = if n = cnt then v else iter(n+1,f(n,v))
          in
            if cnt < 0 
              then badArg ("repeat","count < 0")
              else iter (0,init)
          end
        
    fun for f (start,stop,inc) = let
          fun up (n,v) = if n > stop then v else up(n+inc,f(n,v))
          fun down (n,v) = if n < stop then v else down(n+inc,f(n,v))
          in
            if start < stop
              then if inc <= 0 then badArg ("for","inc <= 0 with start < stop")
              else fn v => up(start,v)
            else if stop < start
              then if inc >= 0 then badArg ("for","inc >= 0 with start > stop")
              else fn v => down(start,v)
            else fn v => f(start,v)
          end

  end (* Iterate *)
