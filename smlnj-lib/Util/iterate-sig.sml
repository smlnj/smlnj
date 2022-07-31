(* iterate-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 *)

signature ITERATE =
  sig

    val iterate : ('a -> 'a) -> int -> 'a -> 'a
     (* iterate f cnt init = f(f(...f(f(init))...)) (cnt times)
      * iterate f 0 init = init
      * raises BadArg if cnt < 0
      *)

    val repeat : (int * 'a -> 'a) -> int -> 'a -> 'a
     (* repeat f cnt init 
      *     = #2(iterate (fn (i,v) => (i+1,f(i,v))) cnt (0,init))
      *)

    val for : (int * 'a -> 'a) -> (int * int * int) -> 'a -> 'a
     (* for f (start,stop,inc) init 
      *      "for loop"
      *      implements f(...f(start+2*inc,f(start+inc,f(start,init)))...)
      *      until the first argument of f > stop if inc > 0
      *         or the first argument of f < stop if inc < 0
      * raises BadArg if inc <= 0 and start < stop or if inc >=0 and
      * start > stop.
      *)

  end (* ITERATE *)
