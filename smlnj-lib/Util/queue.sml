(* queue.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Imperative fifos
 *
 *)

structure Queue :> QUEUE =
  struct
    type 'a queue = 'a Fifo.fifo ref

    exception Dequeue = Fifo.Dequeue

    fun mkQueue () = ref Fifo.empty

    fun clear q = (q := Fifo.empty)

    fun enqueue (q,x) = q := (Fifo.enqueue (!q, x))

    fun dequeue q = let 
          val (newq, x) = Fifo.dequeue (!q) 
          in
            q := newq;
            x
          end
  
    fun next q = (case Fifo.next (!q)
	   of SOME(x, newq) => (q := newq; SOME x)
            | NONE => NONE
	  (* end case *))
  
    fun delete (q, pred) = (q := Fifo.delete (!q, pred))
    fun head q = Fifo.head (!q)
    fun peek q = Fifo.peek (!q)
    fun isEmpty q = Fifo.isEmpty (!q)
    fun length q = Fifo.length (!q)
    fun contents q = Fifo.contents (!q)
    fun app f q = Fifo.app f (!q)
    fun map f q = ref(Fifo.map f (!q))
    fun foldl f b q = Fifo.foldl f b (!q)
    fun foldr f b q = Fifo.foldr f b (!q)

  end
