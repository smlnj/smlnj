(* fifo.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Applicative fifos
 *
 *)

structure Fifo : FIFO =
  struct
    datatype 'a fifo = Q of {front: 'a list, rear: 'a list}

    exception Dequeue

    val empty = Q{front=[],rear=[]}

    fun isEmpty (Q{front=[],rear=[]}) = true
      | isEmpty _ = false

    fun enqueue (Q{front,rear},x) = Q{front=front,rear=(x::rear)}

    fun dequeue (Q{front=(hd::tl),rear}) = (Q{front=tl,rear=rear},hd)
      | dequeue (Q{rear=[],...}) = raise Dequeue
      | dequeue (Q{rear,...}) = dequeue(Q{front=rev rear,rear=[]})

    fun next (Q{front=(hd::tl),rear}) = SOME(hd, Q{front=tl,rear=rear})
      | next (Q{rear=[],...}) = NONE
      | next (Q{rear,...}) = next(Q{front=rev rear,rear=[]})

    fun delete (Q{front, rear}, pred) = let
	  fun doFront [] = {front = doRear(rev rear), rear = []}
	    | doFront (x::r) = if (pred x)
		then {front = r, rear = rear}
		else let val {front, rear} = doFront r
		  in {front =  x :: front, rear = rear} end
	  and doRear [] = []
	    | doRear (x::r) = if (pred x) then r else x :: (doRear r)
	  in
	    Q(doFront front)
	  end

    fun peek (Q{front=(hd::_), ...}) = SOME hd
      | peek (Q{rear=[], ...}) = NONE
      | peek (Q{rear, ...}) = SOME(hd(rev rear))

    fun head (Q{front=(hd::_),...}) = hd
      | head (Q{rear=[],...}) = raise Dequeue
      | head (Q{rear,...}) = hd(rev rear)

    fun length (Q {rear,front}) = (List.length rear) + (List.length front)

    fun contents (Q {rear, front}) = (front @ (rev rear))

    fun app f (Q{front,rear}) = (List.app f front; List.app f (List.rev rear))
    fun map f (Q{front,rear}) = 
          Q{front = List.map f front, rear = rev(List.map f(rev rear))}
    fun foldl f b (Q{front,rear}) = List.foldr f (List.foldl f b front) rear
    fun foldr f b (Q{front,rear}) = List.foldr f (List.foldl f b rear) front

  end

