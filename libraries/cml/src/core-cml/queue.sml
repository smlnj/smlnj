(* queue.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * These are the basic scheduling queues used throughout the CML
 * implementation.  We make the representation concrete, so that
 * other modules can inline the operations [someday we can count
 * on the compiler for this].
 *)

structure Q : sig

    type 'a queue (* = 'a RepTypes.queue *)

    val queue : unit -> 'a queue
	(* create a new queue *)

    val sameQ : ('a queue * 'a queue) -> bool
	(* return true, if the two queues are the same *)

    val isEmpty : 'a queue -> bool
	(* return true, if the queue is empty *)

    val enqueue : ('a queue * 'a) -> unit
	(* enqueue an item in the queue *)

    exception EmptyQ
    val dequeue : 'a queue -> 'a
	(* dequeue an item; raise EmptyQ if the queue is empty *)

    val next : 'a queue -> 'a option
	(* dequeue and return then next item in the queue; return NONE, if
	 * the queue is empty.
	 *)

    val reset : 'a queue -> unit
	(* reset a queue to all empty *)

    val remove : ('a queue * ('a -> bool)) -> unit
	(* find and remove the first item that satisfies the predicate *)

  end = struct

    datatype queue = datatype RepTypes.queue

    fun reverse (x, [], rl) = (x, rl)
      | reverse (x, y :: rest, rl) = reverse (y, rest, x :: rl)

    fun revAppend ([], l) = l
      | revAppend (x::r, l) = revAppend(r, x::l)

    fun queue () = Q{front = ref[], rear = ref[]}

    fun sameQ (Q{front=f1, ...}, Q{front=f2, ...}) = (f1 = f2)

    fun isEmpty (Q{front = ref [], rear = ref []}) = true
      | isEmpty _ = false

    fun enqueue (Q{rear, ...}, item) = rear := item :: !rear

    exception EmptyQ
    fun dequeue (Q{front, rear}) = (case !front
	   of (x::r) => (front := r; x)
	    | [] => (case !rear
		 of (x::r) => let val (y, rr) = reverse(x, r, [])
		      in
			front := rr; rear := []; y
		      end
		  | [] => raise EmptyQ
		(* end case *))
	  (* end case *))

    fun next (Q{front, rear}) = (case !front
	   of (x::r) => (front := r; SOME x)
	    | [] => (case !rear
		 of (x::r) => let val (y, rr) = reverse(x, r, [])
		      in
			front := rr; rear := []; SOME y
		      end
		  | [] => NONE
		(* end case *))
	  (* end case *))

    fun reset (Q{front, rear}) = (front := []; rear := [])

    exception Remove
    fun remove (Q{front, rear}, pred) = let
	  fun lookF ([], l) = lookR(!rear, [])
	    | lookF (x::r, l) =
		if (pred x) then front := revAppend(l, r) else lookF(r, x::l)
	  and lookR ([], _) = raise Remove
	    | lookR (x::r, l) =
		if (pred x) then rear := revAppend(l, r) else lookR(r, x::l)
	  in
	    lookF(!front, [])
	  end

  end
