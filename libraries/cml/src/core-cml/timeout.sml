(* timeout.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Events for synchronizing on timeouts.
 *)

structure TimeOut : sig

    include TIME_OUT

    val reset : unit -> unit
    val pollTime : unit -> unit
    val anyWaiting : unit -> Time.time option

  end = struct

    structure R = RepTypes
    structure S = Scheduler

    type 'a event = 'a Event.event

  (* The list of threads waiting for timouts.  It is sorted in increasing order
   * of time value.
   * NOTE: we may want to use some sort of balanced search structure in the
   * future.
   *)
    type item = (Time.time * (unit -> unit) * R.trans_id ref * unit S.cont)
    val timeQ = ref ([] : item list)

    fun timeWait (t, f, id, k) = let
	  fun ins [] = [(t, f, id, k)]
	    | ins ((_, _, ref R.CANCEL, _) :: r) = ins r
	    | ins (l as ((item as (t', _, _, _)) :: r)) = if (Time.<(t', t))
		then item :: ins r
		else (t, f, id, k) :: l
	  in
	    timeQ := ins (! timeQ)
	  end

    fun clean [] = []
      | clean ((_, _, ref R.CANCEL, _) :: r) = clean r
      | clean (item :: r) = item :: clean r

    fun checkQ q = let
	  val now = S.getTime()
	  fun chk [] = []
	    | chk ((_, _, ref R.CANCEL, _) :: r) = chk r
	    | chk (l as ((item as (t', f, transId as ref(R.TRANS tid), k)) :: r)) =
		if (Time.<=(t', now))
		  then (
		    S.enqueueThread (tid, k);
		    f();  (* cleanup function *)
		    chk r)
		  else clean l
	  in
	    chk q
	  end

    fun anyWaiting () = (case clean(!timeQ)
	   of [] => NONE
	    | (q as ((t, _, _, _)::_)) => let
		val now = S.getTime()
		in
		  if (Time.<=(t, now))
		    then SOME(Time.zeroTime)
		    else SOME(Time.-(t, now))
		end
	  (* end case *))
	
    fun pollTime () = (case !timeQ
	   of [] => ()
	    | q => timeQ := checkQ q
	  (* end case *))

    fun reset () = timeQ := []

  (** NOTE: unlike for most base events, the block functions of time-out
   ** events do not have to exit the atomic region or execute the clean-up
   ** operation.  This is done when they are removed from the waiting queue.
   **)
    fun timeOutEvt t = let
	  fun blockFn {transId, cleanUp, next} = let
		val t0 = S.getTime()
		in
		  SMLofNJ.Cont.callcc (fn k => (
		    timeWait (Time.+(t, t0), cleanUp, transId, k);
		    next()));
		    S.atomicEnd()
		end
	  fun pollFn () = if (t = Time.zeroTime)
		then R.ENABLED{prio= ~1, doFn=S.atomicEnd}
		else R.BLOCKED blockFn
	  in
	    R.BEVT[pollFn]
	  end

    fun atTimeEvt t = let
	  fun blockFn {transId, cleanUp, next} = (
		SMLofNJ.Cont.callcc (fn k => (
		  timeWait (t, cleanUp, transId, k);
		  next()));
		S.atomicEnd())
	  fun pollFn () = if Time.<=(t, S.getTime())
		then R.ENABLED{prio= ~1, doFn=S.atomicEnd}
		else R.BLOCKED blockFn
	  in
	    R.BEVT[pollFn]
	  end

  end;
