(* channel.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of synchronous channels.
 *
 * To ensure that we always leave the atomic region exactly once, we
 * require that the blocking operation be responsible for leaving the
 * atomic region (in the event case, it must also execute the clean-up
 * action).  The doFn always transfers control to the blocked thread
 * without leaving the atomic region.  Note thet the send (and sendEvt)
 * blockFns run using the receiver's thread ID.
 *)

structure Channel : sig

    type 'a event

    include CHANNEL

    val resetChan : 'a chan -> unit

  end = struct

    structure T = Thread
    structure S = Scheduler
    structure R = RepTypes

    type 'a event = 'a Event.event

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

  (* Some inline functions to improve performance *)
    fun enqueue (R.Q{rear, ...}, x) = rear := x :: !rear

    datatype 'a chan = CHAN of {
        priority : int ref,
        inQ      : (R.trans_id ref * 'a cont) R.queue,
        outQ     : (R.trans_id ref * (R.thread_id * 'a cont) cont) R.queue
      }

    fun resetChan (CHAN{priority, inQ, outQ}) = (
	  priority := 1;
	  Q.reset inQ;
	  Q.reset outQ)

    fun channel () = CHAN{priority=ref 1, inQ=Q.queue(), outQ=Q.queue()}

  (* sameChannel : ('a chan * 'a chan) -> bool *)
    fun sameChannel (CHAN{inQ=in1, ...}, CHAN{inQ=in2, ...}) =
	  Q.sameQ(in1, in2)

  (* create a new transaction ID *)
    fun mkId () = ref(R.TRANS(S.getCurThread()))

  (* given a transaction ID, get its thread ID and mark it cancelled. *)
    fun getIdFromTrans (transId as ref(R.TRANS tid)) = (
	  transId := R.CANCEL;
	  tid)

  (* given a transaction ID, set the current thread to its thread ID
   * and mark it cancelled.
   *)
    fun setCurThread transId = S.setCurThread(getIdFromTrans transId)

    datatype 'a q_item
      = NoItem
      | Item of (R.trans_id ref * 'a cont)

  (* bump a priority value by one, returning the old value *)
    fun bumpPriority (p as ref n) = (p := n+1; n)

  (* functions to clean channel input and output queues *)
    local
      fun clean [] = []
	| clean ((ref R.CANCEL, _)::r) = clean r
	| clean l = l
      fun cleanRev ([], l) = l
	| cleanRev ((ref R.CANCEL, _)::r, l) = cleanRev (r, l)
	| cleanRev (x::r, l) = cleanRev (r, x::l)
      fun cleanAll l = let
	    fun rev ([], l) = l
	      | rev (x::r, l) = rev(r, x::l)
	    in
	      rev (cleanRev (l, []), [])
	    end
    in
    fun cleanAndChk (priority, R.Q{front, rear}) = let
	  fun cleanFront [] = cleanRear (! rear)
	    | cleanFront f = (case (clean f)
		 of [] => cleanRear (! rear)
		  | f' => (front := f'; bumpPriority priority)
		(* end case *))
	  and cleanRear [] = 0
	    | cleanRear r = (
		rear := [];
		case (cleanRev (r, []))
		 of [] => 0
		  | rr => (front := rr; bumpPriority priority)
		(* end case *))
	  in
	    cleanFront (! front)
	  end
    fun cleanAndRemove (R.Q{front, rear, ...}) = let
	  fun cleanFront [] = cleanRear (! rear)
	    | cleanFront f = (case (clean f)
		 of [] => cleanRear (! rear)
		  | (item::rest) => (front := rest; Item item)
		(* end case *))
	  and cleanRear [] = NoItem
	    | cleanRear r = (
		rear := [];
		case (cleanRev (r, []))
		 of [] => NoItem
		  | (item::rest) => (front := rest; Item item)
		(* end case *))
	  in
	    cleanFront (! front)
	  end
    fun cleanAndEnqueue (R.Q{front, rear, ...}, item) = (case cleanAll(!front)
	   of [] => (front := cleanRev(!rear, [item]); rear := [])
	    | f => (front := f; rear := item :: cleanAll(! rear))
	  (* end case *))
    end (* local *)

    fun impossible () = raise Fail "Channel: impossible"

    fun send (CHAN{priority, inQ, outQ}, msg) = (
	  S.atomicBegin();
	  case (cleanAndRemove inQ)
           of Item(rid, rkont) => callcc (fn sendK => (
                S.enqueueAndSwitchCurThread(sendK, getIdFromTrans rid);
		priority := 1;
                throw rkont msg))
            | NoItem => let
		val (recvId, recvK) = callcc (fn sendK => (
			enqueue (outQ, (mkId(), sendK));
			S.atomicDispatch()))
		in
		  S.atomicSwitchTo (recvId, recvK, msg)
		end
          (* end case *))

    fun sendEvt (CHAN{priority, inQ, outQ}, msg) = let
	  fun doFn () = let
		val (transId, rkont) = Q.dequeue inQ
		in
		  callcc (fn sendK => (
		    S.enqueueAndSwitchCurThread(sendK, getIdFromTrans transId);
		    priority := 1;
		    throw rkont msg))
		end
	  fun blockFn {transId, cleanUp, next} = let
		val (recvId, recvK) = callcc (fn sendK => (
			cleanAndEnqueue (outQ, (transId, sendK));
			next();
			impossible ()))
		in
		  cleanUp();
		  S.atomicSwitchTo (recvId, recvK, msg)
		end
	  fun pollFn () = (case (cleanAndChk (priority, inQ))
		 of 0 => R.BLOCKED blockFn
		  | p => R.ENABLED{prio=p, doFn=doFn}
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

    fun sendPoll (CHAN{priority, inQ, outQ}, msg) = callcc (fn sendK => (
	  S.atomicBegin();
	  case (cleanAndRemove inQ)
           of Item(rid, rkont) => (
		callcc (fn sendK => (
		  S.enqueueAndSwitchCurThread(sendK, getIdFromTrans rid);
		  priority := 1;
                  throw rkont msg));
		true)
            | NoItem => (S.atomicEnd(); false)
          (* end case *)))

    fun recv (CHAN{priority, inQ, outQ}) = callcc (fn recvK => (
	  S.atomicBegin ();
	  case (cleanAndRemove outQ)
	   of Item(transId, sendK) => let
		val myId = S.getCurThread()
		in
		  setCurThread transId;
		  priority := 1;
		  throw sendK (myId, recvK)
		end
	    | NoItem => (
		enqueue (inQ, (mkId(), recvK));
		S.atomicDispatch())
	  (* end case *)))

    fun recvEvt (CHAN{priority, inQ, outQ}) = let
	  fun doFn () = let
		val (transId, sendK) = Q.dequeue outQ
		val myId = S.getCurThread()
		in
		  setCurThread transId;
		  priority := 1;
		  callcc (fn recvK => throw sendK (myId, recvK))
		end
	  fun blockFn {transId, cleanUp, next} = let
		val msg = callcc (fn recvK => (
		      cleanAndEnqueue (inQ, (transId, recvK));
		      next ();
		      impossible()))
		in
		  cleanUp();
		  S.atomicEnd();
		  msg
		end
	  fun pollFn () = (case (cleanAndChk (priority, outQ))
		 of 0 => R.BLOCKED blockFn
		  | p => R.ENABLED{prio=p, doFn=doFn}
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

    fun recvPoll (CHAN{priority, inQ, outQ}) = (
	  S.atomicBegin ();
	  case (cleanAndRemove outQ)
	   of Item(transId, sendK) => SOME(callcc (fn recvK => 
		let
		val myId = S.getCurThread()
		in
		  setCurThread transId;
		  priority := 1;
		  throw sendK (myId, recvK)
		end))
	    | NoItem => (S.atomicEnd(); NONE)
	  (* end case *))

  end
