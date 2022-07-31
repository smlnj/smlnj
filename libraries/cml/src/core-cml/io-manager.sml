(* io-manager.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * This is a generic I/O manager for CML.  It uses the OS.IO polling
 * mechanism.
 * NOTE: it currently does not work if more than one thread blocks on the same
 * descriptor.
 *)

structure IOManager : sig

    type iodesc
    type poll_desc
    type poll_info

    val ioEvt : poll_desc -> poll_info Event.event

    val pollIO : unit -> unit

    val anyWaiting : unit -> bool

  end = struct

    structure R = RepTypes
    structure S = Scheduler

    type iodesc = OS.IO.iodesc
    type poll_desc = OS.IO.poll_desc
    type poll_info = OS.IO.poll_info

    type io_wait_item = {
	pd : poll_desc,
	tid : R.trans_id ref,
	cleanUp : unit -> unit,
	k : poll_info SMLofNJ.Cont.cont
      }

    val waiting = ref ([] : io_wait_item list)

  (* In some OSs (e.g., Linux) this may raise an EINTR error, even though
   * it is non-blocking.
   *)
    fun poll l = OS.IO.poll(l, SOME(Time.zeroTime)) handle _ => []

  (* NOTE: as in the case of condition variables (see event.sml), we need to
   * do the cleanUp routine when we enable the ioEvt (instead of in the blockFn
   * continuation).
   *)
    fun ioEvt pd = let
	  fun blockFn {transId, cleanUp, next} = let
		val pi = SMLofNJ.Cont.callcc (fn k => let
		      val item = {pd=pd, tid=transId, cleanUp=cleanUp, k=k}
		      in
			waiting := item :: !waiting;
			next();
			raise Fail "impossible: ioEvt"
		      end)
		in
		  pi
		end
	  fun pollFn () = (case (poll [pd])
		 of [pi] => R.ENABLED{prio= ~1, doFn=(fn () => (S.atomicEnd(); pi))}
		  | _ => R.BLOCKED blockFn
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

(* NOTE: this code works because SML/NJ doesn't use opaque signature matching
 * on the OS.IO interface.
 *)
    fun sameDesc (pi, pd) = (OS.IO.infoToPollDesc pi = pd)

  (* Take an I/O waiting queue and return the cleaned queue along with the list
   * of poll descriptors in the remaining elements.
   *)
    fun clean wq = let
	  fun cl ([] : io_wait_item list, pds, q) = (pds, q)
	    | cl ({tid=ref R.CANCEL, ...} :: r, pds, wq) = cl (r, pds, wq)
	    | cl ((item as {pd, ...}) :: r, pds, wq) = cl (r, pd::pds, item::wq)
	  in
	    cl (wq, [], [])
	  end

  (* enqueue a thread that is polling on the ready queue.  We have to do some
   * continuation hacking to pass the poll info to the thread.  We also must
   * catch the case where the transaction has been canceled, since a single
   * thread might be polling on multiple descriptors.
   *)
    fun enqueue ({tid as ref(R.TRANS id), cleanUp, k, pd}, pi) = let
	  val uk = SMLofNJ.Cont.callcc (fn kk => (
		SMLofNJ.Cont.callcc (fn uk => SMLofNJ.Cont.throw kk uk);
		SMLofNJ.Cont.throw k pi))
	  in
	    tid := R.CANCEL;
	    cleanUp();
	    S.enqueueThread (id, uk)
	  end
      | enqueue ({tid=ref R.CANCEL, ...}, _) = ()

    fun pollIO () = (case clean(! waiting)
	   of ([], _) => waiting := []
	    | (pds, wq) => (case (poll pds)
		 of [] => waiting := List.rev wq
		  | l => let
		      fun filter ([], r, wq) =
			    waiting := List.revAppend(r, wq)
			| filter (pi::pis, (item : io_wait_item)::r, wq) =
			    if sameDesc(pi, #pd item)
			      then (enqueue (item, pi); filter (pis, r, wq))
			      else filter (pi::pis, r, item::wq)
		      in
			filter (l, wq, [])
		      end
		(* end case *))
	  (* end case *))

    fun anyWaiting () = (case !waiting of [] => false | _ => true)

  end
