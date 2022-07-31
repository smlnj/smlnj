(* scheduler.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * This module implements the scheduling queues and preemption
 * mechanisms.
 *)

structure Scheduler : sig

    type thread_id
    type 'a cont = 'a SMLofNJ.Cont.cont

    val rdyQ1 : (thread_id * unit cont) RepTypes.queue

    val getCurThread : unit -> thread_id
    val setCurThread : thread_id -> unit

    val enqueueThread : (thread_id * unit cont) -> unit

    val enqueueAndSwitchCurThread : (unit cont * thread_id) -> unit
	(* enqueue the given continuation with the current thread ID, and make
	 * the given thread ID be the current one.
	 *)

    val enqueueTmpThread : (unit -> unit) -> unit
	(* create a temporary thread (with dummy ID) to run the given
	 * function and then exit.  The thread is placed on the front
	 * of the scheduling queue.
	 *)

    datatype atomic_state = NonAtomic | Atomic | SignalPending

    val atomicState : atomic_state ref

    val atomicBegin : unit -> unit
    val atomicEnd : unit -> unit
	(* enter/leave an atomic region; note that these do not nest *)

    val atomicDispatch : unit -> 'a
	(* leave the atomic region and dispatch the next thread *)

    val dispatch : unit -> 'a
	(* dispatch the next thread; this should NOT be called while in
	 * an atomic region.  Use atomicDispatch() for that case.
	 *)

    val atomicSwitchTo : (thread_id * 'a cont * 'a) -> unit
	(* switch to the given thread, while leaving the atomic region *)

    val atomicYield : unit cont -> 'a
	(* Yield control to the next thread, while leaving the atomic
	 * region.
	 *)

    val schedulerHook : unit cont ref
	(* this hook points to a continuation that gets dispatched when
	 * a preemption is received, or when a thread exits an atomic
	 * region and there is a signal pending.  It is invoked after
	 * leaving the atomic region.
	 *)

    val pauseHook : unit cont ref
	(* this hook points to a continuation that gets invoked when
	 * when the scheduler has nothing else to do.
	 *)

    val shutdownHook : (bool * OS.Process.status) cont ref
	(* this hook points to a continuation that gets invoked when
	 * the system is otherwise deadlocked.  It takes two arguments:
	 * the first is a boolean flag that says weather to do clean-up,
	 * and the second is the exit status.
	 *)

    val getTime : unit -> Time.time
	(* returns an approximation of the current time of day (this is at
	 * least as accurate as the time quantum.
	 *)

    val reset : bool -> unit

  (* control over the preemptive timer *)
    val startTimer   : Time.time -> unit
    val stopTimer    : unit -> unit
    val restartTimer : unit -> unit

  end = struct

    structure R = RepTypes
    structure Sig = Signals

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

  (* some utility functions that should be inlined *)
    fun reverse ([], rl) = rl
      | reverse (x :: rest, rl) = reverse(rest, x :: rl)

    type thread_id = R.thread_id

  (* the current thread is represented using the "var" register *)
    val getCurThread : unit -> thread_id = Unsafe.getVar
    val setCurThread : thread_id -> unit = Unsafe.setVar

  (* The scheduler defines three continuation "hooks":
   *   schedulerHook	-- this points to a continuation that gets dispatched
   *			   when a thread attempts to exit an atomic region and
   *			   there is a signal pending.  It is invoked after
   *			   leaving the atomic region.
   *   pauseHook	-- this points to a continuation that gets invoked when
   *			   there is nothing else to do.
   *   shutdownHook	-- this points to a continuation that gets invoked when
   *			   the system is deadlocked, or when RunCML.shutdown
   *			   is called.  It takes two arguments: the first is a
   *			   boolean flag that says weather to do clean-up, and
   *			   the second is the exit status.
   *)
    fun bogus _ = raise Fail "should never see this "
    val bogusHook : unit cont = SMLofNJ.Cont.isolate bogus
    val bogusShutdownHook : (bool * OS.Process.status) cont =
	  SMLofNJ.Cont.isolate bogus
    val schedulerHook = ref bogusHook
    val pauseHook = ref bogusHook
    val shutdownHook = ref bogusShutdownHook

  (* the dummy thread Id; this is used when an ID is needed to get
   * the types right
   *)
    val dummyTid = R.TID{
	    id = ~1, alert = ref false, done_comm = ref false,
	    exnHandler = ref(fn _ => ()),
	    props = ref[],
	    dead = R.CVAR(ref(R.CVAR_unset[]))
	  }
  (* the error thread.  This thread is used to trap attempts to run CML
   * without proper initialization (i.e., via RunCML).  This thread is
   * enqueued by reset.
   *)
    val errorTid = R.TID{
            id = ~2, alert = ref false, done_comm = ref false,
            exnHandler = ref(fn _ => ()),
	    props = ref[],
	    dead = R.CVAR(ref(R.CVAR_unset[]))
          }
    val errorCont : unit cont = SMLofNJ.Cont.isolate (fn _ => (
	    Debug.sayDebug "**** Use RunCML.doit to run CML ****\n";
	    raise Fail "CML not initialized"))

  (* thread id marking *)
    fun markTid (R.TID{done_comm, ...}) = done_comm := true
    fun unmarkTid (R.TID{done_comm, ...}) = done_comm := false
    fun isMarked (R.TID{done_comm, ...}) = !done_comm

  (* The thread ready queues:
   * rdyQ1 is the primary queue and rdyQ2 is the secondary queue.
   *)
    val (rdyQ1 as R.Q{rear=rear1, ...}) : (R.thread_id * unit cont) Q.queue =
	  Q.queue()
    val rdyQ2 : (R.thread_id * unit cont) Q.queue = Q.queue()

  (* enqueue a ready thread *)
    fun enqueue p = (rear1 := p :: !rear1)
    fun markAndEnqueue (p as (id, _)) = (markTid id; rear1 := p :: !rear1)

    val enqueueThread = markAndEnqueue

  (* enqueue the current thread, and make the given thread ID be the current
   * one.
   *)
    fun enqueueAndSwitchCurThread (resume, tid) = (
	  markAndEnqueue(getCurThread(), resume);
	  setCurThread tid)

  (* dequeue a thread from the primary queue *)
    fun dequeue1 () = (case rdyQ1
	   of (R.Q{front = ref [], rear = ref []}) => dequeue2()
	    | (R.Q{front as (ref []), rear as (ref l)}) => let
		val (x::r) = reverse(l, [])
		in
		  front := r; rear := []; x
		end
	    | (R.Q{front as (ref(x::r)), ...}) => (front := r; x)
	  (* end case *))

  (* remove a thread from the secondary queue (assuming that the
   * primary queue is empty.
   *)
    and dequeue2 () = (case rdyQ2
	   of (R.Q{front = ref [], rear = ref []}) => (dummyTid, !pauseHook)
	    | (R.Q{front as ref [], rear as ref l}) => (
		rear := []; front := reverse(l, []); dequeue2())
	    | (R.Q{front as ref(item::r), ...}) => (front := r; item)
	  (* end case *))

  (* promote a thread from the secondary queue to the primary queue *)
    fun promote () = (case (Q.next rdyQ2)
	   of (SOME x) => enqueue x
	    | NONE => ()
	  (* end case *))

  (* global flag for implementing atomic operations *)
    datatype atomic_state = NonAtomic | Atomic | SignalPending
    val atomicState = ref NonAtomic

  (* Note, the first thing the scheduler hook does is a atomicBegin, so we don't
   * need to clear the atomic state here.
   *) 
    fun dispatchSchedulerHook () = throw (!schedulerHook) ()

(*
    fun enqueueSchedulerHook () =  let
	  val kont = callcc (fn k => (
		callcc (fn k' => throw k k');
		dispatchSchedulerHook ()))
	  val R.Q{front, ...} = rdyQ1
	  in
	    front := (dummyTid, kont) :: !front
	  end
*)

    fun atomicBegin () = atomicState := Atomic

  (* leave an atomic region.
   * NOTE: there is a race condition between the test of the atomicState
   * flag and the setting of it to NonAtomic, but this is not a problem in
   * practice, because there are no GC tests between these (and thus no
   * preemption).
   *)
    fun atomicEnd () = (case !atomicState
	   of SignalPending => callcc (fn k => (
		enqueue(getCurThread(), k);
		dispatchSchedulerHook()))
	    | _ => atomicState := NonAtomic
	  (* end case *))

    fun atomicDispatch () = (case !atomicState
	   of SignalPending => dispatchSchedulerHook()
	    | _ => let
		val (id, kont) = dequeue1()
		in
		  setCurThread id;
		  atomicState := NonAtomic;
		  throw kont ()
		end
	  (* end case *))

    fun dispatch () = (atomicBegin(); atomicDispatch ())

    fun atomicSwitchTo (tid, k, x) =
	  callcc (fn curK => (
	    case !atomicState
	     of SignalPending => 
		  callcc (fn k' => (
		    enqueue(tid, k');
		    enqueue(getCurThread(), curK);
		    dispatchSchedulerHook()))
	      | _ => (
		  enqueueAndSwitchCurThread (curK, tid);
		  atomicState := NonAtomic)
	    (* end case *);
	    throw k x))

  (* Yield control to the next thread, while leaving the atomic region. *)
    fun atomicYield k = (
	  markAndEnqueue(getCurThread(), k);
	  atomicDispatch ())

  (* create a temporary thread (with dummy ID) to run the given
   * function and then exit.  The thread is placed on the front
   * of the scheduling queue.
   *)
    fun enqueueTmpThread f = let
(** this should be, but the overhead is too high right now. **
	  val kont = SMLofNJ.Cont.isolate f
**)
	  val kont = callcc (fn k => (
		callcc (fn k' => throw k k');
		f () handle _ => ();
		dispatch ()))
	  val R.Q{front, ...} = rdyQ1
	  in
	    front := (dummyTid, kont) :: !front
	  end

    val defaultHook : unit cont = SMLofNJ.Cont.isolate dispatch

  (* this holds an approximation of the current time of day.  It is
   * cleared at each pre-emption, and initialized on demand (by getTime).
   *)
    val clock = ref(SOME Time.zeroTime)

  (* returns an approximation of the current time of day (this is at
   * least as accurate as the time quantum).
   *)
    fun getTime () = (case !clock
	   of NONE => let val t = Time.now()
		in
		  clock := SOME t;  t
		end
	    | (SOME t) => t
	  (* end case *))

  (* preempt the current thread (with continuation k). *)
    fun preempt k = let
	  val curTid = getCurThread()
	  val curP = (curTid, k)
	  in
	    if (isMarked curTid)
	      then (
		unmarkTid curTid;
		promote ();
		enqueue curP)
	      else Q.enqueue(rdyQ2, curP)
	  end

  (* the preemption handler *)
    fun alrmHandler (_, _, k) = (
	  clock := NONE;
	  case !atomicState
	   of NonAtomic => (preempt k; !schedulerHook)
	    | Atomic => (atomicState := SignalPending; k)
	    | _ => k
	  (* end case *))

    val defaultTimeQ = Time.fromMilliseconds 20
    val timeQ = ref defaultTimeQ

    structure IT = SMLofNJ.IntervalTimer

    fun startTimer tq = let
	  val tq = if Time.<(Time.zeroTime, tq) then tq else defaultTimeQ
	  in
	    timeQ := tq;
	    ignore (Sig.setHandler (Sig.sigALRM, Sig.HANDLER alrmHandler));
	    ignore (IT.setIntTimer (SOME tq))
	  end

    fun stopTimer () = (
	  ignore (IT.setIntTimer NONE);
	  ignore (Sig.setHandler (Sig.sigALRM, Sig.IGNORE)))

    fun restartTimer () = startTimer (!timeQ)

  (* reset various pieces of state *)
    fun reset running = (
	  setCurThread dummyTid;
	  pauseHook := bogusHook;
	  shutdownHook := bogusShutdownHook;
	  schedulerHook := defaultHook;
	  clock := NONE;
	  Q.reset rdyQ1; Q.reset rdyQ2;
	  if (not running) then enqueueThread(errorTid, errorCont) else ())

    val _ = reset false

  end

