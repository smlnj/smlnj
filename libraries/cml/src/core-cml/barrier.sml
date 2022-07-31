(* barrier.sml
 *
 * COPYRIGHT (c) 2011 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure Barrier :> BARRIER =
  struct

    structure S = Scheduler

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

    datatype 'a result = RAISE of exn | VALUE of 'a

    datatype 'a barrier = BAR of {
	state : 'a ref,
	update : 'a -> 'a,
	nEnrolled : int ref,
	nWaiting : int ref,
	waiting : (S.thread_id * 'a result cont) list ref
      }

    datatype status = ENROLLED | WAITING | RESIGNED

    datatype 'a enrollment = ENROLL of {
	bar : 'a barrier,
	sts : status ref	(* current status of this enrollment *)
      }

  (* create a new barrier.  The first argument is the update function that
   * is applied to the global state whenever a barrier synchronization occurs.
   * The second argument is the initial global state.
   *)
    fun barrier update init = BAR{
	    state = ref init,
	    update = update,
	    nEnrolled = ref 0,
	    nWaiting = ref 0,
	    waiting = ref []
	  }

  (* enroll in a barrier *)
    fun enroll (bar as BAR{nEnrolled, ...}) = (
	  S.atomicBegin();
	  nEnrolled := !nEnrolled + 1;
	  S.atomicEnd();
	  ENROLL{bar = bar, sts = ref ENROLLED})

    fun wakeupThd result (tid, resumeK) =
	  S.enqueueThread(
	    tid, callcc(fn k => (callcc(fn k' => throw k k'); throw resumeK result)))

    fun return (RAISE exn) = raise exn
      | return (VALUE x) = x

  (* synchronize on a barrier *)
    fun wait (ENROLL{bar=BAR{state, update, nEnrolled, nWaiting, waiting}, sts}) = (
	  S.atomicBegin();
	  case !sts
	   of ENROLLED => (
		sts := WAITING;
		nWaiting := !nWaiting+1;
		if (!nWaiting = !nEnrolled)
		  then let (* all threads are at the barrier, so we can proceed *)
		    val result = let
			  val x = update(!state)
			  in
			    state := x;
			    VALUE x
			  end handle exn => RAISE exn
		    in
		      List.app (wakeupThd result) (!waiting);
		      nWaiting := 0;
		      waiting := [];
		      S.atomicEnd ();
		      return result
		    end
		  else (
		    sts := WAITING;
		    return (callcc (fn resumeK => (
		      waiting := (S.getCurThread(), resumeK) :: !waiting;
		      S.atomicDispatch())))))
	    | WAITING => (S.atomicEnd(); raise Fail "multiple barrier waits")
	    | RESIGNED => (S.atomicEnd(); raise Fail "barrier wait after resignation")
	  (* end case *))

  (* resign from an enrolled barrier *)
    fun resign (ENROLL{bar, sts}) = (
	  S.atomicBegin();
	  case !sts
	   of RESIGNED => () (* ignore multiple resignations *)
	    | WAITING => (S.atomicEnd(); raise Fail "resign while waiting")
	    | ENROLLED => (sts := RESIGNED; S.atomicEnd()))

  (* get the current state of the barrier *)
    fun value (ENROLL{bar=BAR{state, ...}, ...}) = !state

  end
