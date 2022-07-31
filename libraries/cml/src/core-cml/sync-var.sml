(* sync-var.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The implementation of Id-style synchronizing memory cells.
 *)

structure SyncVar :> SYNC_VAR =
  struct

    structure R = RepTypes
    structure S = Scheduler

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

  (* the underlying representation of both ivars and mvars is the same. *)
    datatype 'a cell = CELL of {
	priority : int ref,
	readQ : (R.trans_id ref * 'a cont) Q.queue,
        value : 'a option ref
      }

    type 'a ivar = 'a cell
    type 'a mvar = 'a cell

    exception Put

    fun newCell () = CELL{priority = ref 0, readQ = Q.queue(), value=ref NONE}
    fun sameCell (CELL{value=v1, ...}, CELL{value=v2, ...}) = (v1 = v2)

  (* create a new transaction ID *)
    fun mkId () = ref(R.TRANS(S.getCurThread()))

  (* given a transaction ID, get its thread ID and mark it cancelled. *)
    fun getIdFromTrans (transId as ref(R.TRANS tid)) = (
	  transId := R.CANCEL;
	  tid)

  (* bump a priority value by one, returning the old value *)
    fun bumpPriority (p as ref n) = (p := n+1; n)

    datatype 'a q_item
      = NoItem
      | Item of (R.trans_id ref * 'a cont)

  (* functions to clean channel input and output queues *)
    local
      fun clean [] = []
	| clean ((ref R.CANCEL, _)::r) = clean r
	| clean l = l
      fun cleanRev ([], l) = l
	| cleanRev ((ref R.CANCEL, _)::r, l) = cleanRev (r, l)
	| cleanRev (x::r, l) = cleanRev (r, x::l)
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
    fun cleanAndEnqueue (R.Q{front, rear, ...}, item) = let
	  fun cleanFront [] = cleanRear (! rear)
	    | cleanFront f = (case (clean f)
		 of [] => cleanRear (! rear)
		  | f' => (front := f'; rear := item :: (! rear))
		(* end case *))
	  and cleanRear [] = (front := [item])
	    | cleanRear r = (case (cleanRev (r, []))
		 of [] => (front := [item]; rear := [])
		  | rr => (rear := [item]; front := rr)
		(* end case *))
	  in
	    cleanFront (! front)
	  end
    end (* local *)

  (* When a thread is resumed after being blocked on an iGet or mGet operation,
   * there may be other threads also blocked on the variable.  This function
   * is used to propagate the message to all of the threads that are blocked
   * on the variable (or until one of them takes the value in the mvar case).
   * It must be called from an atomic region; when the readQ is finally empty,
   * we leave the atomic region.  We must use "cleanAndRemove" to get items
   * from the readQ in the unlikely event that a single thread executes a
   * choice of multiple gets on the same variable.
   *)
    fun relayMsg (readQ, msg) = (case (cleanAndRemove readQ)
	   of NoItem => S.atomicEnd()
	    | (Item(transId, kont)) => callcc (fn myKont => (
		S.enqueueAndSwitchCurThread(myKont, getIdFromTrans transId);
		throw kont msg))
	  (* end case *))

    fun impossible () = raise Fail "SyncVar: impossible"


  (** I-variables **)

    val iVar = newCell
    val sameIVar = sameCell

    fun iPut (CELL{priority, readQ, value}, x) = (
	  S.atomicBegin();
	  case !value
	   of NONE => (
		value := SOME x;
		case (cleanAndRemove readQ)
		 of NoItem => S.atomicEnd()
		  | (Item(transId, kont)) => callcc (fn myKont => (
		      S.enqueueAndSwitchCurThread(myKont, getIdFromTrans transId);
		      priority := 1;
		      throw kont x))
		(* end case *))
	    | (SOME _) => (S.atomicEnd(); raise Put)
	  (* end case *))

    fun iGet (CELL{priority, readQ, value}) = (
	  S.atomicBegin();
	  case !value
	   of NONE => let
		val msg = callcc (fn k => (
			Q.enqueue (readQ, (mkId(), k));
			S.atomicDispatch ()))
		in
		  relayMsg (readQ, msg); msg
		end
	    | (SOME v) => (S.atomicEnd(); v)
	  (* end case *))

    fun iGetEvt (CELL{priority, readQ, value}) = let
	  fun blockFn {transId, cleanUp, next} = let
		val msg = callcc (fn k => (
			Q.enqueue (readQ, (transId, k));
			next ();
			impossible()))
		in
		  cleanUp();
		  relayMsg (readQ, msg); msg
		end
	  fun pollFn () = (case !value
		 of NONE => R.BLOCKED blockFn
		  | (SOME v) => R.ENABLED{
			prio=bumpPriority priority,
			doFn=(fn () => (priority := 1; S.atomicEnd(); v))
		      }
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

  (* NOTE: we assume that reads are atomic, so this function does not
   * need to run in an atomic region.
   *)
    fun iGetPoll (CELL{value, ...}) = !value


  (** M-variables **)

    val mVar = newCell
    fun mVarInit x = CELL{priority = ref 0, readQ = Q.queue(), value=ref(SOME x)}
    val sameMVar = sameCell

    fun mPut (CELL{priority, readQ, value}, x) = (
	  S.atomicBegin();
	  case !value
	   of NONE => (
		value := SOME x;
		case (cleanAndRemove readQ)
		 of NoItem => S.atomicEnd()
		  | (Item(transId, kont)) => callcc (fn myKont => (
		      S.enqueueAndSwitchCurThread(myKont, getIdFromTrans transId);
		      priority := 1;
		      throw kont x))
		(* end case *))
	    | (SOME _) => (S.atomicEnd(); raise Put)
	  (* end case *))

    fun mTake (CELL{priority, readQ, value}) = (
	  S.atomicBegin();
	  case !value
	   of NONE => let
		val v = callcc (fn k => (
			Q.enqueue (readQ, (mkId(), k));
			S.atomicDispatch ()))
		in
		  value := NONE;
		  S.atomicEnd();
		  v
		end
	    | (SOME v) => (value := NONE; S.atomicEnd(); v)
	  (* end case *))

    fun mTakeEvt (CELL{priority, readQ, value}) = let
	  fun blockFn {transId, cleanUp, next} = let
		val v = callcc (fn k => (
			Q.enqueue (readQ, (transId, k));
			next ();
			impossible()))
		in
		  cleanUp();
		  value := NONE;
		  S.atomicEnd();
		  v
		end
	  fun pollFn () = (case !value
		 of NONE => R.BLOCKED blockFn
		  | (SOME v) => R.ENABLED{
			prio=bumpPriority priority,
			doFn=(fn () => (value := NONE; S.atomicEnd(); v))
		      }
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

    fun mTakePoll (CELL{priority, readQ, value}) = let
	  val res = (
		S.atomicBegin();
		case !value
		 of NONE => NONE
		  | (SOME v) => (value := NONE; SOME v)
		(* end case *))
	  in
	    S.atomicEnd(); res
	  end

    fun mGet (CELL{priority, readQ, value}) = (
	  S.atomicBegin();
	  case !value
	   of NONE => let
		val v = callcc (fn k => (
			Q.enqueue (readQ, (mkId(), k));
			S.atomicDispatch ()))
		in
		  relayMsg (readQ, v); v
		end
	    | (SOME v) => (S.atomicEnd(); v)
	  (* end case *))

    fun mGetEvt (CELL{priority, readQ, value}) = let
	  fun blockFn {transId, cleanUp, next} = let
		val v = callcc (fn k => (
			Q.enqueue (readQ, (transId, k));
			next ();
			impossible()))
		in
		  cleanUp();
		  relayMsg (readQ, v);
		  v
		end
	  fun pollFn () = (case !value
		 of NONE => R.BLOCKED blockFn
		  | (SOME v) => R.ENABLED{
			prio=bumpPriority priority,
			doFn=(fn () => (S.atomicEnd(); v))
		      }
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

  (* NOTE: we assume that reads are atomic, so this function does not
   * need to run in an atomic region.
   *)
    fun mGetPoll (CELL{value, ...}) = !value

  (* Swap the current contents of the cell with a new value.  This function
   * has the effect of an mTake followed by an mPut, except that it is
   * guaranteed to be atomic.  It is also somewhat more efficient.
   *)
    fun mSwap (CELL{priority, readQ, value}, newV) = (
	  S.atomicBegin();
	  case !value
	   of NONE => let
		val v = callcc (fn k => (
			Q.enqueue (readQ, (mkId(), k));
			S.atomicDispatch ()))
		in
		  value := SOME newV;
		(* relay the new value to any other blocked threads *)
		  relayMsg (readQ, newV);
		  v
		end
	    | (SOME v) => (value := SOME newV; S.atomicEnd(); v)
	  (* end case *))

    fun mSwapEvt (CELL{priority, readQ, value}, newV) = let
	  fun blockFn {transId, cleanUp, next} = let
		val v = callcc (fn k => (
			Q.enqueue (readQ, (transId, k));
			next ();
			impossible()))
		in
		  cleanUp();
		  value := SOME newV;
		  relayMsg (readQ, newV);
		  v
		end
	  fun pollFn () = (case !value
		 of NONE => R.BLOCKED blockFn
		  | (SOME v) => R.ENABLED{
			prio=bumpPriority priority,
			doFn=(fn () => (value := SOME newV; S.atomicEnd(); v))
		      }
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

  end; (* SyncVar *)
