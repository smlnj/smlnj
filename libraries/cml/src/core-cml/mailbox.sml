(* mailbox.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * Asynchronous channels (called mailboxes).
 *)

structure Mailbox : sig

    include MAILBOX

    val resetMbox : 'a mbox -> unit

  end = struct

    structure R = RepTypes
    structure S = Scheduler

    type 'a cont = 'a SMLofNJ.Cont.cont
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

    type 'a queue = {front : 'a list, rear : 'a list}

    fun enqueue ({front, rear}, x) = {front=front, rear=x::rear}

    fun dequeue ({front=x::r, rear}) = ({front=r, rear=rear}, x)
      | dequeue ({front=[], rear}) = dequeue{front=List.rev rear, rear=[]}

  (* the state of a mailbox.  The queue of the NONEMPTY constructor should
   * never be empty (use EMPTY instead).
  *)
    datatype 'a state
      = EMPTY of (R.trans_id ref * 'a cont) queue
      | NONEMPTY of (int * 'a queue)

    datatype 'a mbox = MB of 'a state ref

    fun resetMbox (MB state) = (state := EMPTY{front=[], rear=[]})

    fun mailbox () = MB(ref(EMPTY{front=[], rear=[]}))

    fun sameMailbox (MB s1, MB s2) = (s1 = s2)

  (* create a new transaction ID *)
    fun mkId () = ref(R.TRANS(S.getCurThread()))

  (* given a transaction ID, get its thread ID and mark it cancelled. *)
    fun getIdFromTrans (transId as ref(R.TRANS tid)) = (
	  transId := R.CANCEL;
	  tid)

    datatype 'a q_item
      = NoItem
      | Item of (R.trans_id ref * 'a cont * 'a state)

    local
      fun clean [] = []
	| clean ((ref R.CANCEL, _)::r) = clean r
	| clean l = l
      fun cleanRev ([], l) = l
	| cleanRev ((ref R.CANCEL, _)::r, l) = cleanRev (r, l)
	| cleanRev (x::r, l) = cleanRev (r, x::l)
    in
    fun cleanAndRemove (q as {front, rear}) = let
	  fun cleanFront [] = cleanRear rear
	    | cleanFront f = (case (clean f)
		 of [] => cleanRear rear
		  | ((id, k)::rest) => Item(id, k, EMPTY{front=rest, rear=rear})
		(* end case *))
	  and cleanRear [] = NoItem
	    | cleanRear r = (case (cleanRev (r, []))
		 of [] => NoItem
		  | ((id, k)::rest) => Item(id, k, EMPTY{front=rest, rear=[]})
		(* end case *))
	  in
	    cleanFront front
	  end
    end

    fun send (MB state, x) = (
	  S.atomicBegin();
	  case !state
	   of (EMPTY q) => (case (cleanAndRemove q)
		 of NoItem => (
		      state := NONEMPTY(1, {front=[x], rear=[]});
		      S.atomicEnd())
		  | (Item(transId, recvK, state')) => callcc (fn k => (
		      state := state';
		      S.enqueueAndSwitchCurThread(k, getIdFromTrans transId);
		      throw recvK x))
		(* end case *))
	    | NONEMPTY(p, q) => 
	      (* we force a context switch here to prevent a producer from
	       * outrunning a consumer.
	       *)
		callcc (fn k => (
		  state := NONEMPTY(p, enqueue(q, x));
		  S.atomicYield k))
	  (* end case *))

    fun getMsg (state, q) = let
	  val (q', msg) = dequeue q
	  in
	    case q'
	     of {front=[], rear=[]} => state := EMPTY{front=[], rear=[]}
	      | _ => state := NONEMPTY(1, q')
	    (* end case *);
	    S.atomicEnd();
	    msg
	  end

    fun recv (MB state) = (
	  S.atomicBegin();
	  case !state
	   of (EMPTY q) => let
		val msg = callcc (fn recvK => (
		      state := EMPTY(enqueue(q, (mkId(), recvK)));
		      S.atomicDispatch()))
		in
		  S.atomicEnd(); msg
		end
	    | (NONEMPTY(priority, q)) => getMsg (state, q)
	  (* end case *))

    fun recvEvt (MB state) = let
	  fun blockFn {transId, cleanUp, next} = let
		val (EMPTY q) = !state
		val msg = callcc (fn recvK => (
		      state := EMPTY(enqueue(q, (transId, recvK)));
		      next();
		      raise Fail "Mailbox: impossible"))
		in
		  cleanUp();
		  S.atomicEnd();
		  msg
		end
	  fun pollFn () = (case !state
		 of (EMPTY _) => R.BLOCKED blockFn
		  | (NONEMPTY(priority, q)) => (
		      state := NONEMPTY(priority+1, q);
		      R.ENABLED{prio=priority, doFn=(fn () => getMsg(state, q))})
		(* end case *))
	  in
	    R.BEVT[pollFn]
	  end

    fun recvPoll (MB state) = (
	  S.atomicBegin();
	  case !state
	   of (EMPTY q) => (S.atomicEnd(); NONE)
	    | (NONEMPTY(priority, q)) => SOME(getMsg (state, q))
	  (* end case *))

  end (* Mailbox *)
