(* thread.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *)

structure Thread : sig
    include THREAD
    val defaultExnHandler : (exn -> unit) ref
    val reset : bool -> unit
  end = struct

    structure R = RepTypes
    structure S = Scheduler

    datatype thread_id = datatype R.thread_id
    datatype cvar = datatype R.cvar
    datatype cvar_state = datatype R.cvar_state

    type 'a event = 'a R.event

    local
      val tidCount = ref 0
      fun cvar () = CVAR(ref(CVAR_unset []))
    in

    fun reset running = (
	  tidCount := 0;
	  S.reset running)

    fun exnHandler (exn : exn) = ()

    val defaultExnHandler = ref exnHandler

    fun newTId () = let val n = !tidCount
	  in
	    tidCount := n+1;
	    TID{
		id = n,
		alert = ref false,
		done_comm = ref false,
		exnHandler = ref(! defaultExnHandler),
		props = ref[],
		dead = cvar()
	      }
	  end
    end (* local *)

    fun sameTid (TID{id=a, ...}, TID{id=b, ...}) = (a = b)

    fun compareTid (TID{id=a, ...}, TID{id=b, ...}) = Int.compare(a, b)

    fun hashTid (TID{id, ...}) = Word.fromInt id

    val tidToString = R.tidToString

    fun notifyAndDispatch (TID{dead, ...}) = (
	  S.atomicBegin(); Event.atomicCVarSet dead; S.atomicDispatch())

    fun doHandler (TID{exnHandler, ...}, exn) =
	  ((!exnHandler) exn) handle _ => ()

(** Eventually, this should be:
    fun spawnc f x = let
	  val _ = S.atomicBegin()
	  val id = newTId()
	  fun thread () = (
		(f x) handle ex => doHandler(id, ex);
		notifyAndDispatch id)
	  in
	    SMLofNJ.Cont.callcc (fn parentK => (
	      S.enqueueAndSwitchCurThread(parentK, id);
	      S.atomicEnd();
	      SMLofNJ.Cont.throw (SMLofNJ.Cont.isolate thread) ()));
	    id
	  end
 **)
    fun spawnc f x = let
	  val _ = S.atomicBegin()
	  val id = newTId()
	  in
	    SMLofNJ.Cont.callcc (fn parentK => (
	      S.enqueueAndSwitchCurThread(parentK, id);
	      S.atomicEnd();
	      (f x) handle ex => doHandler(id, ex);
	      notifyAndDispatch id));
	    id
	  end

    fun spawn f = spawnc f ()

    fun joinEvt (TID{dead, ...}) = Event.cvarGetEvt dead

    val getTid = S.getCurThread

    fun exit () = let
	  val (tid as TID{props, ...}) = getTid()
	  in
	    props := [];
	    notifyAndDispatch tid
	  end

    fun yield () = SMLofNJ.Cont.callcc (fn k => (
	  S.atomicBegin();
	  S.atomicYield k))

  (* thread-local data *)
    local
      fun mkProp () = let
	    exception E of 'a 
	    fun cons (a, l) = E a :: l 
	    fun peek [] = NONE
	      | peek (E a :: _) = SOME a
	      | peek (_ :: l) = peek l
	    fun delete [] = []
	      | delete (E a :: r) = r
	      | delete (x :: r) = x :: delete r
	    in
	      { cons = cons, peek = peek, delete = delete }
	    end
      fun mkFlag () = let
	    exception E
	    fun peek [] = false
	      | peek (E :: _) = true
	      | peek (_ :: l) = peek l
	    fun set (l, flg) = let
		  fun set ([], _) = if flg then E::l else l
		    | set (E::r, xs) = if flg then l else List.revAppend(xs, r)
		    | set (x::r, xs) = set (r, x::xs)
		  in
		    set (l, [])
		  end
	    in
	      { set = set, peek = peek }
	    end
      fun getProps () = let val TID{props, ...} = getTid() in props end
    in
    fun newThreadProp (init : unit -> 'b) = let
	  val {peek, cons, delete} = mkProp() 
	  fun peekFn () = peek(!(getProps()))
	  fun getF () = let
		val h = getProps()
		in
		  case peek(!h)
		   of NONE => let val b = init() in h := cons(b, !h); b end
		    | (SOME b) => b
		  (* end case *)
		end
	  fun clrF () = let
		val h = getProps()
		in
		  h := delete(!h)
		end
	  fun setFn x = let
		val h = getProps()
		in
		  h := cons(x, delete(!h))
		end
	  in
	    {peekFn = peekFn, getFn = getF, clrFn = clrF, setFn = setFn}
	  end

    fun newThreadFlag () = let
	  val {peek, set} = mkFlag() 
	  fun getF ()= peek(!(getProps()))
	  fun setF flg = let
		val h = getProps()
		in
		  h := set(!h, flg)
		end
	  in
	    {getFn = getF, setFn = setF}
	  end
    end (* local *)

  end;
