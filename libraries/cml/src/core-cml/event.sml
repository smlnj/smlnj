(* event.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 * COPYRIGHT (c) 1989-1991 John H. Reppy
 *
 * The representation of event values and the event combinators.
 *
 * Some important requirements on the implementation of base event values:
 *
 *  1)	The pollFn, doFn, and blockFn are always called from inside
 *	atomic regions.
 *
 *  2)	The pollFn returns an integer priority: this is 0 when not enabled,
 *	~1 for fixed priority, and a positive value for dynamic priority.
 *	The standard scheme is to associate a counter with the underlying
 *	synchronization object, and to increase it by one for each
 *	synchronization attempt.
 *
 *  3)  The blockFn is responsible for exiting the atomic region; the doFns
 *	should NOT leave the atomic region.
 *
 *  4)  The blockFn is responsible for executing the "cleanUp" action
 *	prior to leaving the atomic region.
 *)

structure Event : sig

    include EVENT

    val atomicCVarSet : RepTypes.cvar -> unit
    val cvarGetEvt    : RepTypes.cvar -> unit event

  end = struct

    structure R = RepTypes
    structure S = Scheduler

    val capture = SMLofNJ.Cont.capture
    val escape = SMLofNJ.Cont.escape
    val callcc = SMLofNJ.Cont.callcc
    val throw = SMLofNJ.Cont.throw

  (* Some inline functions to improve performance *)
    fun map f = let
	  fun mapf [] = []
	    | mapf [a] = [f a]
	    | mapf [a, b] = [f a, f b]
	    | mapf [a, b, c] = [f a, f b, f c]
	    | mapf (a::b::c::d::r) = (f a)::(f b)::(f c)::(f d)::(mapf r)
	  in
	    mapf
	  end
    fun app f = let
	  fun appf [] = ()
	    | appf (x::r) = (f x; appf r)
	  in
	    appf
	  end
    fun foldl f init l = let
	  fun foldf ([], accum) = accum
	    | foldf (x::r, accum) = foldf(r, f(x, accum))
	  in
	    foldf (l, init)
	  end

    fun error msg = raise Fail msg

    datatype event_status = datatype RepTypes.event_status
    type 'a base_evt = 'a RepTypes.base_evt
    datatype event = datatype RepTypes.event


  (** Condition variables.  Because these variables are set inside atomic
   ** regions, we have to use different conventions for clean-up, etc.
   ** Instead of requiring the blockFn continuation to call the cleanUp
   ** action and to leave the atomic region, we call the cleanUp function
   ** when setting the condition variable (in atomicCVarSet), and have the
   ** invariant that the blockFn continuation is dispatched outside the
   ** atomic region.
   **)

  (* set a condition variable; we assume that this function is always
   * executed in an atomic region.
   *)
    fun atomicCVarSet (R.CVAR state) = (
	  case !state
	   of (R.CVAR_unset waiting) => let
		val R.Q{rear, ...} = S.rdyQ1
		fun add [] = !rear
		  | add ({transId=ref R.CANCEL, ...}::r) = add r
		  | add ({transId as ref(R.TRANS tid), cleanUp, kont}::r) = (
		      transId := R.CANCEL;
		      cleanUp();
		      (tid, kont) :: (add r))
		in
		  state := R.CVAR_set 1;
		  rear := add waiting
		end
	    | _ => error "cvar already set"
	  (* end case *))

  (* the event constructor for waiting on a cvar *)
    fun cvarGetEvt (R.CVAR(state)) = let
	  fun blockFn {transId, cleanUp, next} = callcc (fn k => let
		val (R.CVAR_unset waiting) = !state
		val item = {transId=transId, cleanUp=cleanUp, kont=k}
		in
		  state := R.CVAR_unset(item :: waiting);
		  next ()
		end)
	  fun pollFn () = (case !state
		 of (R.CVAR_set n) => let
		      fun doFn () = (state := R.CVAR_set 1; S.atomicEnd())
		      in
			state := R.CVAR_set(n+1);
			ENABLED{prio=n, doFn=doFn}
		      end
		  | _ => BLOCKED blockFn
		(* end case *))
	  in
	    BEVT[pollFn]
	  end


    fun alwaysEvt v = BEVT[fn () => R.ENABLED{
	    prio= ~1, doFn=fn () => (S.atomicEnd(); v)
	  }]

    val never = BEVT[]

    val guard = GUARD

    val withNack = W_NACK

    fun choose (el : 'a event list) = let
	  fun gatherBEvts ([], l) = BEVT l
	    | gatherBEvts (BEVT[] :: r, l) = gatherBEvts (r, l)
	    | gatherBEvts (BEVT[bev] :: r, bevs') = gatherBEvts (r, bev::bevs')
	    | gatherBEvts (BEVT bevs :: r, bevs') = gatherBEvts (r, bevs @ bevs')
	    | gatherBEvts (evts, []) = gather (evts, [])
	    | gatherBEvts (evts, l) = gather (evts, [BEVT l])
	  and gather ([], [evt]) = evt
	    | gather ([], evts) = CHOOSE evts
	    | gather (CHOOSE evts :: r, evts') = gather (r, evts @ evts')
	    | gather (BEVT bevs :: r, BEVT bevs' :: r')
		= gather (r, BEVT(bevs @ bevs') :: r')
	    | gather (evt :: r, evts') = gather (r, evt :: evts')
	  in
	    gatherBEvts (rev el, [])
	  end

    fun wrap (evt, wfn) = let
	  fun wrapBaseEvt pollFn () = (case pollFn()
		 of ENABLED{prio, doFn} => ENABLED{prio=prio, doFn = wfn o doFn}
		  | (BLOCKED blockFn) => BLOCKED(wfn o blockFn)
		(* end case *))
	  fun wrap' (BEVT bevs) = BEVT(map wrapBaseEvt bevs)
	    | wrap' (CHOOSE evts) = CHOOSE(map wrap' evts)
	    | wrap' (GUARD g) = GUARD(fn () => wrap(g(), wfn))
	    | wrap' (W_NACK f) = W_NACK(fn evt => wrap(f evt, wfn))
	  in
	    wrap' evt
	  end

    fun wrapHandler (evt, hfn) = let
	  fun wrap f x = ((f x) handle exn => hfn exn)
	  fun wrapBaseEvt pollFn () = (case pollFn()
		 of ENABLED{prio, doFn} => ENABLED{prio=prio, doFn = wrap doFn}
		  | (BLOCKED blockFn) => BLOCKED(wrap blockFn)
		(* end case *))
	  fun wrap' (BEVT bevs) = BEVT(map wrapBaseEvt bevs)
	    | wrap' (CHOOSE evts) = CHOOSE(map wrap' evts)
	    | wrap' (GUARD g) = GUARD(fn () => wrapHandler(g(), hfn))
	    | wrap' (W_NACK f) = W_NACK(fn evt => wrapHandler(f evt, hfn))
	  in
	    wrap' evt
	  end

    datatype 'a event_group
      = BASE_GRP of 'a base_evt list
      | GRP of 'a event_group list
      | NACK_GRP of (R.cvar * 'a event_group)

(*+DEBUG
fun sayGrp (msg, eg) = let
      fun f (BASE_GRP l, sl) = "BASE_GRP("::Int.toString(List.length l)::")"::sl
	| f (GRP l, sl) = "GRP(" :: g(l, ")"::sl)
	| f (NACK_GRP l, sl) = "NACK_GRP(" :: f(#2 l, ")"::sl)
      and g ([], sl) = sl
	| g ([x], sl) = f(x, sl)
	| g (x::r, sl) = f(x, "," :: g(r, sl))
      in
	Debug.sayDebugId(String.concat(msg :: ": " :: f(eg, ["\n"])))
      end
-DEBUG*)

  (* force the evaluation of any guards in an event group. *)
    fun force (BEVT l) = BASE_GRP l
      | force evt = let
	  fun force' (GUARD g) = force' (g ())
	    | force' (W_NACK f) = let
		val cvar = R.CVAR(ref(R.CVAR_unset []))
		in
		  NACK_GRP(cvar, force' (f (cvarGetEvt cvar)))
		end
	    | force' (BEVT grp) = BASE_GRP grp
	    | force' (CHOOSE evts) = let
		fun forceBL ([], bevs) = BASE_GRP bevs
		  | forceBL (evt::r, bevs') = (case (force' evt)
		       of (BASE_GRP bevs) => forceBL (r, bevs @ bevs')
			| (GRP grp) => forceL (r, grp @ [BASE_GRP bevs'])
			| grp => forceL (r, [grp, BASE_GRP bevs'])
		      (* end case *))
		and forceL ([], [grp]) = grp
		  | forceL ([], l) = GRP l
		  | forceL (evt :: r, l) = (
		      case (force' evt, l)
		       of (BASE_GRP bevs, BASE_GRP bevs' :: r') =>
			    forceL (r, BASE_GRP(bevs @ bevs') :: r')
			| (GRP grp, l) => forceL (r, grp @ l)
			| (grp, l) => forceL (r, grp :: l)
		      (* end case *))
		in
		  forceBL (evts, [])
		end
	  in
	    force' evt
	  end

    local
      val cnt = ref 0
      fun random i = let val j = !cnt
	    in
	      if (j = 1000000) then cnt := 0 else cnt := j+1;
	      Int.rem(j, i)
	    end
    in
    fun selectDoFn ([(_, doFn)], _) = doFn
      | selectDoFn (l, n) = let
	  fun priority ~1 = n
	    | priority p = p
	  fun max ((p, doFn)::r, maxP, k, doFns) = let
		val p = priority p
		in
		  if (p > maxP) then max(r, p, 1, [doFn])
		  else if (p = maxP) then max(r, maxP, k+1, doFn::doFns)
		  else max(r, maxP, k, doFns)
		end
	    | max ([], _, k, [doFn]) = doFn
	    | max ([], _, k, doFns) = List.nth(doFns, random k)
	  in
	    max (l, 0, 0, [])
	  end
    end

    fun mkFlg () = let val flg = ref(R.TRANS(S.getCurThread()))
	  in
	    (flg, fn () => flg := R.CANCEL)
	  end

    fun syncOnOneEvt (pollFn : 'a base_evt) = (
	  S.atomicBegin ();
	  case (pollFn())
	   of ENABLED{doFn, ...} => doFn()
	    | (BLOCKED blockFn) => let val (flg, setFlg) = mkFlg()
		in
		  blockFn{transId=flg, cleanUp=setFlg, next=S.atomicDispatch}
		end
	  (* end case *))

  (* this function handles the case of synchronizing on a list of
   * base events (w/o any negative acknowledgements).  It also handles
   * the case of synchronizing on NEVER.
   *)
    fun syncOnBEvts [] = S.dispatch()
      | syncOnBEvts [bev] = syncOnOneEvt bev
      | syncOnBEvts bevs = let
	  fun ext ([], blockFns) = capture (fn k => let
		val escape = escape k
		val (transId, setFlg) = mkFlg()
		fun log [] = S.atomicDispatch ()
		  | log (blockFn :: r) =
		      escape (blockFn {
			  transId = transId,
			  cleanUp = setFlg,
			  next = fn () => log r
			})
		in
		  log blockFns; error "[log]"
		end)
	    | ext (pollFn :: r, blockFns) = (case pollFn()
		 of ENABLED{prio, doFn} => extRdy (r, [(prio, doFn)], 1)
		  | (BLOCKED blockFn) => ext (r, blockFn::blockFns)
		(* end case *))
(** NOTE: maybe we should just keep track of the max priority?
 ** What about fairness to fixed priority events (e.g., always, timeout?)
 **)
	  and extRdy ([], doFns, n) = selectDoFn (doFns, n) ()
	    | extRdy (pollFn :: r, doFns, n) = (case pollFn()
		 of ENABLED{prio, doFn} => extRdy (r, (prio, doFn)::doFns, n+1)
		  | _ => extRdy (r, doFns, n)
		(* end case *))
	  in
	    S.atomicBegin();
	    ext (bevs, [])
	  end

  (* walk the event group tree, collecting the base events (with associated
   * ack flags), and a list of flag sets.  A flag set is a (cvar * ack flag list)
   * pair, where the flags are those associated with the events covered by the
   * nack cvar.
   *)
    fun collect grp = let
	  val unWrappedFlg = ref false
	  fun gatherWrapped (grp, bl, flgSets) = let
		fun gather (BASE_GRP bevs, bl, allFlgs, flgSets) = let
		      fun append ([], bl, allFlgs) = (bl, allFlgs)
			| append (bev::r, bl, allFlgs) = let
			    val flg = ref false
			    in
			      append (r, (bev, flg)::bl, flg::allFlgs)
			    end
		      val (bl', allFlgs') = append (bevs, bl, allFlgs)
		      in
		        (bl', allFlgs', flgSets)
		      end
		  | gather (GRP grp, bl, allFlgs, flgSets) = let
		      fun f (grp', (bl', allFlgs', flgSets')) =
			    gather (grp', bl', allFlgs', flgSets')
		      in
			foldl f (bl, allFlgs, flgSets) grp
		      end
		  | gather (NACK_GRP(cvar, grp), bl, allFlgs, flgSets) = let
		      val (bl', allFlgs', flgSets') =
			    gather (grp, bl, [], flgSets)
		      in
			(bl', allFlgs' @ allFlgs, (cvar, allFlgs') :: flgSets')
		      end
		val (bl, _, flgSets) = gather (grp, bl, [], flgSets)
		in
		  (bl, flgSets)
		end
	  in
	    case grp
	     of (GRP _) => let
		  val unWrappedFlg = ref false
		  fun append ([], bl) = bl
		    | append (bev::r, bl) = append(r, (bev, unWrappedFlg)::bl)
		  fun gather (BASE_GRP bevs, bl, flgSets) =
			(append(bevs, bl), flgSets)
		    | gather (GRP grp, bl, flgSets) = let
			fun f (grp', (bl', flgSets')) =
			      gather(grp', bl', flgSets')
			in
			  foldl f (bl, flgSets) grp
			end
		    | gather (grp as NACK_GRP _, bl, flgSets) =
			gatherWrapped (grp, bl, flgSets)
		  in
		    gather (grp, [], [])
		  end
	      | grp => gatherWrapped (grp, [], [])
	    (* end case *)
	  end

  (* this function handles the more complicated case of synchronization
   * on groups of events where negative acknowledgements are involved.
   *)
    fun syncOnGrp grp = let
	  val (bl, flgSets) = collect grp
	  fun chkCVars () = let
	      (* chkCVar checks the flags of a flag set.  If they are all false
	       * then the corresponding cvar is set to signal the negative ack.
	       *)
		fun chkCVar (cvar, flgs) = let
		      fun chkFlgs [] = atomicCVarSet cvar
			| chkFlgs ((ref true)::_) = ()
			| chkFlgs (_::r) = chkFlgs r
		      in
			chkFlgs flgs
		      end
		in
		  app chkCVar flgSets
		end
	  fun ext ([], blockFns) = capture (fn k => let
		val escape = escape k
		val transId = ref(R.TRANS(S.getCurThread()))
		fun setFlg flg () = (
		      transId := R.CANCEL; flg := true; chkCVars())
		fun log [] = S.atomicDispatch ()
		  | log ((blockFn, flg) :: r) =
		      escape (blockFn {
			  transId = transId,
			  cleanUp = setFlg flg,
			  next = fn () => log r
			})
		in
		  log blockFns; error "[log]"
		end)
	    | ext ((pollFn, flg) :: r, blockFns) = (case pollFn()
		 of ENABLED{prio, doFn} => extRdy (r, [(prio, (doFn, flg))], 1)
		  | (BLOCKED blockFn) => ext (r, (blockFn, flg)::blockFns)
		(* end case *))
(** NOTE: maybe we should just keep track of the max priority?
 ** What about fairness to fixed priority events (e.g., always, timeout?)
 **)
	  and extRdy ([], doFns, n) = let
		val (doFn, flg) = selectDoFn (doFns, n)
		in
		  flg := true;
		  chkCVars ();
		  doFn()
		end
	    | extRdy ((pollFn, flg) :: r, doFns, n) = (case pollFn()
		 of ENABLED{prio, doFn} =>
		      extRdy (r, (prio, (doFn, flg))::doFns, n+1)
		  | _ => extRdy (r, doFns, n)
		(* end case *))
	  in
	    S.atomicBegin(); ext (bl, [])
	  end

    fun sync ev = (case (force ev)
	   of (BASE_GRP bevs) => syncOnBEvts bevs
	    | grp => syncOnGrp grp
	  (* end case *))

    fun select evts = let
	  fun forceBL ([], bevs) = BASE_GRP bevs
	    | forceBL (evt::r, bevs') = (case (force' evt)
		       of (BASE_GRP bevs) => forceBL (r, bevs @ bevs')
			| (GRP grp) => forceL (r, grp @ [BASE_GRP bevs'])
			| grp => forceL (r, [grp, BASE_GRP bevs'])
		      (* end case *))
	  and forceL ([], [grp]) = grp
	    | forceL ([], l) = GRP l
	    | forceL (evt :: r, l) = (
		case (force' evt, l)
		 of (BASE_GRP bevs, BASE_GRP bevs' :: r') =>
		      forceL (r, BASE_GRP(bevs @ bevs') :: r')
		  | (GRP grp, l) => forceL (r, grp @ l)
		  | (grp, l) => forceL (r, grp :: l)
		(* end case *))
	  and force' (GUARD g) = force' (g ())
	    | force' (W_NACK f) = let
		val cvar = R.CVAR(ref(R.CVAR_unset []))
		in
		  NACK_GRP(cvar, force' (f (cvarGetEvt cvar)))
		end
	    | force' (BEVT grp) = BASE_GRP grp
	    | force' (CHOOSE evts) = forceBL (evts, [])
	  in
	    case forceBL(evts, [])
	     of (BASE_GRP bevs) => syncOnBEvts bevs
	      | grp => syncOnGrp grp
	    (* end case *)
	  end

  end;

