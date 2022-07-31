(* internal-signals.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the internal view of the Signals structure.
 *)

local
    structure String = StringImp
    structure Int = IntImp
in
structure InternalSignals : sig

    include SIGNALS

    val initSigTbl  : 'a -> unit
    val clearSigTbl : 'a -> unit
    val resetSigTbl : 'a -> unit

  end = struct

    structure CI = CInterface

    fun signalFn x = CI.c_function "SMLNJ-Signals" x

    datatype signal = SIG of CI.system_const

    datatype sig_action
      = IGNORE
      | DEFAULT
      | HANDLER of (signal * int * unit PrimTypes.cont) -> unit PrimTypes.cont

    fun sigToConst (SIG sc) = sc
    fun constToSig sc = SIG sc

  (* the list of supported signals, its length, and the maximum signal code.
   * We assume that the signal codes do not change, but that the number of
   * supported signals might vary between versions of the run-time system.
   *)
    type sig_info = {act : sig_action, mask : int, signal : signal}
    local
      val listSignals' : unit -> CI.system_const list = signalFn "listSignals"
      fun findMax sigs =
	    List.foldl
	      (fn (SIG(sigId, _), id) => if (id < sigId) then sigId else id)
		~1 sigs
    in
    val sigList : signal list ref = ref []
    val numSigs = ref 0
    val maxSig = ref ~1
    val sigTbl : sig_info option Array.array ref =
	  ref(Array.fromList[])
(** DEBUG **)
val debug : string -> unit = CInterface.c_function "SMLNJ-RunT" "debug"
fun getInfo sigId = (case (Array.sub(!sigTbl, sigId))
       of NONE => (
	    debug(String.concat[
		"\n*** Internal error: undefined sigTbl entry for signal ",
		Int.toString sigId, " ***\n"
	      ]);
	    raise Option.Option)
	| (SOME info) => info
      (* end case *))
(****
    fun getInfo sigId = (case (Array.sub(!sigTbl, sigId))
	   of NONE => raise Option.Option
	    | (SOME info) => info
	  (* end case *))
****)
    fun setInfo (sigId, info) = Array.update(!sigTbl, sigId, SOME info)
    fun resetList () = (
	  sigList := List.map constToSig (listSignals' ());
	  numSigs := List.length(! sigList);
	  maxSig := findMax(!sigList);
	  sigTbl := Array.array(!maxSig + 1, NONE))
    end

  (* list the signals (and their names) supported by this version *)
    fun listSignals () = (! sigList)

  (* return the name of a signal *)
    fun toString (SIG(_, name)) = name

  (* return the signal with the corresponding name; returns NONE, if
   * no such signal exists.
   *)
    fun fromString name = (
	  case CI.findSysConst (name, List.map sigToConst (!sigList))
	   of NONE => NONE
	    | (SOME sc) => SOME(SIG sc)
	  (* end case *))

  (* these run-time functions deal with the state of a signal in the system. *)
    val getSigState : CI.system_const -> int		= signalFn "getSigState"
    val setSigState : (CI.system_const * int) -> unit	= signalFn "setSigState"
  (* The states are defined as: *)
    val ignoreSigState = 0
    val defaultSigState = 1
    val enabledSigState = 2

  (* clear the signal table of handlers *)
    fun clearSigTbl _ = Array.modify (fn _ => NONE) (!sigTbl)

  (* initialize the signal table to the inherited process environment *)
    fun initSigTbl _ = let
	  fun initSig (s as (SIG sigId)) = let
		val state = getSigState sigId
		fun setState st = setInfo (#1 sigId, {act=st, mask=0, signal=s})
		in
		  if (state = ignoreSigState) then setState IGNORE
		  else if (state = defaultSigState) then setState DEFAULT
		  else (* state = enabledSigState *)
		    raise Fail "unexpected signal handler"
		end
	  in
	    resetList ();
	    List.app initSig (! sigList)
	  end

  (* reset the signal environment to agree with the signal table *)
    fun resetSigTbl _ = let
	  val oldSigTbl = !sigTbl
	  val oldList = !sigList
	  fun copy (SIG sigId) = (case (Array.sub(oldSigTbl, #1 sigId))
		 of NONE => ()
		  | (SOME info) => (
		      setInfo (#1 sigId, info);
		      case (#act info)
		       of IGNORE => setSigState(sigId, ignoreSigState)
			| DEFAULT => setSigState(sigId, defaultSigState)
			| (HANDLER _) => setSigState(sigId, enabledSigState)
		      (* end case *))
		(* end case *))
(** NOTE: we should probably notify the user that old signal handlers
 ** are being lost, but there is no good way to do this right now.
 **)
		  handle _ => ()
	  in
	    resetList ();
	    List.app copy oldList
	  end

  (* signal masking. *)

    datatype sigmask
      = MASKALL
      | MASK of signal list

    local
    (* Run-time system API:
     *   NONE   -- empty mask
     *   SOME[] -- mask all signals
     *   SOME l -- mask the signals in l
     *)
      val setSigMask   : CI.system_const list option -> unit = signalFn "setSigMask"
      val getSigMask : unit -> CI.system_const list option = signalFn "getSigMask"
    (* sort a list of signals eliminating duplicates *)
      fun sortSigs MASKALL = !sigList
	| sortSigs (MASK l) = let
	  (* a simple insertion sort to eliminate duplicates *)
	    fun insert (s as SIG(id, _), []) = [s]
	      | insert (s as SIG(id, _), (s' as SIG(id', _))::r) =
		  if (id < id')
		    then s :: s' :: r
		  else if (id = id')
		    then s' :: r
		    else s' :: insert(s, r)
	    in
	      List.foldl insert [] l
	    end
  (* map a list of signals into the format expected by the runtime system API *)
    fun makeMask (masked, nMasked) =
	  if (nMasked = 0) then NONE
	  else if (nMasked = !numSigs) then SOME[]
	  else SOME masked
  (* is the signal masked? *)
    fun isMasked (SIG(id, _)) = (#mask(getInfo id) > 0)
    in
    fun maskSignals mask = let
	  val sigs = sortSigs mask
	(* function for incrementing a signal mask. *)
	  fun incMask (SIG(sigId, _)) = let
		val {act, mask, signal} = getInfo sigId
		in
		  setInfo(sigId, {act=act, mask=mask+1, signal=signal})
		end
	(* scan over the sorted mask list and the list of all signals.  Record which signals are masked
	 * and how many new signals are masked.
	 *)
	  fun computeNewMask ([], _, _, _, 0) =
	      (* no signals are masked, so we only update the local state *)
		List.app incMask sigs
	    | computeNewMask ([], [], masked, nMasked, _) = (
	      (* NOTE: we must update the OS's view of the mask before we change
	       * our own to avoid a race condition!
	       *)
		setSigMask (makeMask (masked, nMasked));
		List.app incMask sigs)
	    | computeNewMask ([], s2::r2, masked, nMasked, nNew) =
		if isMasked s2
		  then computeNewMask ([], r2, (sigToConst s2)::masked, nMasked+1, nNew)
		  else computeNewMask ([], r2, masked, nMasked, nNew)
	    | computeNewMask ((s1 as SIG(id1, _))::r1, (s2 as SIG(id2, _))::r2, masked, nMasked, nNew) =
		if (id1 = id2)
		  then let
		    val nNew = if (isMasked s1) then nNew else nNew+1
		    in
		      computeNewMask (r1, r2, (sigToConst s1)::masked, nMasked+1, nNew)
		    end
		  else if (isMasked s2)
		    then computeNewMask (s1::r1, r2, (sigToConst s2)::masked, nMasked+1, nNew)
		    else computeNewMask (s1::r1, r2, masked, nMasked, nNew)
	    | computeNewMask (_::_, [], _, _, _) =
	        raise Fail "computeNewMask: bogus mask (impossible)"
	  in
	    computeNewMask (sigs, !sigList, [], 0, 0)
	  end
    fun unmaskSignals mask = let
	  val sigs = sortSigs mask
	(* function for decrementing a signal mask. *)
	  fun decMask (SIG(sigId, _)) = let
		val {act, mask, signal} = getInfo sigId
		in
		  if (mask > 0)
		    then setInfo(sigId, {act=act, mask=mask-1, signal=signal})
		    else ()
		end
	(* return true if decrementing this signal's count will unmask it. *)
	  fun isUnmasked (SIG(id, _)) = (#mask(getInfo id) <= 1)
	(* scan over the sorted mask list and the list of all signals.  Record which signals
	 * are masked and how many new signals are unmasked.
	 *)
	  fun computeNewMask ([], _, _, _, 0) =
	      (* no signals are unmasked, so we only update the local state *)
		List.app decMask sigs
	    | computeNewMask ([], [], masked, nMasked, _) = (
	      (* NOTE: we must update are local view of the mask before we change the OS's view
	       * to avoid a race condition!
	       *)
		List.app decMask sigs;
		setSigMask (makeMask (masked, nMasked)))
	    | computeNewMask ([], s2::r2, masked, nMasked, nNew) =
		if isMasked s2
		  then computeNewMask ([], r2, (sigToConst s2)::masked, nMasked+1, nNew)
		  else computeNewMask ([], r2, masked, nMasked, nNew)
	    | computeNewMask ((s1 as SIG(id1, _))::r1, (s2 as SIG(id2, _))::r2, masked, nMasked, nNew) =
		if (id1 = id2)
		  then if (isUnmasked s1)
		    then computeNewMask (r1, r2, masked, nMasked, nNew+1)
		    else computeNewMask (r1, r2, (sigToConst s1)::masked, nMasked+1, nNew) (* still masked *)
		  else if (isMasked s2)
		    then computeNewMask (s1::r1, r2, (sigToConst s2)::masked, nMasked+1, nNew)
		    else computeNewMask (s1::r1, r2, masked, nMasked, nNew)
	    | computeNewMask (_::_, [], _, _, _) =
	        raise Fail "unmaskSignals: bogus mask (impossible)"
	  in
	    computeNewMask (sigs, !sigList, [], 0, 0)
	  end
    fun masked () = (case getSigMask()
	   of NONE => MASK[]
	    | SOME[] => MASKALL
	    | SOME l => MASK(List.map constToSig l)
	  (* end case *))
    end (* local *)

  (* set the handler for a signal, returning the previous action. *)
    fun setHandler (s as (SIG sigId), act) = let
	  val _ = maskSignals MASKALL
	  val {act=oldAct, mask, ...} = getInfo(#1 sigId)
	  in
	    case (act, oldAct)
	     of (IGNORE, IGNORE) => ()
	      | (DEFAULT, DEFAULT) => ()
	      | (HANDLER _, HANDLER _) =>
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s})
	      | (IGNORE, _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, ignoreSigState))
	      | (DEFAULT, _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, defaultSigState))
	      | (HANDLER _, _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, enabledSigState))
	    (* end case *);
	    unmaskSignals MASKALL;
	    oldAct
	  end

  (* if a signal is not being ignored, then set the handler.  This
   * returns the previous handler (if IGNORE, then the current handler
   * is still IGNORE).
   *)
    fun overrideHandler (s as (SIG sigId), act) = let
	  val _ = maskSignals MASKALL
	  val {act=oldAct, mask, ...} = getInfo(#1 sigId)
	  in
	    case (oldAct, act)
	     of (IGNORE, _) => ()
	      | (DEFAULT, DEFAULT) => ()
	      | (HANDLER _, HANDLER _) =>
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s})
	      | (_, IGNORE) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, ignoreSigState))
	      | (_, DEFAULT) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, defaultSigState))
	      | (_, HANDLER _) => (
		  setInfo(#1 sigId, {act=act, mask=mask, signal=s});
		  setSigState(sigId, enabledSigState))
	    (* end case *);
	    unmaskSignals MASKALL;
	    oldAct
	  end

  (* get the current action for the given signal *)
    fun inqHandler (SIG(sigId, _)) = #act(getInfo sigId)

  (* sleep until the next signal; if called when signals are masked,
   * then signals will still be masked when pause returns.
   *)
    val pause : unit -> unit = signalFn "pause"

  (* Here is the ML handler that gets invoked by the run-time system.
   * It is responsible for dispatching the appropriate ML handler.
   *)
    fun sigHandler (code, count, resume_k) =
	(case (Array.sub(!sigTbl, code))
	  of (SOME{act=HANDLER handler, mask=0, signal}) =>
	     handler(signal, count, resume_k)
(*DEBUG
	     | _ => raise Fail "inconsistent internal signal state"
DEBUG*)
	   | info => let
		 val act = (case info
			     of NONE => "NONE"
			      | SOME{act=IGNORE, ...} => "IGNORE"
			      | SOME{act=DEFAULT, ...} => "DEFAULT"
			      | SOME{act=HANDLER _, mask, ... } => 
				concat ["HANDLER(mask=",Int.toString mask,
					"<>0)"]
				(*end case *))
		 val msg = concat["inconsistent state ", act,
				  " for signal ", Int.toString code]
	     in raise Fail msg
	     end
	    (* end case *))

  (* Install the root handler *)
    val _ = (Assembly.sighandler := sigHandler)

  (* initialize the signal list and table *)
    val _ = initSigTbl()

  (* these signals should be supported even on non-UNIX platforms. *)
    val sigINT  = Option.valOf(fromString "INT")
    val sigALRM = Option.valOf(fromString "ALRM")
    val sigTERM = Option.valOf(fromString "TERM")
    val sigGC   = Option.valOf(fromString "GC")

  end (* Signals *)
end (* local *)
