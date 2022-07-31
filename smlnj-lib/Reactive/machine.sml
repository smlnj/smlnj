(* machine.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is an implementation of the reactive interpreter instructions,
 * and functions to generate them.
 *)

structure Machine : sig

  (* activation return codes *)
    datatype state
      = TERM		(* execution of the instruction is complete; activation
			 * at future instances has no effect.
			 *)
      | STOP		(* execution is stopped in this instant, but could
			 * progress in the next instant.
			 *)
      | SUSP		(* exeuction is suspended and must be resumed during this
			 * instant.
			 *)

    type in_signal
    type out_signal

    type instant = int
    type signal_state = instant ref

    datatype signal = SIG of {
	name : Atom.atom,
	id : int,
	state : signal_state
      }

    datatype machine = M of {
	now : instant ref,
	moveFlg : bool ref,
	endOfInstant : bool ref,
	prog : code,
	signals : signal list,
	inputs : signal list,
	outputs : signal list
      }

    and code = C of {
	isTerm		: unit -> bool,
	terminate	: unit -> unit,
	reset		: unit -> unit,
	preempt		: machine -> unit,
	activation	: machine -> state
      }

    val runMachine : machine -> bool
    val resetMachine : machine -> unit
    val inputsOf : machine -> in_signal list
    val outputsOf : machine -> out_signal list

    val inputSignal : in_signal -> Atom.atom
    val outputSignal : out_signal -> Atom.atom
    val setInSignal  : (in_signal * bool) -> unit
    val getInSignal  : in_signal -> bool
    val getOutSignal : out_signal -> bool

    type config

    val || : (code * code) -> code
    val &  : (code * code) -> code
    val nothing : code
    val stop : unit -> code
    val suspend : unit -> code
    val action : (machine -> unit) -> code
    val exec   : (machine -> {stop : unit -> unit, done : unit -> bool}) -> code
    val ifThenElse : ((machine -> bool) * code * code) -> code
    val repeat     : (int * code) -> code
    val loop       : code -> code
    val close      : code -> code
    val emit       : signal -> code
    val await      : config -> code
    val when       : (config * code * code) -> code
    val trapWith   : (config * code * code) -> code

  end = struct

    structure I = Instruction (* for the config type *)

    datatype state
      = TERM
      | STOP
      | SUSP

    type instant = int
    type signal_state = instant ref

    datatype signal = SIG of {
	name : Atom.atom,
	id : int,
	state : signal_state
      }

    type config = signal I.config

    datatype machine = M of {
	now : instant ref,
	moveFlg : bool ref,
	endOfInstant : bool ref,
	prog : code,
	signals : signal list,
	inputs : signal list,
	outputs : signal list
      }

    and code = C of {
	isTerm		: unit -> bool,
	terminate	: unit -> unit,
	reset		: unit -> unit,
	preempt		: machine -> unit,
	activation	: machine -> state
      }


    fun now (M{now=t, ...}) = !t
    fun newMove (M{moveFlg, ...}) = moveFlg := true
    fun isEndOfInstant (M{endOfInstant, ...}) = !endOfInstant

    datatype presence = PRESENT | ABSENT | UNKNOWN

    fun presence (m, SIG{state, ...}) = let
	  val ts = !state
	  val now = now m
	  in
	    if (now = ts) then PRESENT
	    else if ((now = ~ts) orelse (isEndOfInstant m)) then ABSENT
	    else UNKNOWN
	  end

    fun present (m, SIG{state, ...}) = (now m = !state)
    fun prePresent (m, SIG{state, ...}) = (now m = !state + 1)
    fun absent (m, SIG{state, ...}) = (now m = ~(!state))
    fun emitSig (m, SIG{state, ...}) = state := now m
    fun emitNot (m, SIG{state, ...}) = state := ~(now m)

    datatype in_signal = IN of (machine * signal)
    datatype out_signal = OUT of (machine * signal)

    fun inputSignal (IN(_, SIG{name, ...})) = name
    fun outputSignal (OUT(_, SIG{name, ...})) = name
    fun setInSignal (IN(m, s), false) = emitNot(m, s)
      | setInSignal (IN(m, s), true) = emitSig(m, s)
    fun getInSignal (IN(m, s)) = present(m, s)
    fun getOutSignal (OUT(m, s)) = prePresent(m, s)

    fun terminate (C{terminate=f, ...}) = f()
    fun isTerm (C{isTerm=f, ...}) = f()
    fun reset (C{reset=f, ...}) = f()
    fun preemption (C{preempt=f, ...}, m) = f m
    fun activation (C{activation=f, ...}, m) = f m

    fun activate (i, m) = if (isTerm i)
	  then TERM
	  else (case activation(i, m)
	     of TERM => (terminate i; TERM)
	      | res => res
	    (* end case *))

    fun preempt (i, m) = if (isTerm i)
	  then ()
	  else (preemption(i, m); terminate i)

  (* default instruction methods *)
    fun isTermMeth termFlg () = !termFlg
    fun terminateMeth termFlg () = termFlg := true
    fun resetMeth termFlg () = termFlg := false

    fun || (i1, i2) = let
	  val termFlg = ref false
	  val leftSts = ref SUSP
	  val rightSts = ref SUSP
	  fun resetMeth () = (
		termFlg := false; leftSts := SUSP; rightSts := SUSP;
		reset i1; reset i2)
	  fun preemptMeth m = (preempt(i1, m); preempt(i2, m))
	  fun activationMeth m = (
		if (!leftSts = SUSP) then leftSts := activate(i1, m) else ();
		if (!rightSts = SUSP) then rightSts := activate(i2, m) else ();
		case (!leftSts, !rightSts)
		 of (TERM, TERM) => TERM
		  | (SUSP, _) => SUSP
		  | (_, SUSP) => SUSP
		  | _ => (leftSts := SUSP; rightSts := SUSP; STOP)
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    fun & (i1, i2) = let
	  val termFlg = ref false
	  fun resetMeth () = (termFlg := false; reset i1; reset i2)
	  fun preemptMeth m = (preempt(i1, m); preempt(i2, m))
	  fun activationMeth m =
		if (isTerm i1)
		  then activate(i2, m)
		else (case activate(i1, m)
		   of TERM => activate(i2, m)
		    | res => res
		  (* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    val nothing = C{
	    isTerm	= fn () => true,
	    terminate	= fn () => (),
	    reset	= fn () => (),
	    preempt	= fn _ => (),
	    activation	= fn _ => TERM
	  }

    fun stop () = let
	  val termFlg = ref false
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth termFlg,
		preempt		= fn _ => (),
		activation	= fn _ => (termFlg := true; STOP)
	      }
	  end

    fun suspend () = let
	  val termFlg = ref false
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth termFlg,
		preempt		= fn _ => (),
		activation	= fn _ => (termFlg := true; SUSP)
	      }
	  end

    fun action f = let
	  val termFlg = ref false
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth termFlg,
		preempt		= fn _ => (),
		activation	= fn m => (f m; TERM)
	      }
	  end

    fun exec f = let
	  val termFlg = ref false
	  val ops = ref(NONE : {stop : unit -> unit, done : unit -> bool} option)
(** NOTE: what if a reset occurs while we are running?  We would need to change
 ** the type of resetMeth to take a machine parameter.
 **)
	  fun resetMeth () = (termFlg := false)
	  fun preemptMeth m = (case !ops
		 of NONE => ()
		  | SOME{stop, ...} => (ops := NONE; stop())
		(* end case *))
	  fun activationMeth m = (case !ops
		 of SOME{done, ...} => if done ()
		      then (ops := NONE; TERM)
		      else STOP
		  | NONE => (ops := SOME(f m); SUSP)
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    fun ifThenElse (pred, i1, i2) = let
	  val termFlg = ref false
	  val cond = ref NONE
	  fun resetMeth () = (
		termFlg := false;
		case !cond
		 of (SOME true) => reset i1
		  | (SOME false) => reset i2
		  | NONE => ()
		(* end case *);
		cond := NONE)
	  fun preemptMeth m = (case !cond
		 of (SOME true) => preempt(i1, m)
		  | (SOME false) => preempt(i2, m)
		  | NONE => ()
		(* end case *))
	  fun activationMeth m = (case !cond
		 of (SOME true) => activate(i1, m)
		  | (SOME false) => activate(i2, m)
		  | NONE => let
		      val b = pred m
		      in
			cond := SOME b;
			if b then activate(i1, m) else activate(i2, m)
		      end
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    fun repeat (n, i) = let
	  val termFlg = ref false
	  val counter = ref n
	  fun resetMeth () = (termFlg := false; counter := n)
	  fun preemptMeth m = preempt(i, m)
	  fun activationMeth m =
		if (!counter > 0)
		  then (case activate(i, m)
		     of TERM => (
			  counter := !counter-1; reset i;
			  activationMeth m)
		      | res => res
		    (* end case *))
		  else TERM
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    fun loop i = let
	  val termFlg = ref false
	  val endReached = ref false
	  fun resetMeth () = (termFlg := false; reset i; endReached := false)
	  fun preemptMeth m = preempt (i, m)
	  fun activationMeth m = (case activate(i, m)
		 of TERM => if (!endReached)
		      then (
(*			say(m, "instantaneous loop detected\n"); *)
			STOP)
		      else (endReached := true; reset i; activationMeth m)
		  | STOP => (endReached := false; STOP)
		  | SUSP => SUSP
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    fun close i = let
	  val termFlg = ref false
	  fun activationMeth m = (case activate(i, m)
		 of SUSP => activationMeth m
		  | res => res
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth termFlg,
		preempt		= fn _ => (),
		activation	= activationMeth
	      }
	  end

  (** Configuration evaluation **)
    fun fixed (m, c) = let
	  fun fix (I.posConfig id) = (presence(m, id) <> UNKNOWN)
	    | fix (I.negConfig id) = (presence(m, id) <> UNKNOWN)
	    | fix (I.orConfig(c1, c2)) = let
		val b1 = fix c1 and b2 = fix c2
		in
		  (b1 andalso evaluate(m, c1)) orelse
		  (b2 andalso evaluate(m, c2)) orelse
		  (b1 andalso b2)
		end
	    | fix (I.andConfig(c1, c2)) = let
		val b1 = fix c1 and b2 = fix c2
		in
		  (b1 andalso not(evaluate(m, c1))) orelse
		  (b2 andalso not(evaluate(m, c2))) orelse
		  (b1 andalso b2)
		end
	  in
	    fix c
	  end

    and evaluate (m, c) = let
	  fun eval (I.posConfig id) = present(m, id)
	    | eval (I.negConfig id) = not(present(m, id))
	    | eval (I.orConfig(c1, c2)) = eval c1 orelse eval c2
	    | eval (I.andConfig(c1, c2)) = eval c1 andalso eval c2
	  in
	    eval c
	  end

  (* evaluate the signal configuration `c` returning `SOME b` if
   * the signals are in known state and `b` is the result of the
   * configuration.  Otherwise, return `NONE` if one or more signals
   * are in unknown state.
   *)
    fun fixedEval (m, c) = let
	  fun f (I.posConfig id) = (case presence(m, id)
		 of UNKNOWN => NONE
		  | PRESENT => SOME true
		  | ABSENT => SOME false
		(* end case *))
	    | f (I.negConfig id) = (case presence(m, id)
		 of UNKNOWN => NONE
		  | PRESENT => SOME false
		  | ABSENT => SOME true
		(* end case *))
	    | f (I.andConfig(c1, c2)) = (case (f c1, f c2)
		 of (SOME false, _) => SOME false
		  | (_, SOME false) => SOME false
		  | (SOME true, SOME true) => SOME true
		  | _ => NONE
		(* end case *))
	    | f (I.orConfig(c1, c2)) = (case (f c1, f c2)
		 of (SOME true, _) => SOME true
		  | (_, SOME true) => SOME true
		  | (SOME false, SOME false) => SOME false
		  | _ => NONE
		(* end case *))
	  in
	    f c
	  end

    fun emit signal = let
	  val termFlg = ref false
	  fun activationMeth m = (
		newMove m;
		emitSig(m, signal);
		TERM)
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth termFlg,
		preempt		= fn _ => (),
		activation	= activationMeth
	      }
	  end

    fun await c = let
	  val termFlg = ref false
	  fun activationMeth m = (case fixedEval(m, c)
		 of NONE => SUSP
		  | (SOME false) => STOP
		  | (SOME true) => if (isEndOfInstant m) then STOP else TERM
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth termFlg,
		preempt		= fn _ => (),
		activation	= activationMeth
	      }
	  end

    fun when (c, i1, i2) = let
	  val termFlg = ref false
	  val value = ref NONE
	  fun resetMeth m = (
		termFlg := false;
		reset i1; reset i2;
		value := NONE)
	  fun preemptMeth m = (preempt(i1, m); preempt(i2, m))
	  fun activationMeth m = (case !value
		 of NONE => (case fixedEval(m, c)
		       of NONE => SUSP
			| (SOME v) => (
			    value := SOME v;
			    if (isEndOfInstant m)
			      then STOP
			    else if v
			      then activate(i1, m)
			      else activate(i2, m))
		     (* end case *))
		  | (SOME true) => activate(i1, m)
		  | (SOME false) => activate(i2, m)
		(* end case *))
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

    fun trapWith (c, i1, i2) = let
	  val termFlg = ref false
	  val activeHandle = ref false
	  val resumeBody = ref true
	  fun resetMeth m = (
		termFlg := false;
		reset i1; reset i2;
		activeHandle := false;
		resumeBody := true)
	  fun preemptMeth m = if (! activeHandle)
		then preempt(i2, m)
		else preempt(i1, m)
	  fun activationMeth m =
		if (! activeHandle)
		  then activate(i2, m)
		  else let
		    fun chkConfig () = (case fixedEval(m, c)
			   of NONE => SUSP
			    | (SOME true) => ( (* actual preemption *)
				preempt(i1, m);
				activeHandle := true;
				if (isEndOfInstant m)
				  then STOP
				  else activate(i2, m))
			    | (SOME false) => (
				resumeBody := true;
				STOP)
			  (* end case *))
		    in
		      if (! resumeBody)
			then (case activate(i1, m)
			   of STOP => (resumeBody := false; chkConfig())
			    | res => res
			  (* end case *))
			else chkConfig()
		    end
	  in
	    C{  isTerm		= isTermMeth termFlg,
		terminate	= terminateMeth termFlg,
		reset		= resetMeth,
		preempt		= preemptMeth,
		activation	= activationMeth
	      }
	  end

  (* run a machine to a stable state; return true if that is a terminal state *)
    fun runMachine (m as M{now, moveFlg, endOfInstant, prog, ...}) = let
	  fun untilStop () = (case activate(prog, m)
		 of SUSP => (
		      if !moveFlg
			then moveFlg := false
			else endOfInstant := true;
		      untilStop ())
		  | STOP => false
		  | TERM => true
		(* end case *))
	  in
	    endOfInstant := false;
	    moveFlg := false;
	    untilStop () before now := !now+1
	  end

  (* reset a machine back to its initial state *)
    fun resetMachine (M{
	    now, moveFlg, endOfInstant, prog, signals, inputs, outputs
	  }) = let
	  fun resetSig (SIG{state, ...}) = state := 0
	  in
	    now := 2;
	    moveFlg := false;
	    endOfInstant := false;
	    reset prog;
	    List.app resetSig signals;
	    List.app resetSig inputs;
	    List.app resetSig outputs
	  end

    fun inputsOf (m as M{inputs, ...}) = List.map (fn s => IN(m, s)) inputs
    fun outputsOf (m as M{outputs, ...}) = List.map (fn s => OUT(m, s)) outputs

  end;
