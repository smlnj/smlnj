(* reactive.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A simple ractive engine modelled after RC and SugarCubes.
 *)

structure Reactive : REACTIVE =
  struct

    structure I = Instruction
    structure M = Machine

    type machine = M.machine
    type instruction = machine Instruction.instr
    type signal = I.signal
    type config = I.signal I.config
    type in_signal = M.in_signal
    type out_signal = M.out_signal

  (* used to bind internal signal names *)
    structure AMap = AtomBinaryMap

    exception UnboundSignal of I.signal

    fun machine {inputs, outputs, body} = let
	  val nextId = ref 0
	  val sigList = ref []
	  fun newSignal s = let
		val id = !nextId
		val s' = M.SIG{name=s, id=id, state = ref 0}
		in
		  nextId := id+1;
		  sigList := s' :: !sigList;
		  s'
		end
	  fun bindSig (env, s) = (case AMap.find (env, s)
		 of NONE => raise UnboundSignal s
		  | (SOME s') => s'
		(* end case *))
	  fun trans (instr, env) = (case instr
		 of I.||(i1, i2) => M.||(trans(i1, env), trans(i2, env))
		  | I.&(i1, i2) => M.&(trans(i1, env), trans(i2, env))
		  | I.nothing => M.nothing
		  | I.stop => M.stop()
		  | I.suspend => M.suspend()
		  | I.action act => M.action act
		  | I.exec f => M.exec f
		  | I.ifThenElse(pred, i1, i2) =>
		      M.ifThenElse(pred, trans(i1, env), trans(i2, env))
		  | I.repeat(cnt, i) => M.repeat(cnt, trans(i, env))
		  | I.loop i => M.loop(trans(i, env))
		  | I.close i => M.close(trans(i, env))
		  | I.signal(s, i) => trans(i, AMap.insert(env, s, newSignal s))
		  | I.rebind(s1, s2, i) =>
		      trans(i, AMap.insert(env, s2, bindSig(env, s1)))
		  | I.emit s => M.emit(bindSig(env, s))
		  | I.await cfg => M.await(transConfig(cfg, env))
		  | I.when(cfg, i1, i2) =>
		      M.when(transConfig(cfg, env), trans(i1, env), trans(i2, env))
		  | I.trapWith(cfg, i1, i2) =>
		      M.trapWith(transConfig(cfg, env), trans(i1, env), trans(i2, env))
		(* end case *))
	  and transConfig (cfg, env) = let
		fun transCfg (I.posConfig s) = I.posConfig(bindSig(env, s))
		  | transCfg (I.negConfig s) = I.negConfig(bindSig(env, s))
		  | transCfg (I.orConfig(cfg1, cfg2)) =
		      I.orConfig(transCfg cfg1, transCfg cfg2)
		  | transCfg (I.andConfig(cfg1, cfg2)) =
		      I.andConfig(transCfg cfg1, transCfg cfg2)
		in
		  transCfg cfg
		end
	  val inputs' = List.map newSignal inputs
	  val outputs' = List.map newSignal outputs
	  fun ins (s as M.SIG{name, ...}, env) = AMap.insert(env, name, s)
	  val initialEnv =
		List.foldl ins (List.foldl ins AMap.empty inputs') outputs'
	  val body' = trans (body, initialEnv)
	  in
	    M.M{
		now = ref 0,
		moveFlg = ref false,
		endOfInstant = ref false,
		prog = body',
		signals = !sigList,
		inputs = inputs',
		outputs = outputs'
	      }
	  end

    val run = M.runMachine
    val reset = M.resetMachine
    val inputsOf = M.inputsOf
    val outputsOf = M.outputsOf
    val inputSignal = M.inputSignal
    val outputSignal = M.outputSignal
    val setInSignal = M.setInSignal
    val getInSignal = M.getInSignal
    val getOutSignal = M.getOutSignal

    val posConfig = I.posConfig
    val negConfig = I.negConfig
    val orConfig = I.orConfig
    val andConfig = I.andConfig

    val || = I.||
    val & = I.&
    val nothing = I.nothing
    val stop = I.stop
    val suspend = I.suspend
    val action = I.action
    val exec = I.exec
    val ifThenElse = I.ifThenElse
    val repeat = I.repeat
    val loop = I.loop
    val close = I.close
    val signal = I.signal
    val rebind = I.rebind
    val when = I.when
    val trapWith = I.trapWith
    fun trap (c, i) = I.trapWith(c, i, I.nothing)
    val emit = I.emit
    val await = I.await

  end
