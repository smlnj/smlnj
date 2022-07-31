(* backend-fn.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor BackendFn (

    structure M : CODE_GENERATOR

    val cproto_conv : string

  ) : BACKEND = struct

    structure Interact = Interact (
        EvalLoopF (
          CompileF (
	    val cproto_conv = cproto_conv
            structure M = M
            structure CC : CCONFIG = struct
                (* configuration for interactive toplevel:
                 * no real pickling/unpickling, pids are assigned randomly
                 *)
                type pickle = unit
                type hash = unit
                type pid = PersStamps.persstamp
                type guid = unit
                local
                  val topCount = ref 0
                in
                fun pickUnpick { context, env = newenv, guid } = let
                      val _ = topCount := !topCount + 1
                      val { newenv = newenv', hash, exportLvars, hasExports } =
                            PickMod.dontPickle { env = newenv, count = !topCount }
                      in {
                        pid = (),
                        pickle = (),
                        exportLvars = exportLvars,
                        exportPid = if hasExports then SOME hash else NONE,
                        newenv = newenv'
                      } end
                end (* local *)
                local
                  val stampGen = Stamps.newGenerator ()
                in
                fun mkMkStamp () = stampGen (* always the same *)
                end (* local *)
            end)))

    structure Compile = CompileF (
        val cproto_conv = cproto_conv
        structure M = M
        structure CC : CCONFIG = struct
            (* compiler configuration for batch compilation
             * (under control of CM); real pickling, unpickling, and
             * pid-generation
             *)
            type pickle = Word8Vector.vector
            type hash = PersStamps.persstamp
            type pid = hash
            type guid = string

            fun pickUnpick { context, env = newenv, guid } = let
                val m = GenModIdMap.mkMap context
                fun up_context _ = (m, fn () => "batch context")
                val { hash, pickle, exportLvars, hasExports } =
                      PickMod.pickleEnv (PickMod.INITIAL m) newenv
                val pid = Rehash.addGUID { hash = hash, guid = guid }
                val newenv' = UnpickMod.unpickleEnv up_context (pid, pickle)
                in {
                  pid = pid,
                  pickle = pickle,
                  exportLvars = exportLvars,
                  exportPid = if hasExports then SOME pid else NONE,
                  newenv = newenv'
                } end

            val mkMkStamp = Stamps.newGenerator
        end)

    structure Profile = ProfileFn (
        structure ProfEnv = ProfEnvFn (
            type env = Environment.environment
            val staticPart = Environment.staticPart
            val layer = Environment.concatEnv
            fun eval (s, e) = Interact.evalStream (TextIO.openString s, e))
            val pervasive = EnvRef.pervasive)

  (* the following are used by CM *)
    val architecture = M.architecture
    val abi_variant = M.abi_variant

  end
