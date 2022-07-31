(* cconfig.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CCONFIG = sig

    type pickle
    type hash
    type pid = PersStamps.persstamp
    type guid

    val pickUnpick : {
	    context: StaticEnv.staticEnv,
	    env: StaticEnv.staticEnv,
	    guid: guid
	  } -> {
	    pid: hash,
	    pickle: pickle,
	    exportLvars: LambdaVar.lvar list,
	    exportPid: pid option,
	    newenv: StaticEnv.staticEnv
	  }

    val mkMkStamp : unit -> Stamps.generator

  end (* signature CCONFIG *)
