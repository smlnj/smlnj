(* Elaborator/modules/sigmatch.sig *)

signature SIGMATCH =
sig

  (*** these four functions are only called inside elabmod.sml ***)
  val matchStr : 
       {sign     : Modules.Signature, 
        str      : Modules.Structure,
        strExp   : Modules.strExp,     (* entity exp for str *)
        evOp     : EntPath.entVar option,
        tdepth   : DebIndex.depth,
        entEnv   : Modules.entityEnv,
        statenv  : StaticEnv.staticEnv,
        rpath    : InvPath.path,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo}
    -> {resDec : Absyn.dec,
        resStr : Modules.Structure,
        resExp : Modules.strExp}

  val matchFct : 
       {sign     : Modules.fctSig,
        fct      : Modules.Functor,
        fctExp   : Modules.fctExp,   (* entity exp for fct *)
        tdepth   : DebIndex.depth,
        entEnv   : Modules.entityEnv,
        statenv  : StaticEnv.staticEnv,
        rpath    : InvPath.path,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo}
    -> {resDec : Absyn.dec,
        resFct : Modules.Functor,
        resExp : Modules.fctExp}

  val applyFct : 
       {fct      : Modules.Functor,
        fctExp   : Modules.fctExp,
        argStr   : Modules.Structure, 
        argExp   : Modules.strExp,   (* entity exp for argStr *)
        evOp     : EntPath.entVar option,
        tdepth   : DebIndex.depth,
        epc      : EntPathContext.context,                                
        statenv  : StaticEnv.staticEnv,
	rpath    : InvPath.path,
        region   : SourceMap.region,
        compInfo : ElabUtil.compInfo}
    -> {resDec : Absyn.dec,
        resStr : Modules.Structure,
        resExp : Modules.strExp}

end (* signature SIGMATCH *)
