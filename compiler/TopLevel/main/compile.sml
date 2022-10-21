(* compile.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor CompileF (

    structure M  : CODE_GENERATOR
    structure CC : CCONFIG
    val cproto_conv : string

  ) : COMPILE0 =

struct

local
    structure SE = StaticEnv
    structure LV = LambdaVar
in
    fun mkCompInfo source =
	CompInfo.mkCompInfo
	  {source = source,
	   mkStampGenerator = CC.mkMkStamp}

    type pickle     = CC.pickle		(* pickled format *)
    type hash       = CC.hash		(* environment hash id *)
    type pid        = CC.pid
    type guid       = CC.guid

    fun say msg = (Control.Print.say msg; Control.Print.flush())

    (*************************************************************************
     *                             ELABORATION                               *
     *************************************************************************)

    (** several preprocessing phases done after parsing or
     ** after elaborations *)
    (*
    val fixityparse =
	(* Stats.doPhase (Stats.makePhase "Compiler 005 fixityparse") *)
	    FixityParse.fixityparse
    val lazycomp =
	(* Stats.doPhase (Stats.makePhase "Compiler 006 lazycomp") *)
	    LazyComp.lazycomp
     *)

    val pickUnpick : {context : SE.staticEnv, env : SE.staticEnv, guid : guid}
		  -> {pid: hash, pickle: pickle, exportLvars : LV.lvar list, exportPid: pid option, newenv: SE.staticEnv}
	  = CC.pickUnpick

    val pickUnpick =
	  Stats.doPhase (Stats.makePhase "Compiler 036 pickunpick") pickUnpick

    (* elaborate {ast: Ast.ast, statenv: SE.staticEnv, compInfo: compInfo.compInfo} -> absyn * SE.staticEnv *)
    (* take ast and staticEnv, elaborate the ast, and output the new absyn, staticEnv *)
    fun elaborate {ast: Ast.dec, statenv: SE.staticEnv, compInfo: CompInfo.compInfo} =
	  let val (absyn, nenv) = ElabTop.elabTop (ast, statenv, compInfo)
	   in if CompInfo.anyErrors compInfo
	      then (Absyn.SEQdec nil, StaticEnv.empty)
	      else (absyn, nenv)
	  end (* function elaborate *)
(*
	      val { pid, pickle, exportLvars, exportPid, newenv } =
		    pickUnpick { context = statenv, env = nenv, guid = guid }
	   in {absyn=absyn, newstatenv=newenv, exportPid=exportPid,
	       exportLvars=exportLvars, staticPid = pid, pickle = pickle}
*)

    val elaborate =
	  Stats.doPhase (Stats.makePhase "Compiler 030 elaborate") elaborate

    (*************************************************************************
     *                        ABSYN INSTRUMENTATION                          *
     *************************************************************************)

    local

      val specialSyms = [
	      SpecialSymbols.paramId,
	      SpecialSymbols.functorId,
	      SpecialSymbols.hiddenId,
	      SpecialSymbols.tempStrId,
	      SpecialSymbols.tempFctId,
	      SpecialSymbols.fctbodyId,
	      SpecialSymbols.anonfsigId,
	      SpecialSymbols.resultId,
	      SpecialSymbols.returnId,
	      SpecialSymbols.internalVarId
	    ]

      fun isSpecial s = List.exists (fn s' => Symbol.eq (s, s')) specialSyms

    in
      (** instrumenting the abstract syntax to do time- and space-profiling *)
      fun instrument {source, senv, compInfo} =
	    SProf.instrumDec (senv, compInfo) source
	    o TProf.instrumDec PrimopId.isPrimCallcc (senv, compInfo)
	    o TDPInstrument.instrument isSpecial (senv, compInfo)

    end (* local *)

    val instrument =
	  Stats.doPhase (Stats.makePhase "Compiler 039 instrument") instrument

    (*************************************************************************
     *                       TRANSLATION INTO FLINT                          *
     *************************************************************************)

    (** take the abstract syntax tree, generate the flint intermediate code *)
    fun translate {absyn, exportLvars, newstatenv, oldstatenv, compInfo} =
	(*** statenv used for printing Absyn in messages ***)
	  let val statenv = StaticEnv.atop (newstatenv, oldstatenv)
	   in Translate.transDec
		{rootdec = absyn,
		 exportLvars = exportLvars,
		 oldenv = oldstatenv,
		 env = statenv,
		 cproto_conv = cproto_conv,
		 compInfo = compInfo}
	  end

    val translate =
	  Stats.doPhase (Stats.makePhase "Compiler 040 translate") translate


    (*************************************************************************
     *                       CODE GENERATION                                 *
     *************************************************************************)

    (** take the flint code and generate the machine binary code *)
    local
      val addCode = Stats.addStat (Stats.makeStat "Code Size")
    in
    fun codegen { flint, imports, sourceName } = let
        (* optimized FLINT code *)
          val flint = FLINTOpt.optimize (flint, sourceName)
	(* from optimized FLINT code, generate the machine code.  *)
	  val csegs = M.compile {prog = flint, source = sourceName}
	(* Obey the nosplit directive used during bootstrapping.  *)
	(* val inlineExp = if isSome splitting then inlineExp else NONE *)
	  val codeSz = (CodeObj.size(#code csegs) + Word8Vector.length(#data csegs))
	  in
	    addCode codeSz;
	    { csegments=csegs, imports = imports }
	  end
    end (* local codegen *)

    (*
    val codegen =
	Stats.doPhase (Stats.makePhase "Compiler 140 CodeGen") codegen
     *)

    (*************************************************************************
     *                         COMPILATION                                   *
     *        = ELABORATION + TRANSLATION TO FLINT + CODE GENERATION         *
     * used by interact/evalloop.sml, cm/compile/compile.sml only            *
     *************************************************************************)
    (* compile: {source: Source.source, ast: Ast.ast; statenv: StaticEnv.staticEnv,
                 compInfo: ?.compInfo, checkErr : string -> unit, guid: guid} -> { ... }
     * compiling the ast into the binary code: elab; pickUnpick; instrument; translate; codegen *)
    fun compile {source, ast, statenv, compInfo, checkErr, guid} =
	  let val (absyn, nenv) =
		    elaborate {ast=ast, statenv=statenv, compInfo=compInfo}
		    before (checkErr "elaborate")
	      val {pid, pickle, exportLvars, exportPid, newenv} =
		    pickUnpick { context = statenv, env = nenv, guid = guid }
		    before (checkErr "pickUnpick")
	      val absyn =
		    instrument {source=source, senv = statenv, compInfo=compInfo} absyn
		    before (checkErr "instrument")
	      val {flint, imports} =
		    translate {absyn=absyn, exportLvars=exportLvars,
			       newstatenv=newenv, oldstatenv=statenv,
			       compInfo=compInfo}
		    before checkErr "translate"
	      val {csegments, imports} =
		    codegen {flint = flint, imports = imports,
			     sourceName = #fileOpened (#source compInfo)}
		    before (checkErr "codegen")

	 (* interp mode was [is] currently turned off.
	  *   [DBM 2022.10.19] Does interp mode still exist?]
	  *
	  * if !Control.interp then Interp.interp flint
	  *  else codegen {flint=flint, splitting=splitting, compInfo=cinfo}) *)

	  in {csegments = csegments,     (* produced by codegen *)
	      imports = imports,         (* produced by translate > codegen *)
	      absyn = absyn,             (* produced by elaborate > instrument*)
	      newstatenv = newenv,       (* produced by elaborate (nenv) > pickUnpick (newenv) *)
	      exportPid = exportPid,     (* produced by pickUnpick *)
	      exportLvars = exportLvars, (* produced by pickUnpick *)
	      staticPid = pid,           (* produced by pickUnpick *)
	      pickle = pickle}           (* produced by pickUnpick *)

	 end (* function compile *)

end (* top local *)
end (* functor CompileF *)
