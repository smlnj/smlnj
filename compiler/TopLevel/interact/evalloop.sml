(* evalloop.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor EvalLoopF (Compile: TOP_COMPILE) : EVALLOOP =
  struct

    structure C  = Compile
    structure EM = ErrorMsg
    structure S = Symbol
    structure E  = Environment
    structure PP = PrettyPrint
    structure T = Time
    structure U = Unsafe
    structure PC = SMLofNJ.Internals.ProfControl
    structure ED = ElabDebug

    type lvar = LambdaVar.lvar

    val debugging = Control.eldebugging
    fun say msg = (Control.Print.say msg; Control.Print.flush())
    fun newline () = say "\n"
    fun saynl msg = (Control.Print.say msg; newline ())
    fun saysnl strs = saynl (concat strs)
    fun dbsaynl msg =
	if !debugging then saynl msg else ()
		    (* diagnostic printing of Ast and Absyn *)
		      val printDepth = Control_Print.printDepth

    fun debugPrint flag (msg: string, printfn: PP.stream -> 'a -> unit, arg: 'a) =
	if !flag
	then PP.with_pp (EM.defaultConsumer())
	  (fn ppstrm =>
	      (PP.openHVBox ppstrm (PP.Rel 0);
	       PP.string ppstrm msg;
	       PP.newline ppstrm;
	       PP.openHVBox ppstrm (PP.Rel 0);
	       printfn ppstrm arg;
	       PP.closeBox ppstrm;
	       PP.newline ppstrm;
	       PP.closeBox ppstrm;
	       PP.flushStream ppstrm))
	else ()

    val compManagerHook : {
	    manageImport : Ast.dec * EnvRef.envref -> unit,
	    managePrint :  Symbol.symbol * EnvRef.envref -> unit,
	    getPending : unit -> Symbol.symbol list
	  } ref = ref {
	    manageImport = fn _ => (),
	    managePrint = fn _ => (),
	    getPending = fn () => []
	  }

    fun installCompManagers cm = compManagerHook := cm

    exception Interrupt
    exception EndOfFile
  (* to wrap exceptions that are raised during the execution of a top-level transaction *)
    exception ExnDuringExecution of exn

    fun interruptable f x = let
	  val oldcont = !U.topLevelCont
	  in
	    U.topLevelCont := SMLofNJ.Cont.callcc
		(fn k => (SMLofNJ.Cont.callcc(fn k' => (SMLofNJ.Cont.throw k k'));
			  raise Interrupt));
	    (f x before U.topLevelCont := oldcont)
	      handle e => (U.topLevelCont := oldcont; raise e)
	  end

  (* The baseEnv and localEnv are purposely refs so that a top-level command
   * can re-assign either one of them, and the next iteration of the loop
   * will see the new value. It's also important that the toplevelenv
   * continuation NOT see the "fetched" environment, but only the ref;
   * then, if the user "filters" the environment ref, a smaller image
   * can be written.
   *)
    fun evalLoop source = let
	  val parser = SmlFile.parseOne source
	  val cinfo = C.mkCompInfo { source = source, transform = fn x => x }

	  fun checkErrors (s: string) =
		if CompInfo.anyErrors cinfo
		  then (
		    if !Control.progressMsgs
		      then saysnl ["<<< Error stop after ", s]
		      else ();
		    raise EM.Error)
		else if !Control.progressMsgs
		  then saysnl ["### ", s, " successful"]
		  else ()

	  fun oneUnit () = ((* perform one transaction  *)
		if !Control.progressMsgs
		  then saysnl ["<<< oneUnit [compiling \"", #fileOpened source, "\"]"]
		  else ();
		case parser ()
		 of NONE => raise EndOfFile
		  | SOME ast => let
		      val _ = if !Control.progressMsgs
			    then say "### parsing successful\n"
			    else ()

                      val loc = EnvRef.loc ()
                      val base = EnvRef.base ()
                      val _ = #manageImport (!compManagerHook) (ast, loc)

		      fun getenv () = E.layerEnv (#get loc (), #get base ())

		      val {static=statenv, dynamic=dynEnv} = getenv ()

		      (* conditional diagnostic code to print ast - could it be involked from parser?
			 if so, what statenv would be used? *)
		      val _ = let fun ppAstDec ppstrm astdec =
				      PPAst.ppDec NONE ppstrm (astdec, 1000)
			      in debugPrint debugging ("AST::", ppAstDec, ast)
			      end

		      val {csegments, newstatenv, absyn, exportPid, exportLvars, imports, ...} =
			  C.compile {source=source, ast=ast,
				     statenv=statenv,
				     compInfo=cinfo,
				     checkErr=checkErrors,
				     guid = () }
		      (** returning absyn and exportLvars here is a bad idea,
			  they hold on things unnecessarily; this must be
			  fixed in the long run. (ZHONG)
		       *)
		      val _ = if !Control.progressMsgs
			    then say "### C.compile successful\n"
			    else ()
		      val _ = let fun ppAbsynDec ppstrm absyn_dec =
				      PPAbsyn.ppDec (statenv, NONE) ppstrm (absyn_dec, 1000)
			      in debugPrint debugging ("Absyn: ", ppAbsynDec, absyn)
			      end

		      val executable = Execute.mkExec
					   { cs = csegments,
					     exnWrapper = ExnDuringExecution }
				       before checkErrors ("mkExec")
		      val executable = Isolate.isolate (interruptable executable)

		      val _ = (PC.current := Profile.otherIndex)
		      val newdynenv =
			  Execute.execute { executable=executable, imports=imports,
					    exportPid=exportPid, dynEnv=dynEnv }
		      val _ = (PC.current := Profile.compileIndex)

		      val newenv = E.mkenv { static = newstatenv, dynamic = newdynenv }
                      (* refetch local environment because execution may have changed its contents *)
		      val newLocalEnv = E.concatEnv (newenv, #get loc ())

		      (* we install the new local env first before we go about
		       * printing, otherwise we find ourselves in trouble if
		       * the autoloader changes the the contents of loc under
		       * our feet... *)
		      val _ = #set loc newLocalEnv

		      fun look_and_load sy = let
			  fun look () = StaticEnv.look (E.staticPart (getenv ()), sy)
			  in
			      look ()
			      handle StaticEnv.Unbound =>
				     (#managePrint (!compManagerHook) (sy, loc);
				      look ())
			  end

		      (* Notice that even through several potential rounds
		       * the result of get_symbols is constant (up to list
		       * order), so memoization (as performed by
		       * StaticEnv.special) is ok. *)
		      fun get_symbols () = let
			  val se = E.staticPart (getenv ())
			  val othersyms = #getPending (!compManagerHook) ()
			  in
			      StaticEnv.symbols se @ othersyms
			  end

		      val ste1 = StaticEnv.special (look_and_load, get_symbols)
		      val _ = if !Control.progressMsgs then say "### ste1 successful\n" else ()

		      val e0 = getenv ()
		      val _ = if !Control.progressMsgs then say "### e0 successful\n" else ()

		      val e1 = E.mkenv { static = ste1, dynamic = E.dynamicPart e0 }
		      val _ = if !Control.progressMsgs then say "### e1 successful\n" else ()

		      in
			PP.with_pp
			    (#errConsumer source)
			    (fn ppstrm => PPDec.ppDec e1 ppstrm (absyn, exportLvars));
		        if !Control.progressMsgs then say "### oneUnit\n" else ()
		      end
		  (* end case *))

	  fun loop() = (oneUnit(); loop())
	  in
	    interruptable loop ()
	  end (* function evalLoop *)

  (* return the message for an exception *)
    fun exnMsg (CompileExn.Compile s) = concat ["Compile: \"", s, "\""]
      | exnMsg exn = General.exnMessage exn

  (* print the exception history for an exception *)
    fun showhist exn = let
	  fun show [s] = say (concat ["  raised at: ", s, "\n"])
	    | show (s::r) =
		(show r; say (concat ["             ", s, "\n"]))
	    | show [] = ()
	  in
	    show (SMLofNJ.exnHistory exn)
	  end

    fun uncaughtExnMessage (ExnDuringExecution exn) = uncaughtExnMessage exn
      | uncaughtExnMessage exn = let
	  val msg = exnMsg exn
	  val name = exnName exn
	  in
	    if msg = name
	      then say (concat ["\nuncaught exception ", name, "\n"])
	      else say (concat ["\nuncaught exception ", name, " [", msg, "]\n"]);
	    showhist exn
	  end

    fun withErrorHandling treatAsUser { thunk, flush, cont = k } = let
	  fun user_hdl exn = (
		uncaughtExnMessage exn;
		flush ();
		k exn)

	  fun bug_hdl exn = (
		say (concat [
		    "\nunexpected exception (bug?) in SML/NJ: ",
		    exnName exn," [", exnMsg exn, "]\n"
		  ]);
		showhist exn;
		flush();
		k exn)

	  fun non_bt_hdl e = (case e
		 of EndOfFile => (say "\n")
		  | (Interrupt | ExnDuringExecution Interrupt) =>
		      (say "\nInterrupt\n"; flush(); k e)
		  | EM.Error => (flush(); k e)
		  | CompileExn.Compile "syntax error" => (flush(); k e)
		  | CompileExn.Compile s =>
		      (say(concat["\nuncaught exception Compile: \"", s,"\"\n"]);
		       flush(); k e)
		  | Isolate.TopLevelCallcc =>
		      (say("Error: throw from one top-level expression \
			   \into another\n");
		       flush (); k e)
		  | (Execute.Link | ExnDuringExecution Execute.Link) =>
		      (flush (); k e)
		  | ExnDuringExecution EM.Error => (flush(); k e)
		  | ExnDuringExecution ParserControl.RESET_PARSER => (flush(); k e)
		  | ExnDuringExecution exn => user_hdl exn
		(* the following handle Suspend/Resume on Unix (4 == Posix.Error.intr) *)
		  | IO.Io{cause=OS.SysErr(_, SOME 4), ...} => (say "\n"; k e)
		  | exn => if treatAsUser then user_hdl exn else bug_hdl exn
		(* end case *))
	  in
	    (SMLofNJ.Internals.TDP.with_monitors false thunk)
	      handle e => non_bt_hdl e
	  end (* withErrorHandling *)

  (*** interactive loop, with error handling ***)
    fun interact () = let
	  val source = Source.newSource ("stdIn", TextIO.stdIn, true, EM.defaultConsumer ())
	  fun flush' () = (
		case TextIO.canInput(TextIO.stdIn, 4096)
		 of (NONE | SOME 0) => ()
		  | SOME _ => (ignore (TextIO.input TextIO.stdIn); flush'())
		(* end case *))
	  fun flush () = (
		#anyErrors source := false;
		flush'() handle IO.Io _ => ())
	  fun loop () = withErrorHandling false {
		  thunk = fn () => evalLoop source,
		  flush = flush, cont = loop o ignore
		}
	  in
	    loop ()
	  end

    fun isTermIn f = let
	  val (rd, buf) = TextIO.StreamIO.getReader(TextIO.getInstream f)
	  val isTTY = (case rd
		 of TextPrimIO.RD{ioDesc = SOME iod, ...} => (OS.IO.kind iod = OS.IO.Kind.tty)
		  | _ => false
		(* end case *))
	  in
	  (* since getting the reader will have terminated the stream, we need
	   * to build a new stream.
	   *)
	    TextIO.setInstream(f, TextIO.StreamIO.mkInstream(rd, buf));
	    isTTY
	  end

    fun evalStream (fname, stream) = let
	  val interactive = isTermIn stream
	  val source = Source.newSource (fname, stream, interactive, EM.defaultConsumer ())
	  in
	    evalLoop source
	      handle exn => (
		  Source.closeSource source;
		  case exn
		   of EndOfFile => ()
		    | _ => raise exn
		  (* end case *))
	  end

  end (* functor EvalLoopF *)
