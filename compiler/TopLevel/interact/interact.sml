(* interact.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor Interact(EvalLoop : EVALLOOP) : INTERACT =
  struct
    exception Interrupt = EvalLoop.Interrupt

    type envref = EnvRef.envref

    val installCompManagers = EvalLoop.installCompManagers

    fun interact() = (EvalLoop.interact (); OS.Process.exit OS.Process.success)

    val withErrorHandling = EvalLoop.withErrorHandling

  (* compile and execute the contents of a file. *)
    local
      val outerUse = ref true

      exception CompileError

    in

    fun use fname = let
	  val isOuter = !outerUse
	  val _ = outerUse := false
	  fun restore () = (outerUse := isOuter)
	  (* for nested calls to "use", we want to propagate compilation errors
           * so as to abort the outermost call.
           *)
          fun compileError () = (
                if isOuter
                  then false
                  else raise CompileError)
          in
            if OS.FileSys.access(fname, [OS.FileSys.A_READ])
              then let
                val _ = app Control.Print.say ["[opening ", fname, "]\n"]
                val strm = TextIO.openIn fname
                in
                  (EvalLoop.evalStream (fname, strm); restore(); true)
                    handle exn => (
                      restore ();
                      case exn
                       of CompileError => compileError()
                        | ErrorMsg.Error => compileError()
                        | EvalLoop.ExnDuringExecution CompileError => compileError()
                        | EvalLoop.ExnDuringExecution exn' =>
(* FIXME: it would be useful to have a way to raise exn' without modifying its
 * trace-back history so that the message displayed to the user does not include
 * a confusing reference to this line of compiler code!  Also for line 65.
 *)
                            if isOuter then raise exn' else raise exn
                        | _ => (
                          (* this is probably an error in the compiler! *)
                            if isOuter
                              then (
                                app Control.Print.say [
                                    "[unexpected exception: ", General.exnMessage exn,
                                    "]\n"
                                  ];
                                raise exn)
                              else raise EvalLoop.ExnDuringExecution exn)
                      (* end case *))
                end
              else (
                restore();
                app Control.Print.say [
                    "[use failed: '", fname,
                    "' does not exist or is unreadable]\n"
                  ];
                compileError())
	    end (* use *)

  (* compile a file; returns true if okay and false on either compile-time
   * or run-time error
   *)
    fun useFile fname = (
	  outerUse := false;
	  (use fname; outerUse := true; true)
	    handle CompileError => false
	      | exn => (
		  outerUse := true;
		  EvalLoop.uncaughtExnMessage exn;
		  false))

    end (* local *)

    fun useStream stream = EvalLoop.evalStream ("<instream>", stream)

    fun evalStream (stream, baseEnv) = let
	  val r = ref Environment.emptyEnv
	  val base = { set = fn _ => raise Fail "evalStream: #set base",
		       get = fn () => baseEnv }
	  val loc = { set = fn e => r := e,
		      get = fn () => !r }
	  val props = PropList.newHolder ()
	  val state = { loc = loc, base = base, props = props }
	  in
	    EnvRef.locally (state, fn () => (EvalLoop.evalStream ("<instream>", stream); !r))
	  end

    local
      open SMLofNJ.Cont
    in
      val redump_heap_cont : string cont ref =
	    ref (callcc (fn ret => (callcc (fn k => throw ret k);
				    raise Fail "redump_heap_cont init")))
    end

  end (* functor Interact *)
