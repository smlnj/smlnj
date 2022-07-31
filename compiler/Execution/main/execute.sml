(* execute.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(*****************************************************************************
 *                        EXECUTING THE EXECUTABLE                           *
 *****************************************************************************)

structure Execute : sig

    exception Link	(* For the compilation manager to signal to the
			 * interactive loop that error messages have been
			 * issued already.  The interactive loop should
			 * simply discard this exception (keep quiet) and
			 * go to the next input prompt.
			 *)

  (* turn the byte-vector-like code into an executable closure.
   * The resulting closure will wrap any uncaught exception raised by the code using
   * the `exnWrapper` function.
   *)
    val mkExec : { cs : CodeObj.csegments, exnWrapper : exn -> exn } -> CodeObj.executable

    val execute : {
	    executable: CodeObj.executable,
	    imports: ImportTree.import list,
	    exportPid: PersStamps.persstamp option,
	    dynEnv: DynamicEnv.env
	  } -> DynamicEnv.env

  end = struct

    exception Link

    structure Obj = Unsafe.Object
    type object = Obj.object

    val say = Control_Print.say
    fun bug s = ErrorMsg.impossible ("Execute: " ^ s)

    fun mkExec { cs = {code, data}, exnWrapper } = let
	  val exec = CodeObj.exec code
	  val nex = if (Word8Vector.length data > 0)
		then (fn ivec => exec (Obj.mkTuple (Obj.toTuple ivec @ [CodeObj.mkLiterals data])))
	        else (fn ivec => exec ivec)
          in
	    fn args => (nex args handle exn => raise exnWrapper exn)
          end

  (** perform the execution of the excutable, output the new dynEnv *)
    fun execute {executable, imports, exportPid, dynEnv } = let
	  val args : object = let
		fun selObj (obj, i) =
		      Obj.nth(obj, i)
			handle _ => bug "unexpected linkage interface in execute"
		fun getObj ((p, n), zs) = let
		      fun get (obj, ImportTree.ITNODE [], z) = obj::z
			| get (obj, ImportTree.ITNODE xl, z) = let
			    fun g ((i, n), x) = get (selObj(obj, i), n, x)
			    in
			      List.foldr g z xl
			    end
		      val obj = (case  DynamicEnv.look dynEnv p
			     of SOME obj => obj
			      | NONE => (
				  say ("lookup " ^ (PersStamps.toHex p) ^ "\n");
				  raise CompileExn.Compile
				      "imported objects not found or inconsistent")
			    (* end case *))
		      in
			get(obj, n, zs)
		      end
		in
		  Obj.mkTuple (foldr getObj [] imports)
		end
	  val result : object = executable args
	  in
	    case exportPid
	     of NONE => DynamicEnv.empty
	      | SOME p => DynamicEnv.singleton (p, result)
	  end

    val execute = Stats.doPhase (Stats.makePhase "Execute") execute

  end
