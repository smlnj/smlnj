(*
 * Build a simple dependency graph from a direct DAG description.
 *   - This is used in the bootstrap compiler to establish the
 *     pervasive env and the primitives which later get used by
 *     the rest of the system.
 *   - One important job is to set up a binding to "structure _Core".
 *
 * (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
signature BUILD_INIT_DG = sig
    val build : GeneralParams.info -> SrcPath.file ->
	{ pervasive: DependencyGraph.sbnode,
	  others: DependencyGraph.sbnode list,
	  src: Source.inputSource } option
end

structure BuildInitDG :> BUILD_INIT_DG = struct

    structure S = Source
    structure EM = ErrorMsg
    structure SM = SourceMap
    structure DG = DependencyGraph

    fun build (gp: GeneralParams.info) specgroup = let
	val ovldR = Control.overloadKW
	val ovldC = { save'restore =
		        fn () => let val orig = !ovldR in
				  fn () => ovldR := orig
				 end,
		      set = fn () => ovldR := true }
	val penv = #penv (#param gp)
	val errcons = #errcons gp
	val groupreg = #groupreg gp

	val context = SrcPath.dir specgroup
	val _ = Say.vsay ["[reading init spec from ",
			  SrcPath.descr specgroup, "]\n"]

	fun defined symbol = isSome (#get (#symval (#param gp) symbol) ())

	fun work stream = let
	    val source = S.newSource (SrcPath.osstring specgroup,
				      stream, false, errcons)
	    val sourceMap = #sourceMap source

	    val _ = GroupReg.register groupreg (specgroup, source)

	    fun error r m = EM.error source r EM.COMPLAIN m EM.nullErrorBody

	    fun lineIn pos = let
		fun sep c = Char.isSpace c orelse Char.contains "(),=;" c
		val sub = String.sub
		val null = List.null
		fun return (pos, line) = SOME (String.tokens sep line, pos)
		fun loop (pos, NONE, []) = NONE
		  | loop (pos, NONE, lines) = return (pos, concat (rev lines))
		  | loop (pos, SOME line, lines) = let
			val len = size line
			val newpos = pos + len
			val iscont =
			    len >= 2 andalso
			    sub (line, len -1 ) = #"\n" andalso
			    sub (line, len - 2) = #"\\"
		    in
			SourceMap.newline sourceMap newpos;
			if iscont then
			    loop (newpos, TextIO.inputLine stream,
				  substring (line, 0, len - 2) :: lines)
			else if null lines andalso sub (line, 0) = #"#" then
			    SOME ([], newpos)
			else return (newpos, concat (rev (line :: lines)))
		    end
	    in
		loop (pos, TextIO.inputLine stream, [])
	    end

	    fun loop (m, pos) =
		case lineIn pos of
		    NONE => (error (pos, pos) "unexpected end of file"; NONE)
		  | SOME (line, newpos) => let
			val error = error (pos, newpos)
			fun sml (spec, xe, rts, ecs) = let
			    val p = SrcPath.file
				     (SrcPath.standard
					  { env = penv, err = error }
					  { context = context, spec = spec })
			    val attribs =
				{ is_rts = rts, extra_compenv = xe,
				  explicit_core_sym = ecs, noguid = false }
			in SmlInfo.info' attribs gp
					 { sourcepath = p,
					   group = (specgroup, (pos, newpos)),
					   sh_spec = Sharing.DONTCARE,
					   setup = (NONE, NONE),
					   locl = false,
					   controllers = [ovldC] }
			end
			fun bogus n =
			    DG.SNODE { smlinfo = sml (n, NONE, false, NONE),
				       localimports = [], globalimports = [] }
			fun look n =
			    case StringMap.find (m, n) of
				SOME x => x
			      | NONE => (error ("undefined: " ^ n); bogus n)
			fun node (name, file, args, is_rts, ecs) = let
			    fun one (arg, (li, needs_primenv)) =
				if arg = "primitive" then (li, true)
				else (look arg :: li, needs_primenv)
			    val (li, needs_primenv) =
				foldr one ([], false) args
			    val xe =
				if needs_primenv then SOME PrimEnv.primEnv
				else NONE
			    val i = sml (file, xe, is_rts, ecs)
			    val n = DG.SNODE { smlinfo = i,
					       localimports = li,
					       globalimports = [] }
			in
			    loop (StringMap.insert (m, name, n), newpos)
			end
			val looksb = DG.SB_SNODE o look

			fun proc [] = loop (m, newpos)
			  | proc ("bind" :: name :: file :: args)  =
			    node (name, file, args, false, NONE)
			  | proc ("rts-placeholder" :: name :: file :: args) =
			    node (name, file, args, true, NONE)
			  | proc ("bind-core" :: ecs :: name :: file :: args) =
			    node (name, file, args, false,
				  SOME (Symbol.strSymbol ecs))
			  | proc ("return" :: pervasive :: prims) =
			    SOME { pervasive = looksb pervasive,
				   others = map looksb prims,
				   src = source }
			  | proc ("ifdef" :: symbol :: line) =
			    proc (if defined symbol then line else [])
			  | proc ("ifndef" :: symbol :: line) =
			    proc (if defined symbol then [] else line)
			  | proc _ = (error "malformed line"; NONE)
		    in
			proc line
		    end
	in
	    loop (StringMap.empty, 1)
	end
	fun openIt () = TextIO.openIn (SrcPath.osstring specgroup)
    in
	SafeIO.perform { openIt = openIt, closeIt = TextIO.closeIn,
			 work = work, cleanup = fn _ => () }
    end
end
