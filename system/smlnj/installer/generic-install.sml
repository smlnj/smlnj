(* generic-install.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * SML/NJ installer script -- written in SML.
 *   This script runs after the runtime system has been built and
 *   the interactive system has been booted from bootfiles.
 *
 * The remainder of the build process compiles additional libraries
 * and installs certain standalone programs such as ml-yacc and ml-lex.
 * This is the bulk of what used to be done by install.sh.
 *
 * The script is written in such a way that it can be used portably
 * on both *nix- and win32-systems.
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)

structure GenericInstall : sig

  (* all filenames that are passed as arguments use native syntax: *)
    val proc : {
	    smlnjroot : string,
	    installdir : string,
	    configcmd : string,
	    buildcmd : string,
	    instcmd : string -> unit,
	    unpack : (string list -> bool) option
	  } -> unit

  end = struct

    structure U = InstallerUtil
    structure P = OS.Path
    structure F = OS.FileSys
    structure SM = RedBlackMapFn (type ord_key = string
				  val compare = String.compare)
    structure SS = RedBlackSetFn (type ord_key = string
				  val compare = String.compare)

    structure SCC = GraphSCCFn (type ord_key = string
				val compare = String.compare)

    val say = U.say and warn = U.warn and fail = U.fail

    val { arch_oskind, heap_suffix, isUnix } = U.platformInfo ()

  (* convert standard syntax to native syntax *)
    val native = P.fromUnixPath

  (* several worklists for delayed execution *)
    val stablist : (unit -> bool) list ref = ref []
    val movlist  : (unit -> unit) list ref = ref []
    val salist : (unit -> unit) list ref = ref []

  (* move a stable library file to its final location *)
    fun movelib src dst () = (
	  U.mkdir (P.dir dst);
	  U.rename { old = src, new = dst })

  (* register a temporary anchor-value binding *)
    fun localanchor { anchor, path } =
	  #set (CM.Anchor.anchor anchor) (SOME (native path))

    fun getInputTokens s = (case TextIO.inputLine s
	   of NONE => NONE
	    | SOME l => if String.sub (l, 0) = #"#"
		then getInputTokens s
	        else SOME (String.tokens Char.isSpace l)
	  (* end case *))

    fun tokenLine l = String.concatWith " " l

    (* Take a list of modules and dependencies (in depfile) and
     * build the transitive closure of those modules.
     * We do this by considering the dependency graph and construct
     * a topological order for it. *)
    fun resolve (modules, depfile) = let
	val s = TextIO.openIn depfile
	fun rd m = (case getInputTokens s
	       of NONE => (TextIO.closeIn s; fn x => getOpt (SM.find (m, x), []))
		| SOME (x :: xs) => rd (SM.insert (m, x, xs))
		| SOME [] => rd m
	      (* end case *))
	fun strip (SCC.SIMPLE c) = c
	  | strip _ = fail ["cyclic dependencies in ", depfile, "\n"]
	in
	  List.revMap strip (SCC.topOrder' { roots = modules, follow = rd SM.empty })
	end

    (* do all the delayed stuff: *)

  (* stabilization of libraries... *)
    fun dostabs () =
	  foldr (fn (f, true) => f () | (_, false) => false) true (!stablist)

  (* move stable library files to their final locations... *)
    fun domoves () = (
	  app (fn f => f ()) (rev (!movlist));
	  true)
	    handle _ => false

  (* fold a function over the contents of a pathconfig file: *)
    fun pc_fold g m f = let
	  val s = TextIO.openIn f
	  fun loop m = (case getInputTokens s
		 of NONE => (TextIO.closeIn s; m)
		  | SOME [k, v] => loop (g (m, k, v))
		  | SOME l => (say ("funny line in " :: f :: ":" ::
				    foldr (fn (x, l) => " " :: x :: l)
					  ["\n"] l);
			       loop m)
		(* end case *))
	  in
	    loop m
	  end handle _ => m	(* in case file does not exist *)

    (* build those standalone programs that require libraries
     * and, therefore, must be compiled "late"... *)
    fun dolatesas () = (
	  app (fn f => f ()) (rev (!salist));
	  true)
	    handle _ => false

  (* representation of actions *)
    datatype action
      = RegLib of { anchor: string, relname: string, dir: string,
		    altanchor: string option }
		  * bool (* true = only on Unix *)
      | Anchor of { anchor: string, path: string }
		  * bool (* true = relative to libdir *)
      | Program of { target: string, optheapdir: string option,
		     dir: string }
		   * bool	(* true = defer *)
      | Config of { target : string, dir : string }

  (* parse the action file and return the actions map and set of modules *)
    fun parseActions actionfile = let
	  val s = TextIO.openIn actionfile
	(* optional heap directory specifier *)
	  fun opthd "-" = NONE
	    | opthd h = SOME h
	(* process arguments for a "prog" or "dprog" action *)
	  fun progargs (mn, []) =
		{ target = mn, optheapdir = NONE, dir = mn }
	    | progargs (mn, [t]) =
		{ target = t, optheapdir = NONE, dir = mn }
	    | progargs (mn, [t, h]) =
		{ target = t, optheapdir = opthd h, dir = mn }
	    | progargs (mn, t :: h :: d :: _) =
		{ target = t, optheapdir = opthd h, dir = d }
	(* process arguments for a "lib" or "ulib" action *)
	  fun libargs (a, r, d, aa) =
		{ anchor = a, relname = r, dir = d, altanchor = aa }
	(* loop over the action file *)
	  fun loop (m, ams) = (case getInputTokens s
		 of NONE => (m, ams)
		  | SOME [mn, "src"] =>
		      loop (m, SS.add (ams, mn))
		  | SOME [mn, "lib", a, r, d] =>
		      ins (m, ams, mn,
			   RegLib (libargs (a, r, d, NONE), false))
		  | SOME [mn, "lib", a, r, d, aa] =>
		      ins (m, ams, mn,
			   RegLib (libargs (a, r, d, SOME aa), false))
		  | SOME [mn, "ulib", a, r, d] =>
		      ins (m, ams, mn,
			   RegLib (libargs (a, r, d, NONE), true))
		  | SOME [mn, "ulib", a, r, d, aa] =>
		      ins (m, ams, mn,
			   RegLib (libargs (a, r, d, SOME aa), true))
		  | SOME [mn, "anchor", a, p] =>
		      ins (m, ams, mn,
			   Anchor ({ anchor = a, path = p }, false))
		  | SOME [mn, "libanchor", a, p] =>
		      ins (m, ams, mn,
			   Anchor ({ anchor = a, path = p }, true))
		  | SOME (mn :: "prog" :: args) =>
		      ins (m, ams, mn,
			   Program (progargs (mn, args), false))
		  | SOME (mn :: "dprog" :: args) =>
		      ins (m, ams, mn,
			   Program (progargs (mn, args), true))
		  | SOME [mn, "config", dir] =>
		      ins (m, ams, mn, Config{target=mn, dir=dir})
		  | SOME [] =>
		      loop (m, ams)
		  | SOME other => fail [
			"Illegal line in ", actionfile, ": ",
			String.concatWith " " other, "\n"
		      ]
		(* end case *))
	  and ins (m, ams, mn, act) =
		loop (SM.insert (m, mn, act :: getOpt (SM.find (m, mn), [])),
		      SS.add (ams, mn))
	  in
	    loop (SM.empty, SS.empty)
	      before TextIO.closeIn s
	  end

  (* representation of requests in the targets file *)
    datatype target_req
      = REQUEST of string
      | IFDEF of string * target_req list * target_req list

  (* parse the targets file.  The syntax of requests is:
   *
   *    <file> ::= <req>*
   *	<req>  ::= 'request' <module> NL
   *		|  'if' SYMBOL NL <req>* <elif>* 'endif' NL
   *    <elif> ::= 'elif' SYMBOL NL <req>*
   *
   * where NL represents end-of-line and SYMBOL is one of
   *
   *	SIZE_32		-- true for 32-bit systems
   *	SIZE_64		-- true for 64-bit systems
   *	WINDOWS		-- true for Microsoft Windows
   *	UNIX		-- true for Unix systems (including macOS and Linux)
   *)
    fun parseTargets (file, actions) = let
	  val inS = TextIO.openIn file
	  fun badLine l = fail ["ill-formed targets line: ", tokenLine l, "\n"]
	  fun unexpected (tok, l) = fail ["unexpected '", tok, "': ", tokenLine l, "\n"]
	  fun eof () = fail ["unexpected end-of-file in conditional"]
	  fun parse reqs = (case getInputTokens inS
		 of NONE => List.rev reqs
		  | SOME [x as ("dont_move_libraries" | "move_libraries")] => (
		      warn [
			  "\"", x, "\" no longer supported",
			  " (installer always moves libraries)\n"
			];
		      parse reqs)
		  | SOME["request", module] => parse (REQUEST module :: reqs)
		  | SOME["if", symb] => parse (parseIf symb :: reqs)
		  | SOME(l as ["elif", _]) => unexpected("elif", l)
		  | SOME(l as ["endif"]) => unexpected("endif", l)
		  | SOME[] => parse reqs
		  | SOME l => badLine l
		(* end case *))
	  and parseIf (symb) : target_req = let
	      (* parse the contents of an 'if' or 'elif' block *)
		fun condBlk (symb, reqs) : target_req = (case getInputTokens inS
			 of NONE => eof ()
			  | SOME["request", module] => condBlk (symb, REQUEST module :: reqs)
			  | SOME["if",  symb'] => condBlk (symb, parseIf symb' :: reqs)
			  | SOME["elif", symb'] =>
			      IFDEF(symb, List.rev reqs, [condBlk (symb, [])])
			  | SOME["else", symb'] =>
			      IFDEF(symb, List.rev reqs, elseBlk [])
			  | SOME["endif"] =>
			      IFDEF(symb, List.rev reqs, [])
			  | SOME[] => condBlk (symb, reqs)
			  | SOME l => badLine l
			(* end case *))
	      (* parse the contents of an 'else' block *)
		and elseBlk reqs = (case getInputTokens inS
			 of NONE => eof ()
			  | SOME["request", module] => elseBlk (REQUEST module :: reqs)
			  | SOME["if",  symb'] => elseBlk (parseIf symb' :: reqs)
			  | SOME(l as ["elif", symb']) => unexpected("elif", l)
			  | SOME(l as ["else", symb']) => unexpected("else", l)
			  | SOME["endif"] => List.rev reqs
			  | SOME[] => elseBlk reqs
			  | SOME l => badLine l
			(* end case *))
		in
		  condBlk (symb, [])
		end
	  val requests = parse []
	  val _ = TextIO.closeIn inS
	(* process the requests *)
	  fun loop ([], modules, srcReqs, allsrc) = (modules, srcReqs, allsrc)
	    | loop (REQUEST "src-smlnj" :: reqs, modules, srcReqs, allsrc) =
		loop (reqs, modules, srcReqs, true)
	    | loop (REQUEST module :: reqs, modules, srcReqs, allsrc) =
		if SM.inDomain(actions, module)
		  then loop (reqs, module :: modules, srcReqs, allsrc)
		  else loop (reqs, modules, module :: srcReqs, allsrc) (* assume a src module *)
	    | loop (IFDEF(symb, tReqs, fReqs) :: reqs, modules, srcReqs, allsrc) = let
		val cond = (case String.map Char.toUpper symb
		       of "SIZE_32" => (SMLofNJ.SysInfo.getArchSize() = 32)
			| "SIZE_64" => (SMLofNJ.SysInfo.getArchSize() = 64)
			| "UNIX" => (SMLofNJ.SysInfo.getOSKind() = SMLofNJ.SysInfo.UNIX)
			| "WINDOWS" => (SMLofNJ.SysInfo.getOSKind() = SMLofNJ.SysInfo.WIN32)
			| _ => fail ["unknown symbol '", symb, "' in conditional\n"]
		      (* end case *))
		val (modules, srcReqs, allsrc) =
		      loop (if cond then tReqs else fReqs, modules, srcReqs, allsrc)
		in
		  loop (reqs, modules, srcReqs, allsrc)
		end
	  in
	    loop (requests, [], [], false)
	  end

  (* our main routine *)
    fun proc { smlnjroot, installdir, configcmd, buildcmd, instcmd, unpack } = let
	  val smlnjroot = F.fullPath smlnjroot
	  val installdir = F.fullPath installdir
	  val libdir = P.concat (installdir, "lib")
	  val configdir = P.concat (smlnjroot, "config")
	  val bindir = P.concat (installdir, "bin")
	  val heapdir = P.concat (bindir, ".heap")
	  val cm_pathconfig = P.concat (libdir, "pathconfig")
	(* dependency file: config/dependencies *)
	  val depfile = P.concat (configdir, "dependencies")
	(* where to get additional path configurations *)
	  val extrapathconfig = P.concat (configdir, "extrapathconfig")
	(* action file: mapping from "modules" to lists of "actions" *)
	  val actionfile = P.concat (configdir, "actions")
	(* add an entry to lib/pathconfig *)
	  fun write_cm_pathconfig (a, p) = let
	        val s = TextIO.openAppend cm_pathconfig
		in
		  TextIO.output (s, concat [a, " ", p, "\n"])
		  before TextIO.closeOut s
		end
	  fun augment_anchor_mapping pcfile =
		pc_fold (fn ((), k, v) =>
			    (#set (CM.Anchor.anchor k)
				  (SOME (P.concat (libdir, native v)));
			     write_cm_pathconfig (k, v)))
			()
			pcfile
	(* augment anchor mapping with extra bindings: *)
	  val _ = augment_anchor_mapping extrapathconfig
	(* find and open first usable targetsfiles *)
	  val targetsfiles = [
	          P.concat (configdir, "targets.customized"),
	          P.concat (configdir, "targets")
		]
	  val targetFile = (case List.find U.fexists targetsfiles
		 of SOME f => f
		  | NONE => fail ["cannot find targets file in '", configdir, "'\n"]
		(* end case *))
	(* get the actions from the actionfile *)
	  val (actions, allmoduleset) = parseActions actionfile
	(* parse the targets file *)
	  val (modules, srcReqs, allsrc) = parseTargets (targetFile, actions)
	(* now resolve dependencies; get full list of modules in correct build order: *)
	  val modules = resolve (modules, depfile)
	  val moduleset = SS.fromList modules
	(* add requested source modules *)
	  val moduleset = if allsrc
		then SS.union (moduleset, allmoduleset)
		else SS.addList (moduleset, srcReqs)
	(* fetch and unpack source trees, using auxiliary helper command
	 * which takes the root directory as its first and the module
	 * names to be fetched as subsequent arguments.
	 *)
	  val _ = (case unpack
		 of NONE => ()		(* archives must exist *)
		  | SOME upck =>
		      if upck (SS.listItems moduleset) then ()
		      else fail ["unpacking failed\n"]
		(* end case *))
	(* at the end, read lib/pathconfig and eliminate duplicate entries *)
	  fun uniqconfig () = let
		fun swallow (f, m) = pc_fold SM.insert m f
		fun finish m = let
		      val s = TextIO.openOut cm_pathconfig
		      fun one (k, v) = TextIO.output (s, concat [k, " ", v, "\n"])
		      in
			SM.appi one m; TextIO.closeOut s
		      end
		in
		  finish (pc_fold SM.insert SM.empty cm_pathconfig)
		end
	(* register library to be built *)
	  fun reglib { anchor, altanchor, relname, dir } = let
	      (* anchor: the anchor name currently used by the library
	       *   to be registered for compilation
	       * altanchor: optional alternative anchor name which is
	       *   to be used once the library is in its final location
	       *   (this must be used if "anchor" is already bound
	       *   and used for other libraries which come from the
	       *   bootfile bundle),
	       * relname: path to library's .cm file relative to anchor
	       *   (standard syntax)
	       * dir: directory name that anchor should be bound to,
	       *   name is relative to smlnjroot and in standard syntax *)
		val nrelname = native relname
		val ndir = native dir
		val libname = P.concat ("$" ^ anchor, nrelname)
		val adir = P.concat (smlnjroot, ndir)
		val finalanchor = getOpt (altanchor, anchor)
		val { dir = nreldir, file = relbase } = P.splitDirFile nrelname
		val relloc = U.pconcat [nreldir, CM.cm_dir_arc, arch_oskind, relbase]
		val srcfinalloc = P.concat (adir, relloc)
		val (finalloc, finalconfigpath) =
		      (U.pconcat [libdir, finalanchor, relloc], finalanchor)
		in
		  if U.fexists finalloc
		    then (
		      say ["Library ", libname, " already existed in ",
		      finalloc, ".  Will rebuild.\n"];
		      U.rmfile finalloc)
		    else ();
		  if U.fexists srcfinalloc then U.rmfile srcfinalloc else ();
		  if not (U.fexists (P.concat (adir, nrelname)))
		    then fail [
			"Source tree for ", libname, " at ",
			P.concat (adir, nreldir), "(", relbase,
			") does not exist.\n"
		      ]
		    else (
		      say [
			  "Scheduling library ", libname, " to be built as ",
			  finalloc, "\n"
			];
		      stablist := (fn () => CM.stabilize false libname) :: !stablist;
		      #set (CM.Anchor.anchor anchor) (SOME adir);
		      movlist := movelib srcfinalloc finalloc :: !movlist;
		      write_cm_pathconfig (finalanchor, finalconfigpath))
		end (* reglib *)
	  fun command_pathconfig target =
		write_cm_pathconfig (target, P.concat (P.parentArc, "bin"))
	(* build a standalone program, using an auxiliary build script *)
	  fun standalone { target, optheapdir, dir } = let
	      (* target: name of program; this is the same as the basename
	       *   of the heap image to be generated as well as the
	       *   final arc of the source tree's directory name
	       * optheapdir: optional subdirectory where the build command
	       *   drops the heap image
	       * dir:
	       *   The source tree for the target, relative to smlnjroot.
	       *)
		val heapname = concat [target, ".", heap_suffix]
	      (* where we expect the resulting heap image to be placed *)
		val targetheaploc = (case optheapdir
		       of NONE => heapname
			| SOME hd => P.concat (native hd, heapname)
		      (* end case *))
	      (* directory that has the build script *)
		val treedir = P.concat (smlnjroot, native dir)
	      (* path to the final heap image *)
		val finalheaploc = P.concat (heapdir, heapname)
		val alreadyExists = U.fexists finalheaploc
		fun finish () = (
		      instcmd target;
		      #set (CM.Anchor.anchor target) (SOME bindir))
		in
		  if alreadyExists
		    then say ["Target ", target, " already exists; will rebuild.\n"]
		    else ();
		  if not (U.fexists treedir)
		    then fail [
			"Source tree for ", target, " at ", treedir, " does not exist.\n"
		      ]
		    else (
		      say ["Building ", target, ".\n"];
		      F.chDir treedir;
		      if OS.Process.system buildcmd <> OS.Process.success
			then fail ["Building ", target, " failed.\n"]
		      else if not alreadyExists
		      andalso not(U.fexists targetheaploc)
		      andalso U.fexists finalheaploc
		        (* the build script already put the heap image where it belongs *)
			then finish ()
		      else if U.fexists targetheaploc
			then (
			  if alreadyExists
			    then U.rmfile finalheaploc
			    else ();
			  U.rename { old = targetheaploc, new = finalheaploc };
			  finish ())
			else fail ["Built ", target, "; ", heapname, " still missing.\n"]
(* old version
		      if OS.Process.system buildcmd = OS.Process.success
			then if U.fexists targetheaploc
			  then (
			    if alreadyExists
			      then U.rmfile finalheaploc
			      else ();
			    U.rename { old = targetheaploc, new = finalheaploc };
			    instcmd target;
			    #set (CM.Anchor.anchor target) (SOME bindir))
			  else fail ["Built ", target, "; ", heapname, " still missing.\n"]
			else fail ["Building ", target, " failed.\n"];
*)
		      command_pathconfig target;
		      F.chDir smlnjroot)
		end (* standalone *)
	(* configure a module *)
	  fun configure {target, dir} = let
		val treedir = P.concat (smlnjroot, native dir)
		in
		  if not (U.fexists treedir)
		    then fail [
			"Source tree for ", target, " at ", treedir, " does not exist.\n"
		      ]
		    else (
		      say ["Configuring ", target, ".\n"];
		      F.chDir treedir;
		      if OS.Process.system configcmd = OS.Process.success
			then ()
			else fail ["Configuration of ", target, " failed.\n"];
		      F.chDir smlnjroot)
		end
	(* perform the actions for the given module in order of specification *)
	  fun one module = let
		fun perform (RegLib (args, justunix)) =
		      if not justunix orelse isUnix then reglib args else ()
		  | perform (Anchor ({ anchor, path }, false)) =
		      #set (CM.Anchor.anchor anchor) (SOME (native path))
		  | perform (Anchor ({ anchor, path }, true)) =
		      #set (CM.Anchor.anchor anchor)
			   (SOME (P.concat (libdir, native path)))
		  | perform (Program (args, false)) =
		      standalone args
		  | perform (Program (args, true)) =
		      salist := (fn () => standalone args) :: (!salist)
		  | perform (Config args) = configure args
		in
		  case SM.find (actions, module)
		   of SOME al => app perform (rev al)
		    | NONE => fail ["unknown module: ", module, "\n"]
		end
	  in
	    ( command_pathconfig "bindir";	(* dummy -- for CM make tool *)
	      app one modules;
	      if not (dostabs ())
		then fail ["stabilization of libraries failed\n"]
	      else if not (domoves ())
		then fail ["post stabilization moves failed\n"]
	      else if not (dolatesas ())
		then fail ["late compiles failed\n"]
		else uniqconfig ()
	    ) handle e => fail ["unexpected exception: ", General.exnMessage e, "\n"];
	    OS.Process.exit OS.Process.success
	  end (* proc *)

  end
