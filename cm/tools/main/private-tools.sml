(*
 * This is the implementation of the private interface to CM's tools
 * mechanism.  It lacks certain public features implemented by ToolsFn
 * but provides other, non-public routines such as "expand".
 *
 *   (C) 2006 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
structure PrivateTools : PRIVATETOOLS = struct

    type class = string

    type srcpath = SrcPath.file
    type presrcpath = SrcPath.prefile
    type rebindings = SrcPath.rebindings

    val nativeSpec = SrcPath.osstring_relative
    val nativePreSpec = SrcPath.osstring_prefile_relative

    val srcpath = SrcPath.file

    val augment = SrcPath.extend

    exception ToolError of { tool: string, msg: string }

    type pathmaker = unit -> presrcpath

    type fnspec = { name: string, mkpath: pathmaker }

    datatype toolopt =
	STRING of fnspec
      | SUBOPTS of { name: string, opts: toolopts }
    withtype toolopts = toolopt list

    type tooloptcvt = toolopts option -> toolopts option

    type spec = { name: string,
		  mkpath: pathmaker,
		  class: class option,
		  opts: toolopts option,
		  derived: bool }

    type setup = string option * string option

    type controller =
	 { save'restore: unit -> unit -> unit,
	   set: unit -> unit }

    type smlparams =
	 { share: Sharing.request,
	   setup: setup,
	   noguid: bool,
	   locl: bool,
	   controllers: controller list }

    type cmparams =
	 { version: Version.t option,
	   rebindings: rebindings }

    type expansion =
	 { smlfiles: (srcpath * smlparams) list,
	   cmfiles: (srcpath * cmparams) list,
	   sources: (srcpath * { class: string, derived: bool}) list }

    type partial_expansion = expansion * spec list

    type rulefn = unit -> partial_expansion
    type rulecontext = rulefn -> partial_expansion
    type rule = { spec: spec,
		  native2pathmaker: string -> pathmaker,
		  context: rulecontext,
		  defaultClassOf: fnspec -> class option,
		  sysinfo: { symval: string -> int option,
			     archos: string } } ->
		partial_expansion

    type gcarg = { name: string, mkfname: unit -> string }

    type registry = { classes : rule StringMap.map ref,
		      sfx_classifiers : (string -> class option) ref,
		      gen_classifiers : (gcarg -> class option) ref }

    fun layer (look1, look2) s = case look1 s of NONE => look2 s | x => x

    fun newRegistry () =  { classes = ref StringMap.empty,
			    sfx_classifiers = ref (fn _ => NONE),
			    gen_classifiers = ref (fn _ => NONE) } : registry

    (* Three registries:
     *  1. global: where globally available tools are registered and found.
     *  2. local: where locally available tools are found;
     *            the local registry is being set anew every time "expand"
     *            is being called; each instance of a local registry belongs
     *            to one description file that is being processed.
     *  3. plugin registries: mapping from tool implementations (indexed
     *            by their respective description files) to that tool's
     *            registry; here is where local tools register themselves;
     *            the rule for the "tool" class causes the tool to register
     *            itself if it has not already done so and then merges
     *            the contents of the tool's registry into the current
     *            local registry.
     * These complications exist because tools register themselves via
     * side-effects. *)

    val global_registry = newRegistry ()

    val local_registry : registry ref = ref (newRegistry ())

    val plugin_registries : registry SrcPathMap.map ref = ref SrcPathMap.empty

    val current_plugin : SrcPath.file option ref = ref NONE

    local
	fun registry sel cvt s = let
	    val get = cvt o ! o sel
	in
	    layer (get (!local_registry), get global_registry) s
	end
	fun curry f x y = f (x, y)
    in
        val classes = registry #classes (curry StringMap.find)
        val sfx_classifiers = registry #sfx_classifiers (fn x => x)
	val gen_classifiers = registry #gen_classifiers (fn x => x)
    end

    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of gcarg -> class option

    fun stdSfxClassifier { sfx, class } =
	SFX_CLASSIFIER (fn e => if sfx = e then SOME class else NONE)

    local
	fun upd sel augment = let
	    val rf =
		sel (case !current_plugin of
			 NONE => global_registry
		       | SOME p =>
			 (case SrcPathMap.find (!plugin_registries, p) of
			      SOME r => r
			    | NONE => let
				  val r = newRegistry ()
			      in
				  plugin_registries :=
				  SrcPathMap.insert (!plugin_registries, p, r);
				  r
			      end))
	in
	    rf := augment (!rf)
	end
    in
        fun registerClass (class, rule) =
	    upd #classes (fn m => StringMap.insert (m, class, rule))
	fun registerClassifier (SFX_CLASSIFIER c) =
	    upd #sfx_classifiers (fn c' => layer (c, c'))
	  | registerClassifier (GEN_CLASSIFIER c) =
	    upd #gen_classifiers (fn c' => layer (c, c'))

	fun transfer_local p = let
	    val lr = !local_registry
	in
	    case SrcPathMap.find (!plugin_registries, p) of
		NONE => ()
	      | SOME pr => let
		    fun upd sel join = sel lr := join (! (sel pr), ! (sel lr))
		in
		    upd #classes (StringMap.unionWith #1);
		    upd #sfx_classifiers layer;
		    upd #gen_classifiers layer
		end
	end

	fun withPlugin p thunk =
	    SafeIO.perform { openIt = fn () => !current_plugin before
					       current_plugin := SOME p,
			     closeIt = fn prev => (transfer_local p;
						   current_plugin := prev),
			     work = fn _ => thunk (),
			     cleanup = fn _ => () }
    end

    datatype extensionStyle
      = EXTEND of (string * class option * tooloptcvt) list
      | REPLACE of string list * replacement list
      | RENAME of string list * (string -> replacement list)

    withtype replacement = string * class option * tooloptcvt

    local
      val splitBE = OS.Path.splitBaseExt
      val joinBE = OS.Path.joinBaseExt
      fun extInList (ext : string, l) = List.exists (fn e => (e = ext)) l
    in
    fun extend (EXTEND l) (f, too) =
	  map (fn (s, co, toc) => (joinBE { base = f, ext = SOME s }, co, toc too)) l
      | extend (REPLACE (ol, nl)) (f, too) = let
	  val { base, ext } = splitBE f
	  fun join b (e, co, toc) = (joinBE { base = b, ext = SOME e }, co, toc too)
	  fun gen b = map (join b) nl
	  in
	    case ext
	     of NONE => gen base
	      | SOME e => if extInList(e, ol) then gen base else gen f
	  end
      | extend (RENAME(ol, genFn)) (f, too) = let
	  val { base, ext } = splitBE f
	(* generate a replacement for base `b` using generator function `f` *)
	  fun gen b = let
		fun gen' (f, co, toc) = (f, co, toc too)
		in
		  List.map gen' (genFn b)
		end
	  in
	    case ext
	     of NONE => gen base
	      | SOME e => if extInList(e, ol) then gen base else gen f
	    (* end case *)
	  end
    end (* local *)

    local
	fun timex f =
	    (OS.FileSys.modTime f, true)
	    handle _ => (Time.zeroTime, false)
	val op < = Time.<
	fun olderThan t f = OS.FileSys.modTime f < t
	fun cannotAccess tool f =
	    raise ToolError { tool = tool, msg = "cannot access " ^ f }
    in
        fun outdated tool (l, f) = let
	    val (ftime, fexists) = timex f
	in
	    (List.exists (olderThan ftime) l)
	    handle _ => if fexists then true else cannotAccess tool f
	end

	fun outdated' tool { src, wtn, tgt } = let
	    val (st, se) = timex src
	    val (tt, te) = timex tgt
	in
	    if not se then
		if te then false else cannotAccess tool src
	    else if te then
		let val (wt, we) = timex wtn
		in
		    if we then wt < st else tt < st
		end
	    else true
	end

	fun targetsExist l = List.all (#2 o timex) l
    end (* local *)

    val openTextOut = AutoDir.openTextOut
    val makeDirs = AutoDir.makeDirs

    fun globally lp arg =
	SafeIO.perform { openIt = fn () => !current_plugin before
					   current_plugin := NONE,
			 closeIt = fn prev => current_plugin := prev,
			 work = fn _ => lp arg,
			 cleanup = fn _ => () }

    (* query default class *)
    fun defaultClassOf load_plugin (s: fnspec) = let
	val p = #name s
	val mkfname = SrcPath.osstring_prefile o #mkpath s
	val gcarg = { name = p, mkfname = mkfname }
	fun sfx_gen_check e =
	    case sfx_classifiers e of
		SOME c => SOME c
	      | NONE => gen_classifiers gcarg
    in
	case OS.Path.ext p of
	    SOME e =>
	    (case sfx_gen_check e of
		 SOME c => SOME c
	       | NONE => let
		     val plugin = concat ["$/", e, "-ext.cm"]
		 in
		     if globally load_plugin plugin then sfx_gen_check e
		     else NONE
		 end)
	  | NONE => gen_classifiers gcarg
    end

    fun parseOptions { tool, keywords, options } = let
	fun err m = raise ToolError { tool = tool, msg = m }
	fun isKW kw = List.exists (fn kw' => kw = kw') keywords
	fun loop ([], m, ro) = { matches = fn kw => StringMap.find (m, kw),
				 restoptions = rev ro }
	  | loop (STRING { name, ... } :: t, m, ro) = loop (t, m, name :: ro)
	  | loop (SUBOPTS { name, opts } :: t, m, ro) =
	    if not (isKW name) then
		raise err (concat ["keyword option `", name,
				   "' not recognized"])
	    else (case StringMap.find (m, name) of
		      SOME _ => err (concat ["keyword option `", name,
					     "' specified more than once"])
		    | NONE => loop (t, StringMap.insert (m, name, opts), ro))
    in
	loop (options, StringMap.empty, [])
    end

  (* a controller that wraps a bool ref *)
    fun flagController flg = {
	    save'restore = fn () => let val orig = !flg in fn () => flg := orig end,
	    set = fn () => flg := true
	  }

  (* dialects of SML that are handled by the smlrule function *)
    datatype lang = StandardML | LazySML | SuccessorML

    fun smlrule language {spec, context, native2pathmaker, defaultClassOf, sysinfo} = let
        val { name, mkpath, opts = oto, derived, ... } : spec = spec
	val tool = "sml"
	fun err s = raise ToolError { tool = tool, msg = s }
	fun fail s = raise Fail ("(SML Tool) " ^ s)
	val kw_setup = "setup"
	val kw_with = "with"
	val kw_noguid = "noguid"
	val kw_local = "local"
	val kw_lazy = "lazy"
	val kw_succML = "succ-ml"
	val Suggest = SOME
	val lazy_controller = flagController Control.lazysml
	val succML_controller = flagController ParserControl.succML
	val (srq, setup, noguid, locl, controllers) =
	    case oto of
		NONE => let
		  val controllers = (case language
			 of StandardML => []
			  | LazySML => [lazy_controller]
			  | SuccessorML => [succML_controller]
			(* end case *))
		  in
		    (Sharing.DONTCARE, (NONE, NONE), false, false, controllers)
		  end
	      | SOME to => let
		    val { matches, restoptions } =
			parseOptions { tool = tool,
				       keywords = [kw_setup,
						   kw_with],
				       options = to }
		    fun is_shspec "shared" = true
		      | is_shspec "private" = true
		      | is_shspec _ = false
		    val (sh_options, restoptions) =
			List.partition is_shspec restoptions
		    val srq =
			case sh_options of
			    [] => Sharing.DONTCARE
			  | ["shared"] => Sharing.SHARED
			  | ["private"] => Sharing.PRIVATE
			  | _ => err "invalid option(s)"
		    fun isKW (kw : string) s = (kw = s)
		  (* check for 'local' option *)
		    val (locls, restoptions) = List.partition (isKW kw_local) restoptions
		    val locl = not (List.null locls)
		  (* check for 'noguid' option *)
		    val (noguids, restoptions) = List.partition (isKW kw_noguid) restoptions
		    val noguid = not (List.null noguids)
		  (* check for 'lazy' option *)
		    val (lazies, restoptions) = List.partition (isKW kw_lazy) restoptions
		    val lazysml = (language = LazySML) orelse not (List.null lazies)
		  (* check for 'succ-ml' option *)
		    val (succMLs, restoptions) = List.partition (isKW kw_succML) restoptions
		    val succML = (language = SuccessorML) orelse not (List.null succMLs)
		    val _ = if List.null restoptions then ()
			    else err (String.concatWith " " ("invalid option(s): " :: restoptions))
		    val setup =
			case matches kw_setup of
			    NONE => (NONE, NONE)
			  | SOME [] => (NONE, NONE)
			  | SOME [STRING s] => (SOME (#name s), NONE)
			  | SOME [SUBOPTS { name = "pre",
					    opts = [STRING pre] }] =>
			    (SOME (#name pre), NONE)
			  | SOME [SUBOPTS { name = "post",
					    opts = [STRING post] }] =>
			    (NONE, SOME (#name post))
			  | (SOME [SUBOPTS { name = "pre",
					     opts = [STRING pre] },
				   SUBOPTS { name = "post",
					     opts = [STRING post] }] |
			     SOME [SUBOPTS { name = "post",
					     opts = [STRING post] },
				   SUBOPTS { name = "pre",
					     opts = [STRING pre] }]) =>
			    (SOME (#name pre), SOME (#name post))
			  | _ => err "invalid setup spec"

		    val controllers =
			case matches kw_with of
			    NONE => []
			  | SOME subopts => let
				fun fields c s =
				    String.fields (fn c' => c = c') s
				fun set (c, v) =
				    Controls.set' (c, v)
				    handle Controls.ValueSyntax vse =>
					   fail (concat ["error setting \
							 \ controller: \
							 \unable to parse \
							 \value `",
							 #value vse, "' for ",
							 #ctlName vse, " : ",
							 #tyName vse])
				fun mk (n, v) =
				    case ControlRegistry.control
					     BasicControl.topregistry
					     (fields #"." n)
				     of NONE =>
					  err ("no such control: " ^ n)
				      | SOME c =>
					  { save'restore =
					      fn () => Controls.save'restore c,
					    set = set (c, v) }
				fun loop ([], a) = a
				  | loop (STRING nv :: r, a) =
				      (case fields #"=" (#name nv) of
					   [n, v] => loop (r, mk (n, v) :: a)
					 | [n] => loop (r, mk (n, "true") :: a)
					 | _ => err "invalid controller spec")
				  | loop (SUBOPTS { name = "name",
						    opts = [STRING n] } ::
					  SUBOPTS { name = "value",
						    opts = [STRING v] } :: r,
					  a) =
				      loop (r, mk (#name n, #name v) :: a)
				  | loop (SUBOPTS { name = "name",
						    opts = [STRING n] } :: r,
					  a) =
				      loop (r, mk (#name n, "true") :: a)
				  | loop _ = err "invalid controller spec"
			    in
				loop (subopts, [])
			    end

		    val controllers = (case language
			   of StandardML => controllers
			    | LazySML => lazy_controller :: controllers
			    | SuccessorML => succML_controller :: controllers
			  (* end case *))
		in
		    (srq, setup, noguid, locl, controllers)
		end
	val p = srcpath (mkpath ())
	val sparam = { share = srq, setup = setup,
		       noguid = noguid,
		       locl = locl, controllers = controllers }
    in
	({ smlfiles = [(p, sparam)],
	   sources = [(p, { class = "sml", derived = derived })],
	   cmfiles = [] },
	 [])
    end
    fun cmrule { spec, context, native2pathmaker, defaultClassOf, sysinfo } = let
	val { name, mkpath, opts = oto, derived, ... } : spec = spec
	fun err m = raise ToolError { tool = "cm", msg = m }
	fun proc_opts (rb, vrq, []) = (rb, vrq)
	  | proc_opts (_, _, STRING _ :: _) = err "ill-formed option"
	  | proc_opts (rb, vrq, SUBOPTS { name = "version", opts } :: r)  =
	    let fun ill () = err "ill-formed version specification"
	    in
		case (vrq, opts) of
		    (SOME _, _) =>
		    err "version cannot be specified more than once"
		  | (NONE, [STRING { name, ... }]) =>
		    (case Version.fromString name of
			 NONE => ill ()
		       | SOME v => proc_opts (rb, SOME v, r))
		  | _ => ill ()
	    end
	  | proc_opts (rb, vrq, SUBOPTS { name = "bind", opts } :: r) =
	    (case opts of
		 [SUBOPTS { name = "anchor", opts = [STRING { name, ... }] },
		  SUBOPTS { name = "value", opts = [STRING v] }] =>
		 proc_opts ({ anchor = name, value = #mkpath v () }
			    :: rb,
			    vrq, r)
	       | _ => err "ill-formed bind specification")
	  | proc_opts (_, _, SUBOPTS { name, ... } :: _) =
	    err ("unknown option: " ^ name)
	val (rb, vrq) = case oto of
			    NONE => ([], NONE)
			  | SOME l => proc_opts ([], NONE, l)
	val p = srcpath (mkpath ())
	val cparams = { version = vrq, rebindings = rev rb }
    in
	({ smlfiles = [],
	   sources = [(p, { class = "cm", derived = derived })],
	   cmfiles = [(p, cparams)] },
	 [])
    end

    fun expand { error, local_registry = lr, spec, context, load_plugin, sysinfo } = let
	val dummy = ({ smlfiles = [], cmfiles = [], sources = [] }, [])
	fun norule _ = dummy
	fun native2pathmaker s () =
	    SrcPath.raw { err = error } { context = context, spec = s }
	fun class2rule class =
	    case classes class of
		SOME rule => rule
	      | NONE => let
		    val base = concat ["$/", class, "-tool"]
		    val plugin = OS.Path.joinBaseExt { base = base,
						       ext = SOME "cm" }
		    fun complain () =
			(error (concat ["unknown class: ", class]);
			 norule)
		in
		    if globally (load_plugin context) plugin then
			case classes class of
			    SOME rule => rule
			  | NONE => complain ()
		    else complain ()
		end

	fun expand1 (spec as { name, mkpath, class = co, ... }) = let
	    val fns = { name = name, mkpath = mkpath }
	    val rule =
		case co of
		    SOME c0 => class2rule (String.map Char.toLower c0)
		  | NONE =>
			(case defaultClassOf (load_plugin context) fns of
			     SOME c => class2rule c
			   | NONE =>
			     (error (concat ["unable to classify: ", name]);
			      norule))
	    fun rcontext rf = let
		val dir = SrcPath.osstring_dir context
		val cwd = OS.FileSys.getDir ()
	    in
		SafeIO.perform { openIt = fn () => OS.FileSys.chDir dir,
				 closeIt = fn () => OS.FileSys.chDir cwd,
				 work = rf,
				 cleanup = fn _ => () }
	    end
	in
	    rule { spec = spec, context = rcontext,
		   native2pathmaker = native2pathmaker,
		   defaultClassOf = defaultClassOf (load_plugin context),
		   sysinfo = sysinfo }
	    handle ToolError { tool, msg } =>
		   (error (concat ["tool \"", tool, "\" failed: ", msg]);
		    dummy)
	end
	fun loop (expansion, []) = expansion
	  | loop ({ smlfiles, cmfiles, sources }, item :: items) = let
		val ({ smlfiles = sfl, cmfiles = cfl, sources = sl }, il) =
		    expand1 item
	    in
		loop ({ smlfiles = smlfiles @ sfl,
			cmfiles = cmfiles @ cfl,
			sources = sources @ sl },
		      il @ items)
	    end
    in
	SafeIO.perform { openIt = fn () => !local_registry
					   before local_registry := lr,
			 closeIt = fn prev => local_registry := prev,
			 work = fn _ => loop ({ smlfiles = [], cmfiles = [],
						sources = [] },
					      [spec]),
			 cleanup = fn _ => () }
    end

    local
	fun sfx (s, c) =
	    registerClassifier (stdSfxClassifier { sfx = s, class = c })
    in
	val _ = registerClass ("sml", smlrule StandardML)
	val _ = registerClass ("lazysml", smlrule LazySML)
	val _ = registerClass ("succ-ml", smlrule SuccessorML)
	val _ = registerClass ("cm", cmrule)

	val _ = sfx ("sml", "sml")
	val _ = sfx ("lml", "lazysml")
	val _ = sfx ("sig", "sml")
	val _ = sfx ("fun", "sml")
	val _ = sfx ("cm", "cm")
    end
end
