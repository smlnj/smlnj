(*
 * The common core of both public and private interface to CM's tools
 * mechanism.
 *
 *   (C) 2006 The Fellowship of SML/NJ
 *
 * Author: Matthias Blume (blume@tti-c.org)
 *)
signature CORETOOLS = sig

    (* We don't make classes abstract.  It doesn't look like there
     * would be much point to it. *)
    type class = string

    (* We keep source paths abstract. Tool writers should not mess
     * with their internals.
     * The function that makes a srcpath from a string is passed as
     * part of the input specification (type "spec").  Which function
     * is originally being passed depends on which syntax was used for
     * this member in its .cm-file.  Most tools will want to work on
     * native pathname syntax (function "outdated" -- see below -- depends
     * on native syntax!).  In these cases the tool should first convert
     * the name to a srcpath and then get the native string back by
     * applying "nativeSpec". *)
    type srcpath
    type presrcpath

    type rebindings = { anchor: string, value: presrcpath } list

    (* get the spec (i.e., relative to the directory context) of a path: *)
    val nativeSpec : srcpath -> string
    (* same for presrcpath... *)
    val nativePreSpec : presrcpath -> string

    (* make a srcpath from a presrcpath, checking that the "at least one
     * arc" rule is satisfied... *)
    val srcpath : presrcpath -> srcpath

    (* augment a presrcpath with extra arcs; the new path has inherits
     * the context (i.e., any anchoring) from the original one... *)
    val augment : presrcpath -> string list -> presrcpath

    exception ToolError of { tool: string, msg: string }

    type pathmaker = unit -> presrcpath

    (* a file-name specification *)
    type fnspec = { name: string, mkpath: pathmaker }

    (* case-by-case parameters that can be passed to tools... *)
    datatype toolopt =
	STRING of fnspec
      | SUBOPTS of { name: string, opts: toolopts }
    withtype toolopts = toolopt list

    type tooloptcvt = toolopts option -> toolopts option

    (* A member specification consists of the actual string, an optional
     * class name, (optional) tool options, a function to convert a
     * string to its corresponding srcpath, and information about whether
     * or not this source is an "original" source or a derived source
     * (i.e., output of some tool). *)
    type spec = { name: string,
		  mkpath: pathmaker,
		  class: class option,
		  opts: toolopts option,
		  derived: bool }

    type setup = string option * string option (* (pre, post) *)

    (* A controller is a generic mechanism for manipulating state.
     * The first stage of save'restore is meant to capture the part of
     * the state in question so that the second stage can restore it.
     * Function set, on the other hand, is meant to establish the
     * new state.  All controllers associated with an SML source are
     * invoked for both parsing and compilation.
     * Roughly speaking, given a controller c, each of these two phases
     * is bracketed as follows:
     *
     *   let val restore = #save'restore c ()
     *   in #set c (); parse_or_compile () before restore () end
     *)
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

    (* The goal of applying tools to members is to obtain an "expansion",
     * i.e., a list of ML-files and a list of .cm-files.  We also
     * obtain a list of "sources".  This is used to implement CM.sources,
     * i.e., to generate dependency information etc. *)
    type expansion =
	 { smlfiles: (srcpath * smlparams) list,
	   cmfiles: (srcpath * cmparams) list,
	   sources: (srcpath * { class: class, derived: bool}) list }

    (* A partial expansion is an expansion with a list of things yet to be
     * expanded... *)
    type partial_expansion = expansion * spec list

    (* A rule takes a spec and a rulecontext where the name contained
     * in the spec -- if relative -- is considered relative to the directory
     * of the corresponding description file.  In general,
     * when coding a rule one would write a rule function and pass it to
     * the context, which will temporarily change the current working
     * directory to the one that holds the description file ("the context").
     * If this is not necessary for the rule to work correctly, then
     * one can simply ignore the context (this saves system call overhead
     * during dependency analysis).
     * If the rule yields a genuine partial expansion (where the resulting
     * spec list is not empty), then it must pass the proper "path maker"
     * along with each new name.  For most cases this will be the given
     * "native path maker" because most rules work on native path names.
     * Some rules, however, might want to use the same convention for
     * derived specs that was used for the original spec. *)
    type rulefn = unit -> partial_expansion
    type rulecontext = rulefn -> partial_expansion
    type rule = { spec: spec,
		  native2pathmaker: string -> pathmaker,
		  context: rulecontext,
		  defaultClassOf: fnspec -> class option,
		  sysinfo: { symval: string -> int option,
			     archos: string } } ->
		partial_expansion

    (* install a class *)
    val registerClass : class * rule -> unit

    (* classifiers are used when the class is not given explicitly *)
    datatype classifier =
	SFX_CLASSIFIER of string -> class option
      | GEN_CLASSIFIER of { name: string, mkfname: unit -> string } ->
			  class option

    (* make a classifier which looks for a specific file name suffix *)
    val stdSfxClassifier : { sfx: string , class: class } -> classifier

    (* three ways of dealing with filename extensions...
     * (Tool options can be calculated from the options that we have.) *)
    datatype extensionStyle
      = EXTEND of (string * class option * tooloptcvt) list
      | REPLACE of string list * replacement list
      | RENAME of string list * (string -> replacement list)

    withtype replacement = string * class option * tooloptcvt

    (* perform filename extension *)
    val extend : extensionStyle ->
		 (string * toolopts option) ->
		 (string * class option * toolopts option) list

    (* check for outdated files; the pathname strings must be in
     * native syntax! *)
    val outdated : string -> string list * string -> bool

    (* Alternative way of checking for outdated-ness using a "witness"
     * file.  The idea is that if both tgt (target) and wtn (witness)
     * exist, then tgt is considered outdated if wtn is older than src.
     * Otherwise, if tgt exists but wtn does not, then tgt is considered
     * outdated if it is older than src.  If tgt does not exist, it is
     * always considered outdated. *)
    val outdated' : string ->
		    { src: string, wtn: string, tgt: string } -> bool

    (* see if all given targets exist (regardless of time stamp) *)
    val targetsExist : string list -> bool

    (* open output file; make all necessary directories for it *)
    val openTextOut : string -> TextIO.outstream

    (* make all directories leading up to a given file; the file itself
     * is to be left alone *)
    val makeDirs : string -> unit

    (* install a classifier *)
    val registerClassifier : classifier -> unit

    (* grab all named options... *)
    val parseOptions :
	{ tool : string, keywords : string list, options : toolopts } ->
	{ matches : string -> toolopts option, restoptions : string list }
end
