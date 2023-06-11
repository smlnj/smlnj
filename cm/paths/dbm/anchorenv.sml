(* cm/paths/dbm/anchorenv.sml *)

(* environments: anchor-path env, anchor-path functional env (formerly "bound") *)

(* The path environment (mapping anchors to paths) is global, and is now implemented as
 * a structure PathEnv.  It maintains a mapping from anchors to paths (path SM.map)
 * as its state, and opertions get, set, defined, and reset operate on this internal state.
 * The path that is associated with an anchor by this environment is assumed to designate
 * the "anchor point" of the anchor (presumably a directory), i.e. the directory "named" by
 * the anchor.
 *
 * structure : PathEnv
 * This stateful module embodies an anchor to path environment as an "object" structure.
 * The mapping from anchors to paths is stored in the local reference variable anchorMapRef.
 *
 * getAnchor a: lookup the anchor a and return the associated path, if a is bound in the current
 *   map (!anchorMapRef).  Causes a fatal error (Compiler Bug) if a is not in the domain
 *   of the map.
 *
 * setAnchor (a, SOME pp) : bind (or rebind) the anchor a to path pp, modifying the map
 * setAnchor (a, NONE) : remove a from the domain of the map if it is currently bound, otherwise
 *   do nothing.
 *
 * definedAnchor a : is the anchor a in the domain of the current map?
 *
 * resetAnchors () : replace the current map with the empty SM.map, thus reseting the path
 *   anchor environment to be the empty environment.
 *
 * An anchor that is not in the domain of the current path map (!anchorMapRef) can be
 * considered to be "invalid".
 * The operations getAnchor and setAnchor and definedAnchor are not called outside this file.
 *   while (some other version of?) reset is called in many files in cm.
 * getAnchor is only called in get_anchor and mk_anchor.
 * setAnchor is only called in setRelative, which is used to define the exported set_anchor.
 *
 * Previously, the anchor to path environment(s) were created by a function newEnv
 * that was called just once in each of the files main/cm-boot.sml and bootstrap/btcompile.sml.
 * I conjecture that there was never more than one (global) instance of this environment,
 * and hence it is safe to replace any (indirect) references to the environments created by
 * newEnv with references to the global PathEnv structure defined inwith SrcPath.
 *
 * There is still a separate "functional" environment, of type dpathEnv, that maps
 * anchors to dpaths.  These dpath environment are passed as parameters to the
 * exported SrcPath functions bind, decodeFpath, and unpickle.
 *
 * The global path environment is represented by the structure PathEnv and path
 * anchor environments are no longer passed as parameters.
 *
 * QUESTION: Why do we need two anchor environments, the global, stateful one in
 *   PathEnv and the functional version (type dpathEnv)?
 *
 * QUESTION: Does the global environment correspond to the "root anchor environment" mentioned
 *   in Sec 3.4 of the CM manual?  Or does "root anchor environment" refer to the dual global
 *   and "local" anchor environments?
 *
 * QUESTION: How (where) do the pathconfig files system/pathconfig, config/extrpathconfig,
 *   and $SMLNJ/lib/pathconfig contribute to initializing the anchor environments (and which
 *   environments are initialized)?
 *)

structure AnchorEnv : ANCHORENV =
struct

local

  structure P = Path
  structure SM = StringMap

in

  type pathEnv = P.path SM.map

  val emptyPathEnv : pathEnv = SM.empty

  (* bindAnchors : dpathEnv -> (anchor * dpath) list -> dpathEnv *)
  (* produces a new env record with only the "bound" field altered.
   * Anchors are bound to corresponding dpaths, with these bindings being
   * added to the existing "bound" mapping.
   * exported
   * external: main/general-params.sml, elsewhere? *)
  fun bindAnchors (pathenv: pathEnv) (alist: (P.anchor * P.path) list) : pathEnv =
      let fun folder ((anchor, dpath), env) = SM.insert (env, anchor, dpath)
       in foldl folder pathenv alist
      end

  local (* global anchor environment *)

    val anchorMapRef : pathEnv ref = ref emptyPathEnv

    (* find : anchor -> path option *)
    (* locally used "look up" function for accessing the current state of the path
     * environment *)
    fun find anchor = SM.find (!anchorMapRef, anchor)

  in

    (* definedAnchor : anchor -> bool *)
    (* Is anchor bound in !anchorMapRef?, i.e. in the global path anchor environment? *)
    fun definedAnchor anchor = SM.inDomain (!anchorMapRef, anchor)

    (* getAnchor : anchor -> path option *)
    (* look up anchor in !anchorMapRef. If found, return a new elab for the anchor
     * containing the same path and validity as the existing binding. If not
     * found (anchor is not in the domain of !anchorMapRef), produces an undefined
     * anchor fatal error (impossible, compiler bug. So get should only be called with
     * an anchor that is known to be defined. *)
    fun getAnchor anchor = find anchor

    (* setAnchor : anchor * path option -> unit *)
    (* If pathOp is SOME path, binds or rebinds anchor to path in !anchorMapRef.
     * If pathOp is NONE, unbinds anchor in !anchorMapRef. *)
    fun setAnchor (anchor: P.anchor, pathOp: P.path option) : unit =
	case find anchor
	  of SOME _ =>
	       anchorMapRef :=
		 (case pathOp
		    of SOME path => SM.insert (!anchorMapRef, anchor, path) (* rebind *)
		     | NONE => #1 (SM.remove (!anchorMapRef, anchor))) (* remove *)
		       (* this can't raise NotFound because find returned SOME *)
	   | NONE =>
	       (case pathOp
		  of SOME path => anchorMapRef := SM.insert (!anchorMapRef, anchor, path)
		       (* bind the anchor to the path *)
		   | NONE => ()) (* do nothing *)

    (* resetAnchors : unit -> unit *)
    (* wipe out the contents of the path environment by setting anchorMapRef to SM.empty *)
    fun resetAnchors () = anchorMapRef := SM.empty  (* reset anchorMapRef to the empty map *)

  end (* local -- global anchor env *)

  (* lookAnchor : pathEnv * anchor -> path option
   * looks up anchor in "local" pathEnv, and if that fails, tries the global anchor env
   * not exported
   * local: native, standard *)
  fun lookAnchor (pathEnv: pathEnv, anchor: P.anchor) : P.path option =
      case SM.find (pathEnv, anchor)
	of NONE => getAnchor anchor (* anchor is not in pathEnv, try global anchor env *)
	 | pathOp => pathOp

  (* get_anchor : anchor -> path option
   * maps an anchor to the fpath of its "anchor point", if it is defined in the global anchor env *)
  val get_anchor = getAnchor

  (* setRelative [set0]: anchor * path option * path -> unit *)
  (* ASSERT: pathOp = SOME p => p is absolute
   * not exported
   * local: set_anchor *)
  fun setRelative (anchor: P.anchor, pathOp: P.path option, relativeTo: P.path) =
      setAnchor (anchor, Option.map (P.mkAbsolute relativeTo) pathOp)

  (* set_anchor : anchor * path option -> unit *)
  (* When fpathOp is SOME, binds a corresponding path to the anchor in env;
   * when fpathOp is NONE, deletes the anchor and its binding from env.
   * Exported and called externally (3 times) in main/cm-boot.sml *)
  fun set_anchor (anchor, pathOp) =
      setRelative (anchor, pathOp, getCwdPath ()) before sync ()

  (* reset_anchors : unit -> unit *)
  (* What does sync() do for us here?
     exported
   * external: main/cm-boot.sml (in resetPathConfig) *)
  fun reset_anchors () = (resetAnchors (); File.sync ())

end (* top local *)
end (* structure AnchorEnv *)
