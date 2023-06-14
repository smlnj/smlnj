(* cm/paths/dbm/anchorenv.sig *)

signature ANCHORENV =
sig

  (* Anchor environments *)

  (* Two-level anchor environments, both mapping anchors to paths:
   *   (1) global, stateful environment (a stringMap reference)
   *   (2) functional anchor environments (local or "dynamic") (type pathEnv). *)

  (* accessing and destructively updating the global anchor-path environment *)
  val get_anchor : Path.anchor -> Path.path option
  val set_anchor : Path.anchor * Path.path option -> unit
  val reset_anchors : unit -> unit  (* reset the global anchor env to empty *)

  (* pathEnv : "functional" anchor environments mapping anchors to paths *)
  type pathEnv

  type anchorPathAlist = (P.anchor * P.path) list

  (* non-destructive bindings for anchors (for "scoped" or "local" anchor bindings) *)
  val bindAnchors: pathEnv -> anchorPathAlist -> pathEnv

  (* look for anchor first in pathEnv, then in the global anchor env if not found *)
  val lookAnchor : pathEnv -> Path.anchor -> Path.path option

end (* signature ANCHORENV  *)
