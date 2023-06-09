(* cm/paths/dbm-srcpath.sig *)

(* Revision 8 -- adding and reorganizing Path functions + ? *)

signature SRCPATH =
sig

    type pathEnv  (* a "functional" anchor environment: anchor --> path *)

  (* paths *)

    type anchor = string

    type fpath = string   (* file path *)

    type arc = string  (* file path arcs *)
  
    type path  (* "abstract" file paths *)


    (* split off the final (innermost) arc of a path *)
    val splitPath : path -> (arc * path) option

    (* add a new innermost (final) arc to a path *)
    val addArc : arc * path -> path

    (* add a list of arcs (ordered outer to inner) to a path *)
    val addArcs : arc list * path -> path

    (* is a path absolute? *)
    val absolutePath : path -> bool

    (* returns path1 relative to path2, if path1 is a proper extension of path2 
     * where path2 is absolute *)
    val relativePath : path * path -> path

    (* expand an anchor root using a given anchor expansion function
     * returns path argument if it is not anchored *)
    val reanchorPath : (anchor -> path) -> path -> path

    (* an absolute path ==> the path relative to CWD, otherwise returns arg *)
    val cwdRelativePath : path -> path  (* replaces osstring' *)

  (* pickling and unpickling paths *)

    exception Format

    (* "pickle" a path as a string list *)
    val picklePath : path -> string list

    (* "unpickle" a string list to a path; raises Format *)
    val unpicklePath : string list -> path

  (* parsing fpaths and unparsing paths to fpaths *)

    (* parse file path strings into paths *)
    val parseFpath : fpath -> path

    (* native and standard file path parsers; with anchor expansion (once) *)
    val parseNative : pathEnv -> fpath -> path    (* redundant? *)
    val parseStandard : pathEnv -> fpath -> path  (* redundant? *)

    (* parsing segmented fpaths, with anchor expansion (once) *)
    val parseSegmented : pathEnv -> fpath -> path

    (* translate path to fpath (unparsing paths to strings) *)
    val pathToFpath : path -> fpath

  (* files *)

    type file

    val compareFile : file * file -> order

    (* return the path component of a file *)
    val fileToPath : file -> path

    (* create a partially defined file;
     * check that there is at least one arc in the path? *)
    val pathToFile : path -> file

    (* get a time stamp *)
    val tstamp : file -> TStamp.t

  (* interned files, "sync", CWD path, CWD change notifications *)

    (* "re-establish stability of ordering", DBM: What does this mean? *)
    (* reconstructs the internal set of interned files,
     * recomputing their file_ids (FileId.id). *)
    val sync : unit -> unit

    (* forget all known path names *)
    (* clears (resets to empty) the internal set of known, interned, files. *)
    val clear : unit -> unit

    (* register a "client module" with a function that notifies it when CWD changes.
     * Such notifications will be initiated by calls of the cwdPath function. *)
    val addClientToBeNotified : (unit -> unit) -> unit

    (* make sure all such clients get notified about the a CWD during "next validation". *)
    val scheduleNotification : unit -> unit

    (* fetch/revalidate the path of the current working directory;
     * notify registerd "clients" if the CWD has changed since last call. *)
    val cwdPath : unit -> path


  (* Anchor environments *)
  (* Two-level anchor environments:
   *   (1) global, stateful environment, implemented as a structure (PathEnv);
   *   (2) functional anchor environments (local or "dynamic") (type pathEnv).
   * Both kinds of anchor envronments map anchors to paths. *)

    (* accessing and destructively updating the global anchor-path environment (PathEnv) *)
    val get_anchor : anchor -> path option
    val set_anchor : anchor * path option -> unit
    val reset_anchors : unit -> unit

    (* non-destructive bindings for anchors (for "scoped" anchor bindings) *)
    val bindAnchors: pathEnv -> (anchor * path) list -> pathEnv

    (* looks for anchor first in pathEnv, then in global anchor env *)
    (* val lookAnchor : pathEnv -> anchor -> path option -- don't need to export this *)
							 

  (* parsing pathconfig ("spec") files  *)

    (* process a specification file; must sync afterwards! *)
    val processSpecFile : fpath -> TextIO.instream -> unit


end (* signature SRCPATH *)
