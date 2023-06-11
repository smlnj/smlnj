(* cm/paths/dbm-path.sig *)

signature PATH =
sig

  type anchor = string

  type fpath = string   (* file path in "standard" (?) format *)

  type arc = string  (* file path arcs; should be non-empty strings *)

  type path  (* "abstract syntax" for file paths *)

(* construcing paths *)

  val rootedU : arc list -> path
  val rootedV : string * arc list -> path
  val relPath : int * arc list -> path
  val anchorPath : string * arc list -> path

(* manipulating paths *)

  (* split off the final (innermost) arc of a path *)
  val splitPath : path -> (arc * path) option

  (* add a new innermost (final) arc to a path *)
  val addArc : arc * path -> path

  (* add a list of arcs (ordered outer to inner) to a path *)
  val addArcs : arc list * path -> path

  (* is a path absolute? *)
  val absolutePath : path -> bool

  val mkAbsolute : path -> path -> path

  (* returns the relative path path1/path2, if path1 is a proper extension of path2
   * otherwise returns path1 unchanged;
   * where path2 is absolute; if path1 extends path2, then path1 must also be absolute *)
  val relativePath : path * path -> path

  (* if path2 is anchored, its anchor head is replaced by path1;
   * otherwise, returns path2 unchanged *)
  val substAnchor : path * path -> path

  (* expand an anchor root using a given anchor expansion function
   * returns path argument if it is not anchored *)
  val reanchorPath : (anchor -> path) -> path -> path

(* pickling and unpickling paths *)

  (* translate paths to and from a very simple string list representation (a "pickle") *)

  exception Format

  (* "pickle" a path as a string list *)
  val picklePath : path -> string list

  (* "unpickle" a string list to a path; raises Format *)
  val unpicklePath : string list -> path

(* parsing fpaths and unparsing paths to fpaths *)

  (* parse file path strings into paths *)
  val parseFpath : fpath -> path

  (* translate path to fpath: unparsing paths to file path strings in "standard" format *)
  val pathToFpath : path -> fpath

  (* parseNative is used for quoted file paths in CDFs -- OS native format; no anchor expansion *)
  val parseNative : fpath -> path    (* == parseFpath, redundant? *)

  (* parseStandard is used for unquoted file paths in CDFs -- "standard" format; no anchor expansion *)
  val parseStandard : fpath -> path  (* redundant? *)

  (* parsing segmented fpaths, with no anchor expansion *)
  val parseSegmented : fpath -> path

end (* signature PATH *)
