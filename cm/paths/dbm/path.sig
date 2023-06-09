(* cm/paths/dbm-path.sig *)


signature PATH =
sig

    type anchor = string

    type fpath = string   (* file path in "standard" (?) format *)

    type arc = string  (* file path arcs *)
  
    type path  (* "abstract syntax" for file paths *)

    (* construcing paths *)
    val rootedU : string list -> path
    val rootedV : string * string list -> path
    val relPath : int * string list -> path
    val anchorPath : string * string list -> path

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

    (* translate path to fpath (unparsing paths to strings) *)
    val pathToFpath : path -> fpath


end (* signature PATH *)
