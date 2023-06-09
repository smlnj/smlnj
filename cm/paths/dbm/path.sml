(* cm/paths/dbm-path.sml *)

(* Path structure *)

structure Path :> PATH =
struct

    (* fpath: a file path in _standard_ format
     * Standard format: the format that OS.Path uses
     * but possibly starting with an anchorn (initial arc begins with #"$").
     * Any ambiguity about the format of an fpath should be cleared up! *)
    type fpath = string

    (* anchor: the name of an anchor, e.g. "smlnj" for the anchor $smlnj, non-empty *)
    type anchor = string

    type arc = string  (* non-empty, alphanumberic with initial letter, disallowing ".", and ".." *)

    type arcs = arc list  (* possibly empty, maintained in outside-in (reverse) order *)

    (* path root, differentiated by OS FS. Could unify by using the volume = "" convention for
       the Unix root. *)

    datatype root
      = U            (* Unique/Unix FS root directory *)
      | V of string  (* Volume (Windows) roots (where the _non-empty_ string is the volume name) *)

    (* the head of a path specifies the starting point of the path *)
    datatype head
      = ABS of root
      | REL of int   (* relative path with implicit root, defaulting to CWD, possibly empty arcs,
		      * int parent-levels, corresponding to the number of leading ".." arcs in standard
		      * path string format. *)
      | ANC of anchor

    (* type path
     * The convention is that the arcs are ordered from inner to outer (reverse of top to bottom).
     * However, it is unclear whether this has significant efficiency advantages over the more usual
     * top to bottom, or outer to inner, ordering, since it does require reversing the arc list for
     * some functions.  So this arc order convention might change. *)
    type path = head * arcs

    (* QUESTION: Are paths used to designate only regular files, e.g. CDFs, or do they sometimes denote
     * directories in the file system?  This relates to the question of whether empty arc lists are
     * allowed.  For instance, the Unix FS root directory would be denoted by (ABS U, nil).
     * Another question is when and whether anchor path heads (ANC anchor-name) directly designate
     * CDFs, in which case we will normally need to discover the parent directory of the CDF so
     * designated. The question also relates to that of whether a file is always a non-directory
     * file, and whether the path of a file can have an empty arc list. One assumes that if files
     * are always non-directory (e.g. CDFs), then their paths will always have at least one arc.*)


   (* *********************************************************************************** *)
   (* path functions *)

    (* path construction *)
    fun rootedU (arcs: string list) : path = (ABS U, arcs)
    fun rootedV (vol: string, arcs: string list) : path = (ABS (V vol), arcs)
    fun relPath (parentarcs: int, arcs: string list) : path = (REL parentarcs, arcs)
    fun anchorPath (anchor: string, arcs: string list) : path = (ANC anchor, arcs)

    (* rootPath : path *)
    fun rootPath (root : root) : path = (ABS root, nil)

    (* absolutePath : path -> bool *)
    (* is the argument path absolute? *)
    fun absolutePath (ABS _, _) = true
      | absolutePath _ = false 

    (* equalPaths : path * path -> bool *)
    (* (structural) equality of Paths *)
    fun equalPaths ((head1, arcs1), (head2, arcs2)) =
	let fun equalHeads (ABS U, ABS U) = true
	      | equalHeads (ABS (V vol1), ABS (V vol2)) = (vol1 = vol2)
	      | equalHeads (REL n1, REL n2) = (n1 = n2)
	      | equalHeads (ANC a1, ANC a2) = (a1 = a2)
	      | equalHeads _ = false
	in equalHeads (head1, head2) andalso
	   ListPair.allEq (fn (s1: string, s2) => (s1 = s2)) (arcs1, arcs2)
	end

    (* splitPath : path -> (arc * path) option  *)
    (* if there are arcs, split off the innermost, returning that arc and the trimmed path *)
    fun splitPath (head, arc::arcs) = SOME (arc, (head, arcs))
      | splitPath (_, nil) = NONE

    (* addArc : arc * path -> path *)
    (* add a single arc to the acs component of a path *)
    fun addArc (arc: arc, (head, arcs): path) = (head, arc::arcs)

    (* addArcs : arc list * path -> path *)
    (* similar to extendArcs, but arc list is not in reverse order *)
    fun addArcs (newarcs: arc list, (head, arcs): path) =
	  (head, List.revAppend (newarcs, arcs))

    (* arcsDiff : arc list * arc list -> arc list option *)
    (* assume both arc lists are in inner-to-outer order (i.e. reversed),
     * Returns SOME arcs if arcs1 is a prefix of arcs2, with arcs = arcs2 "-" arcs1  *)
    fun arcsDiff (arcs1: arc list, arcs2: arc list) =
	let fun diff (nil : arc list, ys) = SOME ys  (* ys = nil => arcs1 = arcs2 *)
	      | diff (x::xs, y::ys) = if x = y then diff (xs, ys) else NONE
	      | diff (_, nil) = NONE  (* ys ran out first *)
	 in Option.map rev (diff (rev arcs1, rev arcs2))
	end

    (* relativePath : [path1:]path * [path2:]path -> path *)
    (* path2 is required to be absolute (it will normally be the absolute CWD).
     * If path1 is absolute and is an extension of path2, return the relative path
     * for path1 relative to path2, otherwise return path1. *)
    fun relativePath (path1 as (head1, arcs1), (head2 as ABS root2, arcs2)) =
	(* ASSUME: head2 = ABS _ *)
	(case head1
	   of ABS root1 =>
	      if root1 = root2 (* both absolute with same root *)
	      then (case (arcsDiff (arcs2, arcs1))
		      of NONE => path1     (* path1 does not extend path2 *)
		       | SOME nil => path1 (* path1 = path2 *)
		       | SOME arcs => (REL 0, arcs))  (* path1 extends path2 by arcs *)
	      else path1  (* different roots, so path1 cannot extend path2 *)
	    | _ => path1)  (* path1 is not absolute *)
      | relativePath _ = impossible ["relativePath"]

    (* extendArcs : arcs -> path -> path
     * rarcs in reverse order, outer to inner, like the arcs in the path
     * not exported
     * local uses: native, standard *)
    fun extendArcs (rarcs: arc list) ((head, arcs): path) : path  =
	(head, (rarcs @ arcs))

    (* substAnchor : [anchorPath:]path -> [anchoredPath:]path -> path *)
    (* add anchoredPath's arcs onto anchorPath, effectively replacing anchoredPath's
     * anchor head with anchorPath *)
    fun substAnchor ((head, arcs) : path, (ANC _, arcs') : path) =
	  (head, arcs' @ arcs)
      | substAnchor _ = impossible ["substAnchor"]

    (* reanchorPath : (anchor -> path) -> path -> path *)
    (* used once in main/filename-policy.sml (FilenamePolicyFn)? *)
    fun reanchorPath (cvt: anchor -> path) (path as (head, arcs) : path) =
	  (case head
	     of ANC anchor => substAnchor (cvt anchor, path)
	      | _ => path)  (* path is not anchored, return it unchanged *)


   (* *********************************************************************************** *)
   (* pickling and unpickling paths
      These operations are now rather trivial.  Are they still needed?
        QUESTIONS:
          What do we really need to pickle/unpickle?
	  Do we need to pickle/unpickle relative to a file as the original version did?
      SrcPath.pickle is called twice in stable/stabilize.sml. *)

    exception Format
     (* This is raised only in the exported function unpickle.
        We could instead use the existing UnpickleUtil.Format exception, as is done in other
	source files like stable/stabilize.sml and smlfile/skel-io.sml. This would require importing
	"$pickle-lib.cm" in srcpath-lib.cm, as is done in cm-lib.cm.
	There is one specific "handle SrcPath.Format", occuring in the file stable/stabilize.sml
        where the handler just raises UnpickleUtil.Format in place of this Format exception. *)

    (* picklePath : path -> string list *)
    fun picklePath ((head, arcs): path) = 
	(case head
	   of (ABS U) => "%u" :: arcs
	    | (ABS (V vol)) => "%w" :: arcs
	    | (REL n) => "%r" :: Int.toString n :: arcs
	    | (ANC a) => "%a" :: a :: arcs)

    (* unpicklePath : string list -> path;
     *   raises Format *)
    fun unpicklePath ("%u" :: arcs) = (ABS U, arcs)
      | unpicklePath ("%w" :: vol :: arcs) = (ABS (V vol), arcs)
      | unpicklePath ("%r" :: nparents :: arcs) =
	  (case Int.fromString nparents
	     of SOME n => (REL n, arcs)
	      | NONE => raise Format)
      | unpicklePath ("%a" :: anchor :: arcs) = (ANC anchor, arcs)
      | unpicklePath _ = raise Format


   (* *********************************************************************************** *)
   (* Path parsing I:  
    *   parsing and unparsing fpaths/paths: fpath <--> path
    *   This version does not anchor expansion and so is independent of anchor environments. *)

    (* parseAnchor : string -> string option *)
    fun parseAnchor (arc : string) =
          (case Char.compare (String.sub (arc, 0), #"$")
	     of EQUAL => SOME (String.extract (arc, 1, NONE))
	      | _=> NONE)

    (* countParentLinks : string list -> int * string list *)
    fun countParentLinks (".." :: rest) =
	  let val (n, rest') = countParentLinks rest
	   in (n+1, rest')
	  end
      | countParentLinks arcs = (0, arcs)

    (* parseFpath : fpath -> path *)
    (* OS.Path.fromString provides the initial fpath parse, which we convert to a path.
     * Thus we delegate the primary "parsing" of the file path string to OS.Path.fromString.
     * Can we assume that this is the "standard" parse?
     * Can we assume that fpath is "canonical"? Should we call F.canonicalize to make sure?
     * We will assume that isAbs = true, vol = "" implies the Unix root.
     * This translation translates (expands) the "$/a" anchor abbreviation. *)
    fun parseFpath (fpath: fpath) =
	let val { arcs, vol, isAbs } = P.fromString fpath
	 in if isAbs
	    then (ABS (if vol = "" then U else V vol), rev arcs)
	    else (case arcs
		    of nil => (REL 0, nil)
		     | "$" :: rest => 
			  (case rest
			     of nil => impossible ["parseFpath: bad initial $ arc"]
			      | arc0 :: rest' => (ANC arc0, rest))
		     | arc0::rest =>
		         (case (parseAnchor arc0)
			    of SOME name => (ANC name, rev rest)
			     | NONE =>
 				 let val (nparents, arcs') = countParentLinks arcs
				  in (REL nparents, rev arcs')
				 end))
	end

    (* interleave : string * string list -> string list *)
    fun interleave (s, nil) = nil
      | interleave (s, [x]) = [x]
      | interleave (s, y::ys) = y :: s :: interleave (s, ys)

    (* arcsToString :  string list -> string *)
    fun arcsToString arcs = concat (interleave ("/", (rev arcs)))

    (* addParentLinks : int * string list -> string list *)
    fun addParentLinks (0, ss) = ss
      | addParentLinks (n, ss) = ".." :: "/" :: addParentLinks (n-1, ss)

    (* pathToFpath : path -> string *)
    (* unparsing path to string (file path in standard* format *)
    fun pathToFpath ((head, arcs): path) =
	(case head
	   of ABS U => "/" ^ arcsToString arcs
	    | ABS (V vol) => concat ["%", vol, arcsToString arcs]  (* right Windows FS syntax? *)
	    | REL parentlinks => arcsToString (addParentLinks (parentlinks, arcs))
	    | ANC name => concat ["$" ^ name, "/", arcsToString arcs])

end (* structure Path *)

