(* cm/paths/dbm-path.sml *)

(* Path structure: abstract syntax of file paths used in CDFs *)

    (* QUESTION: Are paths used to designate only regular files, e.g. CDFs, or do they sometimes denote
     * directories in the file system?  This relates to the question of whether empty arc lists are
     * allowed.  For instance, the Unix FS root directory would be denoted by (ABS U, nil).
     * Another question is when and whether anchor path heads (ANC anchor-name) directly designate
     * CDFs, in which case we will normally need to discover the parent directory of the CDF so
     * designated. The question also relates to that of whether a file is always a non-directory
     * file, and whether the path of a file can have an empty arc list. One assumes that if files
     * are always non-directory (e.g. CDFs), then their paths will always have at least one arc.*)

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
     * However, it is unclear whether this has significant efficiency advantages over the normal
     * top to bottom, or outer to inner, ordering, since reversing the arc list is required for
     * some functions.  So this arc order convention might change. *)
    type path = head * arcs


   (* *********************************************************************************** *)
   (* path functions *)

    (* path construction *)
    fun rootedU (arcs: string list) : path = (ABS U, arcs)
    fun rootedV (vol: string, arcs: string list) : path = (ABS (V vol), arcs)
    fun relPath (parentarcs: int, arcs: string list) : path = (REL parentarcs, arcs)
    fun anchorPath (anchor: string, arcs: string list) : path = (ANC anchor, arcs)

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

    (* mkAbsolute : path -> path -> path *)
    (* if path1 is absolute and path2 is relative, the paths are "concatenated",
     *   otherwise, path2 is returned unchanged.
     * ASSERT: path1 is absolute (typically/always? it will be CWD) *)
    fun mkAbsolute ((head1 as (ABS _), arcs1): path) (path2: path) =
  	  (case path2
	     of (REL n, arcs2) => (head1, arcs2 @ List.drop (arcs1, n))
		  (* what if n = length arcs? what if n > length arcs? Error? *)
	      | _ => path2)
      | mkAbsolute _ _ = impossible ["mkAbsolute: path1 is not absolute"]

    (* extendArcs : arcs -> path -> path
     * rarcs in reverse order, outer to inner, like the arcs in the path
     * not exported
     * local uses: native, standard *)
    fun extendArcs (rarcs: arc list) ((head, arcs): path) : path  =
	(head, (rarcs @ arcs))

    (* substAnchor : [anchorPath:]path * [anchoredPath:]path -> path *)
    (* add anchoredPath's arcs onto anchorPath, effectively replacing anchoredPath's
     * anchor head with anchorPath *)
    fun substAnchor ((head, arcs) : path, (ANC _, arcs') : path) =
	  (head, arcs' @ arcs)
      | substAnchor (_, path2) => path2

    (* reanchorPath : (anchor -> path) -> path -> path *)
    (* used once in main/filename-policy.sml (FilenamePolicyFn)? *)
    fun reanchorPath (cvt: anchor -> path) (path as (head, arcs) : path) =
	  (case head
	     of ANC anchor => substAnchor (cvt anchor, path)
	      | _ => path)  (* path is not anchored, return it unchanged *)


   (* *********************************************************************************** *)
   (* pickling and unpickling paths
      These operations are now rather trivial. Are they still needed?
        QUESTIONS:
          What do we really need to pickle/unpickle?
	  Do we need to pickle/unpickle relative to a file as the original version did?
      SrcPath.pickle is called twice in stable/stabilize.sml.

      (1) unpickle (pickle path) == path
      (2) pickle (unpickle ss) = ss if ss = pickle path for some path

   *)

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
    *   This version does not do anchor expansion/substitution and so is independent of
    *   anchor environments. *)

    (* parseAnchor : string -> string option *)
    (* returns the "name" of an anchor arc, i.e. "$abc" => "abc" *)
    fun parseAnchor (arc : string) =
          (case Char.compare (String.sub (arc, 0), #"$")
	     of EQUAL => SOME (String.extract (arc, 1, NONE))
	      | _=> NONE)

    (* countParentLinks : string list -> int * string list *)
    (* strips off leading parent arcs (".."), returning the cound of parent arcs and the remaining arcs *)
    fun countParentLinks (".." :: rest) =
	  let val (n, rest') = countParentLinks rest
	   in (n+1, rest')
	  end
      | countParentLinks arcs = (0, arcs)

    (* parseFpath : fpath -> path *)
    (* OS.Path.fromString provides the initial file path parse, which we convert to a path.
     * Thus we delegate the primary "parsing" of the file path string to OS.Path.fromString.
     * Can we assume that this is the "standard" parse? Or is it the "native" parse?
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


    (* ******************************************************************************** *)
    (* Path Parsing II: Native and Standard Parsing (using pathEnv)
     * Additional, possibly redundant versions for "standard" and "native" file path formats.
     *
     * The "native" file path parser is to be used on group/library member path strings that
     * are quoted. Otherwise, the "standard" file path parser is used.

     * Need precise specifications of the "standard" and "native" file paths.
     * Given parseFpath, do we really need these additional versions?
     *
     * We are not having parseNative and parseStandard expand an anchor head, so these functions
     * do not need to access an anchor environment. *)

    (* parseNative : fpath -> path *)
    (* parse a quoted fpath in "native" format -- we delegate to parseFpath, which calls
     * OS.Path.fromString to do the original parsing (we assume it handles native format).
     * This may work ok when the native OS is Unix. Not sure about Windows.
     * Should we write complete "native" file path parsers for Unix and Windows? *)
    val parseNative = parseFpath

    (* parseStandard : fpath -> path *)
    (* accepts both "/" and "\" arc delimiters -- even mixed!
     * This duplicates some of the file path parsing that is done by OS.Path.fromString *)
    fun parseStandard0 (fpath: fpath) =
	let fun delim #"/" = true
	      | delim #"\\" = true
	      | delim _ = false
	 in case (String.fields delim fpath)
	      of nil => impossible ["parseStandard -- no fields"]
	       | [""] => impossible ["parseStandard -- zero-length arc ", fpath]
	       | arcs as (["$"] | "$" :: "" :: _) =>  (* reject "$", "$/" "$//..." *)
		   impossible ["parseStandard -- invalid zero-length anchor name in: ", fpath]
	       | "" :: arcs => (ABS U, rev arcs)
	       | "$" :: (arcs as (arc1 :: _)) => (ANC arc1, rev arcs)
	       | arcs as (arc1 :: rest) =>
		   if String.sub (arc1, 0) <> #"$" then (REL 0, rev arcs)
		   else (ANC (String.extract (arc1, 1, NONE)), rest)
	end (* fun parseStandard *)


    (* *********************************************************************************** *)
    (* Path Parsing III: Parsing Segmented File Paths (using pathEnv) *)
    (* Conjecture: used in processing PIDMAP file. *)

    (* parseSegmented : fpath -> path
     * The fpath argument is a "segmented fpath" (sequence of fpaths separated by ":").
     * The pathEnv argument is a "local" overlay over the global PathEnv anchor environment.
     * What are "segments"?
     * Where are they documented?
     * Where are they introduced?
         One place where "segmented" paths occur is in the PIDMAP file. Any others?
     * What is the purpose of segments?
         Chaining where one CDF serves as a proxy for another local CDF?
	 Known example is "$SMLNJ-LIB/PrettyPrint/prettyprint-lib.cm:src/prettyprint.cm".
     * Why do we need to apply transCode to the arcs? (Who would have put escape codes in them?)
     * Do segmented file paths need to be represented explicitly in the path type?
         Like in the old dir datatype? Currently segmented paths are collapsed to a path.
     * We are not asking parseSegmented to expand an anchor head in the first segment path. *)

    (* parseSegmented [decode] : fpath -> path *)
    (* Called (externally) in 3 places:
	main/cm-boot.sml -- readpidmap
	bootstrap/btcompile.sml  -- parsing "root"  argument of mk_compile
	main/slave.sml -- not obvious what it is applied to? *)
    fun parseSegmented (fpath: fpath) : path =
	let (* firstseg : string -> path *)
	    fun firstseg (seg : string) =
		(case map transCode (String.fields (isChar #"/") seg)
		   of nil => impossible ["parseFpath: no fields in segment 0"]
		    | (arcs as (arc0 :: arcs0)) =>
		      if arc0 = ""  (* fpath starts with #"/" *)
		      then (ABS U, rev arcs0)
		      else let val char0 = String.sub (arc0, 0) (* 1st char of arc0 *)
			       val name = String.extract (arc0, 1, NONE)
			    in case char0
				 of #"%" => (* name is a volume name, not checking for empty name *)
				      (ABS (W name), rev arcs0)
				  | #"$" => (* name is an anchor, not checking for empty name *)
				      (ARC name, rev arcs0)
	                          | #"." =>  (* not checking that arc0 = ".." *)
				      let val (n, arcs') = countParentLinks (arcs)
				       in (REL n, rev arcs')
				      end
	                          | _ => (REL 0, rev arcs)
	                   end)

	    (* segToPath: string -> path *)
            fun segToPath (seg: string) =
		(REL 0, rev (map transCode (String.fields (isChar #"/") seg)))

            (* flattenPaths : path list -> path *)
	    (* flattens a list of segment paths into a single path, with same head *)
	    fun flattenPaths nil = impossible ["flattenPaths"]
	      | flattenPaths [path] = path
	      | flattenPaths (paths as (head, arcs) :: rest) =
		  let val rarcss = map #2 paths
		      fun combine [rarcs] = rarcs
			| combine (rarcs::rest) = revAppend (tl rarcs, combine rest)
			| combine nil = impossible ["flattenPaths:combine"]
		   in (head, combine rarcss)
		  end

	 in case String.fields (isChar #":") fpath (* parse into segments *)
	      of nil => impossible ["parseSegmented: no segments"]
	       | seg :: segs => flattenPaths (firstseg seg :: map segToPath segs)

	end (* fun parseFpath *)

end (* structure Path *)
