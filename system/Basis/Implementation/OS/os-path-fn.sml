(* os-path-fn.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *
 * A functorized implementation of the OS.Path structure.
 *
 * NOTE: these operations are currently not very efficient, since they
 * explode the path into its volume and arcs.  A better implementation
 * would work "in situ."
 *
 *)

local
    structure String = StringImp
in
functor OS_PathFn (OSPathBase : sig

    exception Path

    datatype arc_kind = Null | Parent | Current | Arc of string
    val classify : string -> arc_kind
    val parentArc : string
    val currentArc : string
    val validArc : string -> bool
    val validVolume : (bool * Substring.substring) -> bool
    val splitVolPath : string -> (bool * Substring.substring * Substring.substring)
	(* Split a string into the volume part and arcs part and note whether it
	 * is absolute.
	 * Note: it is guaranteed that this is never called with "".
	 *)
    val joinVolPath : (bool * string * string) -> string
	(* join a volume and path; raise Path on invalid volumes *)
    val arcSepChar : char
	(* the character used to separate arcs (e.g., #"/" on UNIX) *)
    val sameVol : string * string -> bool

  end) : OS_PATH = struct

    structure P = OSPathBase
    structure SS = Substring

    exception Path = P.Path
    exception InvalidArc

  (* check an arc to see if it is valid and raise InvalidArc if not *)
    fun checkArc arc = if P.validArc arc then arc else raise InvalidArc

    val arcSepStr = String.str P.arcSepChar

    val parentArc = P.parentArc
    val currentArc = P.currentArc

  (* concatArcs is like List.@, except that a trailing empty arc in the
   * first argument is dropped.
   *)
    fun concatArcs ([], al2) = al2
      | concatArcs ([""], al2) = al2
      | concatArcs (a::al1, al2) = a :: concatArcs(al1, al2)

    fun validVolume {isAbs, vol} = P.validVolume(isAbs, SS.full vol)

    fun fromString "" = {isAbs = false, vol = "", arcs = []}
      | fromString p = let
	  val fields = SS.fields (fn c => (c = P.arcSepChar))
	  val (isAbs, vol, rest) = P.splitVolPath p
	  in
	    { isAbs = isAbs,
	      vol = SS.string vol,
	      arcs = List.map SS.string (fields rest)
	    }
	  end

    fun toString {isAbs=false, vol, arcs="" :: _} = raise Path
      | toString {isAbs, vol, arcs} = let
	  fun f [] = [""]
	    | f [a] = [checkArc a]
	    | f (a :: al) = (checkArc a) :: arcSepStr :: (f al)
	  in
	    if validVolume{isAbs=isAbs, vol=vol}
	      then String.concat(P.joinVolPath(isAbs, vol, "") :: f arcs)
	      else raise Path
	  end

    fun getVolume p = #vol(fromString p)
    fun getParent p = let
	  fun getParent' [] = [parentArc]
	    | getParent' [a] = (case (P.classify a)
		 of P.Current => [parentArc]
		  | P.Parent => [parentArc, parentArc]
		  | P.Null => [parentArc]
		  | _ => []
		(* end case *))
	    | getParent' (a :: al) = a :: getParent' al
	  in
	    case (fromString p)
	     of {isAbs=true, vol, arcs=[""]} => p
	      | {isAbs=true, vol, arcs} =>
		  toString{isAbs = true, vol = vol, arcs = getParent' arcs}
	      | {isAbs=false, vol, arcs} => (case (getParent' arcs)
		   of [] => toString{isAbs=false, vol=vol, arcs=[currentArc]}
		    | al' => toString{isAbs=false, vol=vol, arcs=al'}
		  (* end case *))
	    (* end case *)
	  end

    fun splitDirFile p = let
	  val {isAbs, vol, arcs} = fromString p
	  fun split [] = ([], "")
	    | split [f] = ([], f)
	    | split (a :: al) = let val (d, f) = split al
		in
		  (a :: d, f)
		end
	  fun split' p = let val (d, f) = split p
		in
		  {dir=toString{isAbs=isAbs, vol=vol, arcs=d}, file=f}
		end
	  in
	    split' arcs
	  end
    fun joinDirFile {dir="", file} = checkArc file
      | joinDirFile {dir, file} = let
	  val {isAbs, vol, arcs} = fromString dir
	  in
	    toString {isAbs=isAbs, vol=vol, arcs = concatArcs(arcs, [checkArc file])}
	  end
    fun dir p = #dir(splitDirFile p)
    fun file p = #file(splitDirFile p)
    
    fun splitBaseExt p = let
	  val {dir, file} = splitDirFile p
	  val (file', ext') = SS.splitr (fn c => c <> #".") (SS.full file)
	  val fileLen = SS.size file'
	  val (file, ext) =
		if (fileLen <= 1) orelse (SS.isEmpty ext')
		  then (file, NONE)
		  else (SS.string(SS.trimr 1 file'), SOME(SS.string ext'))
	  in
	    {base = joinDirFile{dir=dir, file=file}, ext = ext}
	  end
    fun joinBaseExt {base, ext=NONE} = base
      | joinBaseExt {base, ext=SOME ""} = base
      | joinBaseExt {base, ext=SOME ext} = let
	  val {dir, file} = splitDirFile base
	  in
	    joinDirFile{dir=dir, file=String.concat[file, ".", ext]}
	  end
    fun base p = #base(splitBaseExt p)
    fun ext p = #ext(splitBaseExt p)

    fun mkCanonical "" = currentArc
      | mkCanonical p = let
	  fun scanArcs ([], []) = [P.Current]
	    | scanArcs (l, []) = List.rev l
	    | scanArcs ([], [""]) = [P.Null]
	    | scanArcs (l, a::al) = (case (P.classify a)
		 of P.Null => scanArcs(l, al)
		  | P.Current => scanArcs(l, al)
		  | P.Parent => (case l
		      of (P.Arc _ :: r) => scanArcs(r, al)
		       | _ => scanArcs(P.Parent::l, al)
		     (* end case *))
		  | a' => scanArcs(a' :: l, al) 
		(* end case *))
	  fun scanPath relPath = scanArcs([], relPath)
	  fun mkArc (P.Arc a) = a
	    | mkArc (P.Parent) = parentArc
	    | mkArc _ = raise Fail "mkCanonical: impossible"
	  fun filterArcs (true, P.Parent::r) = filterArcs (true, r)
	    | filterArcs (true, []) = [""]
	    | filterArcs (true, [P.Null]) = [""]
	    | filterArcs (true, [P.Current]) = [""]
	    | filterArcs (false, [P.Current]) = [currentArc]
	    | filterArcs (_, al) = List.map mkArc al
	  val {isAbs, vol, arcs} = fromString p
	  in
	    toString{
		isAbs=isAbs, vol=vol, arcs=filterArcs(isAbs, scanPath arcs)
	      }
	  end

    fun isCanonical p = (p = mkCanonical p)

    fun isAbsolute p = #isAbs(fromString p)
    fun isRelative p = Bool.not(#isAbs(fromString p))

    fun mkAbsolute {path, relativeTo} = (
	  case (fromString path, fromString relativeTo)
	   of (_, {isAbs=false, ...}) => raise Path
	    | ({isAbs=true, ...}, _) => path
	    | ({vol=v1, arcs=al1, ...}, {vol=v2, arcs=al2, ...}) => let
		fun mkCanon vol = mkCanonical(toString{
			isAbs=true, vol=vol, arcs=List.@(al2, al1)
		     })
		in
		  if P.sameVol (v1, v2) then mkCanon v1
		  else if (v1 = "") then mkCanon v2
		  else if (v2 = "") then mkCanon v1
		    else raise Path
		end
	  (* end case *))
    fun mkRelative {path, relativeTo} =
	  if (isAbsolute relativeTo)
	    then if (isRelative path)
	      then path
	      else let
		val {vol=v1, arcs=al1, ...} = fromString path
		val {vol=v2, arcs=al2, ...} = fromString(mkCanonical relativeTo)
		fun strip (l, []) = mkArcs l
		  | strip ([], l) = dotDot([], l)
                  | strip (l1 as (x1::r1), l2 as (x2::r2)) = if (x1 = x2)
                      then strip (r1, r2)
                      else dotDot (l1, l2)
                and dotDot (al, []) = al
                  | dotDot (al, _::r) = dotDot(parentArc :: al, r)
		and mkArcs [] = [currentArc]
		  | mkArcs al = al
		in
		  if not (P.sameVol (v1, v2))
		    then raise Path
		    else (case (al1, al2)
		       of ([""], [""]) => currentArc
			| ([""], _) =>
			    toString{isAbs=false, vol="", arcs=dotDot([], al2)}
			| _ =>
			    toString{isAbs=false, vol="", arcs=strip(al1, al2)}
		      (* end case *))
		end
	    else raise Path

    fun isRoot path = (case (fromString path)
	   of {isAbs=true, arcs=[""], ...} => true
	    | _ => false
	  (* end case *))

    fun concat (p1, p2) = (case (fromString p1, fromString p2)
	   of (_, {isAbs=true, ...}) => raise Path
	    | ({isAbs, vol=v1, arcs=al1}, {vol=v2, arcs=al2, ...}) =>
		if P.sameVol (v2, "") orelse P.sameVol (v1, v2)
		  then toString{isAbs=isAbs, vol=v1, arcs=concatArcs(al1, al2)}
		  else raise Path
	  (* end case *))

    local
      fun fromUnixPath' up = let
	    fun tr "." = P.currentArc
	      | tr ".." = P.parentArc
	      | tr arc = checkArc arc
	    in
	      case String.fields (fn c => c = #"/") up
	       of "" :: arcs => { isAbs = true, vol = "", arcs = map tr arcs }
		| arcs => { isAbs = false, vol = "", arcs = map tr arcs }
	    end
      fun toUnixPath' { isAbs, vol = "", arcs } = let
	    fun tr arc =
		  if arc = P.currentArc then "."
		  else if arc = P.parentArc then ".."
		  else if CharVector.exists (fn #"/" => true | _ => false) arc then raise InvalidArc
		  else arc
	    in
	      String.concatWith "/" (if isAbs then "" :: arcs else arcs)
	    end
	| toUnixPath' _ = raise Path
    in
    val fromUnixPath = toString o fromUnixPath'
    val toUnixPath = toUnixPath' o fromString
    end (* local *)

  end
end

