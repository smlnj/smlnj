(* Copyright 1996 by AT&T Bell Laboratories *)
(* sympaths.sml *)

structure SymPath : SYMPATH =
struct

  structure S = Symbol

  datatype path = SPATH of S.symbol list

  exception SymPath

  val empty = SPATH nil

  fun null(SPATH p) = List.null p

  fun extend(SPATH p: path, s: S.symbol) = SPATH(p @ [s])

  fun prepend(s: S.symbol, SPATH p: path) = SPATH(s::p)

  fun append(SPATH front: path, SPATH back: path) = SPATH(front @ back)

  fun first(SPATH []: path) = raise SymPath
    | first(SPATH(s::_)) = s

  fun rest(SPATH []: path) = raise SymPath
    | rest(SPATH(_::p)) = SPATH p

  fun length(SPATH p: path) = List.length p

  (* the last element of a path *)
  fun last(SPATH p) =
      List.last p
	handle List.Empty => ErrorMsg.impossible "SymPath.last"

  fun equal(SPATH p1: path, SPATH p2: path) = ListPair.all Symbol.eq (p1, p2)

  fun toString(SPATH p: path) =
      let fun f [s] = [Symbol.name s]
	    | f (a::r) = Symbol.name a :: "." :: f r
(* [DBM, 2020.04.25] misplaced hack for curried/noncurried functor absyn expansions
		  if (Symbol.eq(a,resultId)) orelse
		     (Symbol.eq(a,returnId))
		  then f r
		  else Symbol.name a :: "." :: f r
*)
	    | f nil = ["<empty spath>"]
       in concat(f p)
      end

end (* structure SymPath *)


structure InvPath : INVPATH =
struct

  structure S = Symbol

  datatype path = IPATH of S.symbol list

  exception InvPath

  val empty = IPATH nil

  fun null(IPATH p) = List.null p

  fun extend(IPATH p: path, s: S.symbol) = IPATH(s::p)

  fun append(IPATH front: path, IPATH back: path) = IPATH(back @ front)

  fun last(IPATH []: path) = raise InvPath
    | last(IPATH(s::_)) = s

  fun lastPrefix(IPATH []: path) = raise InvPath
    | lastPrefix(IPATH(_::p)) = IPATH p

  fun equal(IPATH p1:path, IPATH p2:path) = ListPair.all Symbol.eq (p1, p2)

  fun toString(IPATH p: path) =
     let fun f [s] = [Symbol.name s, ">"]
	   | f (a::r) = Symbol.name a :: "." :: f r
	   | f nil = [">"]
      in concat("<" :: f p)
     end

end (* structure InvPath *)


signature CONVERTPATHS =
sig
  val invertSPath : SymPath.path -> InvPath.path
  val invertIPath : InvPath.path -> SymPath.path

  val stripPath : InvPath.path -> SymPath.path

  (* needed in PPTypes, PPModules to filter out administrative structure names,
   *  i.e. "resultStr", "returnStr" *)
  val findPath : InvPath.path * ('a -> bool) * (SymPath.path -> 'a option)
                 -> Symbol.symbol list * bool
end

structure ConvertPaths : CONVERTPATHS =
struct

  structure S = Symbol
  structure SP = SymPath
  structure IP = InvPath

  val debugging = ElabDataControl.tpdebugging

  fun saynl msg = (Control_Print.say msg; Control_Print.say "\n"; Control_Print.flush ())
  fun saysnl msgs = saynl (concat msgs)
  fun dbsaynl msg = if !debugging then saynl msg else ()
  fun dbsaysnl msgs = if !debugging then saysnl msgs else ()

  fun invertSPath(SP.SPATH p : SP.path) : IP.path =
      IP.IPATH(rev p)
  fun invertIPath(IP.IPATH p : IP.path) : SP.path =
      SP.SPATH(rev p)

  (* findPath: IP.path * ('a -> bool) * (SP.path -> 'a option)
               -> Symbol.symbol list * bool
    convert inverse symbolic path names to a printable string in the
    context of an environment.

    'a is instantiated to T.tycon in PPType and M.structure in PPModule.

    Its arguments are the inverse symbolic path, a check predicate on static
    semantic values, and a lookup function mapping paths to their bindings
    (if any) in an environment and raising Env.Unbound on paths with no
    binding.

    It looks up each suffix of the path name, going from shortest to longest
    suffix, in the current environment until it finds one whose lookup value
    satisfies the check predicate.  It then converts that suffix to a string.
    If it doesn't find any suffix, the full path (reversed, i.e. in the
    normal order) and the boolean value false are returned, otherwise the
    suffix and true are returned.

    Example:
	   Given A.B.t as a path, and a lookup function for an
	   environment, this function tries:
		     t
		     B.t
		     A.B.t
	   If none of these work, it returns ?.A.B.t

    Note: the symbolic path is passed in reverse order because that is
    how all symbolic paths are stored within static semantic objects.
   *)

  fun stripPath (IP.IPATH p) : SP.path =
      let fun good (sym: S.symbol) =
	      let val name = S.name sym
	       in not (name = "<resultStr>") andalso not (name = "<returnStr>")
	      end
       in SP.SPATH (rev (List.filter good p))
      end

  fun findPath (IP.IPATH p: IP.path, check, look): (S.symbol list * bool) =
      let val _ = dbsaysnl ["### findPath: ", IP.toString (IP.IPATH p)]
	  fun try (nextSym::untried, tried) =
	        let val nextName = S.name nextSym
		 in if nextName = "<resultStr>" orelse nextName = "<returnStr>"
		    then (dbsaysnl ["### dropping special path element ", nextName];
			  try (untried, tried)) (* drop special sym *)
		    else let val nextPath = nextSym :: tried
			     val elemOp = look (SymPath.SPATH nextPath)
			 in case elemOp
			     of NONE => try(untried, nextPath)
			      | SOME elem =>
				if check elem
				then (nextPath, true)
				else try(untried, nextPath)
			 end
		end
	    | try([],tried) = (tried, false)  (* path not bound -- by look *)
       in try(p,[])
      end

end (* structure ConvertPaths *)
