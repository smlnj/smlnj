(* string.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure StringImp : STRING =
  struct

  (* fast add/subtract avoiding the overflow test *)
    infix -- ++
    fun x -- y = InlineT.Int.fast_sub(x, y)
    fun x ++ y = InlineT.Int.fast_add(x, y)

    val op < = InlineT.Int.<
    val op <= = InlineT.Int.<=
    val op > = InlineT.Int.>
    val op >= = InlineT.Int.>=
(*    val op = = InlineT.= *)
    val unsafeSub = InlineT.CharVector.sub
    val unsafeUpdate = InlineT.CharVector.update
  (* list reverse *)
    fun listRev ([], l) = l
      | listRev (x::r, l) = listRev (r, x::l)

    type char = char
    type string = string

    val maxSize = Core.max_length

  (* these functions are implemented in base/system/smlnj/init/pervasive.sml *)
    val size = size
    val op ^ = op ^
    val concat = concat
    val implode = implode
    val explode = explode
    val substring = substring

    val unsafeCreate = Assembly.A.create_s

  (* allocate an uninitialized string of given length *)
    fun create n = if (InlineT.Int.ltu(maxSize, n))
	  then raise General.Size
	  else Assembly.A.create_s n

  (* added for Basis Library proposal 2015-003 *)
    fun implodeRev [] = ""
      | implodeRev l = let
	  fun length l = let
		fun loop (n, []) = n
		  | loop (n, [_]) = n ++ 1
		  | loop (n, _ :: _ :: l) = loop (n ++ 2, l)
		in
		  loop (0, l)
		end
	  val n = length l
	  val s = create n
	  fun fill ([], _) = s
	    | fill (c::cs, i) = (
		unsafeUpdate(s, i, c);
		fill (cs, i -- 1))
	  in
	    fill (l, n -- 1)
	  end
  (* end Basis Library proposal 2015-003 *)

  (* convert a character into a single character string *)
    fun str (c : Char.char) : string =
	  InlineT.PolyVector.sub(PreString.chars, InlineT.cast c)

  (* get a character from a string *)
    val sub : (string * int) -> char = InlineT.CharVector.chkSub

    fun extract (v, base, optLen) = let
	  val len = size v
	  fun newVec n = let
		val newV = Assembly.A.create_s n
		fun fill i = if (i < n)
		      then (unsafeUpdate(newV, i, unsafeSub(v, base ++ i)); fill(i ++ 1))
		      else ()
		in
		  fill 0; newV
		end
	  in
	    case (base, optLen)
	     of (0, NONE) => v
	      | (_, SOME 0) => if ((base < 0) orelse (len < base))
		    then raise General.Subscript
		    else ""
	      | (_, NONE) => if ((base < 0) orelse (len < base))
		      then raise General.Subscript
		    else if (base = len)
		      then ""
		      else newVec (len - base)
	      | (_, SOME 1) =>
		  if ((base < 0) orelse (len < (base ++ 1)))
		    then raise General.Subscript
		    else str(unsafeSub(v, base))
	      | (_, SOME n) =>
		  if ((base < 0) orelse (n < 0) orelse (len < (base ++ n)))
		    then raise General.Subscript
		    else newVec n
	    (* end case *)
	  end

  (* concatenate a list of strings, using the given separator string *)
    fun concatWith _ [] = ""
      | concatWith _ [x] = x
      | concatWith sep (h :: t) =
	  concat (listRev (foldl (fn (x, l) => x :: sep :: l) [h] t, []))

    fun map f vec = (case (size vec)
	   of 0 => ""
	    | len => let
		val newVec = Assembly.A.create_s len
		fun mapf i = if (i < len)
		      then (unsafeUpdate(newVec, i, f(unsafeSub(vec, i))); mapf(i+1))
		      else ()
		in
		  mapf 0; newVec
		end
	  (* end case *))

  (* map a translation function across the characters of a string *)
    fun translate tr s = PreString.translate (tr, s, 0, size s)

  (* tokenize a string using the given predicate to define the delimiter
   * characters.
   *)
    fun tokens isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = if (i = j)
		then l
		else PreString.unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, substr(i, j, toks))
		    else scanTok (i, j ++ 1, toks)
		  else substr(i, j, toks)
	  and skipSep (j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then skipSep(j ++ 1, toks)
		    else scanTok(j, j ++ 1, toks)
		  else toks
	  in
	    listRev (scanTok (0, 0, []), [])
	  end
    fun fields isDelim s = let
	  val n = size s
	  fun substr (i, j, l) = PreString.unsafeSubstring(s, i, j -- i)::l
	  fun scanTok (i, j, toks) = if (j < n)
		  then if (isDelim (unsafeSub (s, j)))
		    then scanTok (j+1, j+1, substr(i, j, toks))
		    else scanTok (i, j+1, toks)
		  else substr(i, j, toks)
	  in
	    listRev (scanTok (0, 0, []), [])
	  end

  (* String comparisons *)
    fun isPrefix s1 s2 = PreString.isPrefix (s1, s2, 0, size s2)
    fun isSuffix s1 s2 =
	let val sz2 = size s2
	in
	    PreString.isPrefix (s1, s2, sz2 - size s1, sz2)
	end
    fun isSubstring s = let
	val stringsearch = PreString.kmp s
	fun search s' = let
	    val epos = size s'
	in
	    stringsearch (s', 0, epos) < epos
	end
    in
	search
    end

    fun compare (a, b) =
	PreString.cmp (a, 0, size a, b, 0, size b)
    fun collate cmpFn (a, b) =
	PreString.collate cmpFn (a, 0, size a, b, 0, size b)

  (* String greater or equal *)
    fun sgtr (a, b) = let
	  val al = size a and bl = size b
	  val n = if (al < bl) then al else bl
	  fun cmp i = if (i = n)
		then (al > bl)
		else let
		  val ai = unsafeSub(a,i)
		  val bi = unsafeSub(b,i)
		  in
		    Char.>(ai, bi) orelse ((ai = bi) andalso cmp(i+1))
		  end
	  in
	    cmp 0
	  end

    fun scan getc = let
	  val cscan = Char.scan getc
	  fun illegal (strm, chrs) = (case chrs
		 of [] => NONE (* string starts with illegal escape or non-printing char *)
		  | _ => SOME(implodeRev chrs, strm)
		(* end case *))
	  fun scan' (strm, chrs) = (case getc strm
		 of NONE => SOME(implodeRev chrs, strm)
		  | SOME(#"\\", strm') => (case getc strm'
		       of SOME(c, strm'') =>
			    if Char.isSpace c
			      then let (* skip over the formatting escape *)
				fun skip strm' = (case getc strm'
				       of SOME(#"\\", strm') => scan'(strm', chrs)
					| SOME(c, strm') =>
					    if Char.isSpace c
					      then skip strm'
					      else illegal (strm, chrs)
					| NONE => illegal (strm, chrs)
				      (* end case *))
				in
				  skip strm''
				end
			    (* otherwise use Char.scan to scan the escape character *)
			      else (case cscan strm
				 of SOME(c, strm') => scan' (strm', c::chrs)
				  | NONE => illegal (strm, chrs)
				(* end case *))
			| NONE => illegal (strm, chrs)
		      (* end case *))
		  | SOME(c, strm') => if Char.isPrint c
		      then scan' (strm', c::chrs)
		      else illegal (strm, chrs)
		(* end case *))
	  in
	    fn strm => scan' (strm, [])
	  end

    val fromString = StringCvt.scanString scan
    val toString = translate Char.toString

    fun fromCString s = let
	  val len = size s
	  fun getc i = if InlineT.Int.<(i, len)
		then SOME(unsafeSub(s, i), i+1)
		else NONE
	  val scanChar = Char.scanC getc
	  fun accum (i, chars) = (case (scanChar i)
		 of NONE => if InlineT.Int.<(i, len)
		      then NONE (* bad format *)
		      else SOME(implodeRev chars)
		  | (SOME(c, i')) => accum(i', c::chars)
		(* end case *))
	  in
	    accum (0, [])
	  end

    val toCString = translate Char.toCString

  (* added for Basis Library proposal 2015-003 *)
    fun rev s = let
	  val n = size s
	  in
	    if (n < 2)
	      then s
	      else let
		val s' = unsafeCreate n
		fun fill i = if (i < n)
		      then (unsafeUpdate(s', i, unsafeSub(s, n--i--1)); fill(i++1))
		      else ()
		in
		  fill 0; s'
		end
	  end

    fun concatWithMap sep cvtFn = let
	  fun concat' [] = ""
	    | concat' [x] = cvtFn x
	    | concat' (x::xs) = let
		val sepLen = size sep
		fun cvt ([], strs, len) = let
		      val s' = unsafeCreate len
		      fun fill ([], _) = s'
			| fill (s::ss, i) = let
			    val n = size s
			    val i = i -- n
			    fun copy j = if j < n
				  then (unsafeUpdate(s', i++j, unsafeSub(s, j)); copy(j++1))
				  else fill (ss, i)
			    in
			      copy 0
			    end
		      in
			fill (strs, len)
		      end
		  | cvt (x::xs, strs, len) = let
		      val s = cvtFn x
		      val len = len ++ sepLen ++ size s
		      in
			if len > maxSize then raise General.Size else ();
			cvt (xs, s::sep::strs, len)
		      end
		val s = cvtFn x
		in
		  cvt (xs, [s], size s)
		end
	  in
	    concat'
	  end
  (* end Basis Library proposal 2015-003 *)

    fun op <= (a,b) = if sgtr(a,b) then false else true
    fun op < (a,b) = sgtr(b,a)
    fun op >= (a,b) = b <= a
    val op > = sgtr

  end (* structure String *)


