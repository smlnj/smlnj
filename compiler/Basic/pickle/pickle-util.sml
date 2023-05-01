(* pickle-util.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the new "generic" pickle utility which replaces Andrew Appel's
 * original "sharewrite" module.  Aside from formal differences, this
 * new module ended up not being any different from Andrew's.  However,
 * it ties in with its "unpickle" counterpart which is a *lot* better than
 * its predecessor.
 *
 * Generated pickles tend to be a little bit smaller, which can
 * probably be explained by the slightly more compact (in the common case,
 * i.e. for small absolute values) integer representation.
 *
 * July 1999, Matthias Blume
 *
 * Addendum: This module now also marks as "actually being shared" those
 * nodes where actual sharing has been detected.  Marking is done by
 * setting the high bit in the char code of the node.  This means that
 * char codes must be in the range [0,126] to avoid conflicts. (127
 * cannot be used because setting the high bit there results in 255 --
 * which is the backref code.)
 * This improves unpickling time by about 25% and also reduces memory
 * usage because much fewer sharing map entries have to be made during
 * unpickling.
 *
 * October 2000, Matthias Blume
 *)

(*
 * By the way, there is no point in trying to internally use
 * Word8Vector.vector instead of string for now.
 * These strings participate in order comparisons (which makes
 * Word8Vector.vector unsuitable).  Moreover, conversion between
 * string and Word8Vector.vector is currently just a cast, so it
 * does not cost anything in the end.
 *)
signature PICKLE_UTIL =
  sig

    type id

    (* Type info.  Use a different number for each type constructor. *)
    type tinfo = int			(* negative numbers are reserved! *)

    type 'ahm pickle
    type ('ahm, 'v) pickler = 'v -> 'ahm pickle

    (* $ produces the pickle for one case (constructor) of a datatype.
     * The string must be one character long and the argument pickle
     * should be the pickle for the constructor's arguments.
     * Use the same tinfo for all constructors of the same datatype
     * and different tinfos for constructors of different types.
     *
     * The latter is really only important if there are constructors
     * of different type who have identical argument types and use the
     * same $ identificaton string.  In this case the pickler might
     * identify two values of different types, and as a result the
     * unpickler will be very unhappy.
     *
     * On the other hand, if you use different tinfos for the same type,
     * then nothing terrible will happen.  You might lose some sharing,
     * though.
     *
     * The string argument could theoretically be more than one character
     * long.  In this case the corresponding unpickling function must
     * be sure to get all those characters out of the input stream.
     * We actually do exploit this "feature" internally. *)
    val $ : tinfo -> string * 'ahm pickle list -> 'ahm pickle

    (* "ah_share" is used to specify potential for "ad-hoc" sharing
     * using the user-supplied map.
     * Ad-hoc sharing is used to identify parts of the value that the
     * hash-conser cannot automatically identify but which should be
     * identified nevertheless, or to identify those parts that would be
     * too expensive to be left to the hash-conser. *)
    val ah_share : {
	    find : 'ahm * 'v -> id option,
	    insert : 'ahm * 'v * id -> 'ahm
	  } -> ('ahm, 'v) pickler -> ('ahm, 'v) pickler

    (* generating pickles for values of some basic types *)
    val w_bool : ('ahm, bool) pickler
    val w_int : ('ahm, int) pickler
    val w_word : ('ahm, word) pickler
    val w_int32 : ('ahm, Int32.int) pickler
    val w_word32 : ('ahm, Word32.word) pickler
    val w_string : ('ahm, string) pickler
    val w_intinf : ('ahm, IntInf.int) pickler

    (* generating pickles for some parameterized types (given a pickler
     * for the parameter) *)
    val w_list : ('ahm, 'a) pickler -> ('ahm, 'a list) pickler
    val w_option : ('ahm, 'a) pickler -> ('ahm, 'a option) pickler
    val w_pair :
	('ahm, 'a) pickler * ('ahm, 'b) pickler -> ('ahm, 'a * 'b) pickler

    (* Pickling a "lazy" value (i.e., a thunk);  the thunk will be forced
     * by the pickler. Unpickling is lazy again; but, of course, that
     * laziness is unrelated to the laziness of the original value. *)
    val w_lazy : ('ahm, 'a) pickler -> ('ahm, unit -> 'a) pickler

    (* run the pickle, i.e., turn it into a string *)
    val pickle : 'ahm -> 'ahm pickle -> string

    (* The xxx_lifter stuff is here to allow picklers to be "patched
     * together".  If you already have a pickler that uses a sharing map
     * of type B and you want to use it as part of a bigger pickler that
     * uses a sharing map of type A, you must write a (B, A) map_lifter
     * which then lets you lift the existing pickler to one that uses
     * type A maps instead of its own type B maps.
     *
     * The idea is that B maps are really part of A maps. They can be
     * extracted for the duration of using the existing pickler.  Then,
     * when that pickler is done, we can patch the resulting new B map
     * back into the original A map to obtain a new A map. *)
    type ('b_ahm, 'a_ahm) map_lifter =
	{ extract: 'a_ahm -> 'b_ahm, patchback: 'a_ahm * 'b_ahm -> 'a_ahm }

    val lift_pickler: ('b_ahm, 'a_ahm) map_lifter ->
	('b_ahm, 'v) pickler -> ('a_ahm, 'v) pickler
end

structure PickleUtil :> PICKLE_UTIL = struct

    type pos = int
    type id = pos
    type codes = id list
    type tinfo = int

    type shareinfo = IntRedBlackSet.set
    val si_empty = IntRedBlackSet.empty
    val si_add = IntRedBlackSet.add
    val si_list = IntRedBlackSet.listItems

    structure HCM = RedBlackMapFn
  	(struct
  	    type ord_key = string * tinfo * codes
  	    fun compare ((c, t, l), (c', t', l')) = let
  		fun codesCmp ([], []) = EQUAL
  		  | codesCmp (_ :: _, []) = GREATER
  		  | codesCmp ([], _ :: _) = LESS
  		  | codesCmp (h :: t, h' :: t') =
		    if h < h' then LESS else if h > h' then GREATER
		    else codesCmp (t, t')
  	    in
		if t < t' then LESS else if t > t' then GREATER
		else case String.compare (c, c') of
		    EQUAL => codesCmp (l, l')
  		  | unequal => unequal
  	    end
  	end)

    structure PM = IntRedBlackMap

    datatype pre_result =
	STRING of string
      | CONCAT of pre_result * pre_result

    fun pre_size (STRING s) = size s
      | pre_size (CONCAT (p, p')) = pre_size p + pre_size p'

    val backref = STRING "\255"
    val size_backref = 1
    val nullbytes = STRING ""

    type hcm = id HCM.map
    type fwdm = id PM.map		(* forwarding map *)
    type 'ahm state = hcm * fwdm * 'ahm * pos * shareinfo

    type 'ahm pickle = 'ahm state -> codes * pre_result * 'ahm state
    type ('ahm, 'v) pickler = 'v -> 'ahm pickle

    infix 3 $
    infixr 4 &

    fun (f & g) state = let
	val (fc, fpr, state') = f state
	val (gc, gpr, state'') = g state'
    in
	(fc @ gc, CONCAT (fpr, gpr), state'')
    end

    (* collapse a non-empty list of pickles into one *)
    fun collapse (h, []) = h
      | collapse (h, ht :: tt) = h & collapse (ht, tt)

    fun anyint_encode (n, negative) = let
	(* this is essentially the same mechanism that's also used in
	 * TopLevel/batch/binfile.sml (maybe we should share it) *)
	val // = LargeWord.div
	val %% = LargeWord.mod
	val !! = LargeWord.orb
	infix // %% !!
	val toW8 = Word8.fromLargeWord
	fun r (0w0, l) = Word8Vector.fromList l
	  | r (n, l) =
	    r (n // 0w128, toW8 ((n %% 0w128) !! 0w128) :: l)
	val lastDigit = n %% 0w64
	val lastByte = if negative then lastDigit !! 0w64 else lastDigit
    in
	Byte.bytesToString (r (n // 0w64, [toW8 lastByte]))
    end

    fun largeword_encode n = anyint_encode (n, false)
    fun largeint_encode i =
	if i >= 0 then anyint_encode (LargeWord.fromLargeInt i, false)
	(* careful to do the negation in word domain... *)
	else anyint_encode (0w0 - LargeWord.fromLargeInt i, true)

    val word32_encode = largeword_encode o Word32.toLargeWord
    val word_encode = largeword_encode o Word.toLargeWord

    val int32_encode = largeint_encode o Int32.toLarge
    val int_encode = largeint_encode o Int.toLarge

    fun % ti c (hcm, fwdm, ahm, next, si) = let
	val key = (c, ti, [])
        in
	  case HCM.find (hcm, key)
	   of SOME i =>
		([i], STRING c, (hcm, PM.insert (fwdm, next, i),
				 ahm, next + size c, si))
	    | NONE =>
		([next], STRING c, (HCM.insert (hcm, key, next), fwdm,
				    ahm, next + size c, si))
        end

    fun dollar ti (c, []) state = % ti c state
      | dollar ti (c, plh :: plt) (hcm, fwdm, ahm, next, si) = let
	    val p = collapse (plh, plt)
	    val (codes, pr, (hcm', fwdm', ahm', next', si')) =
		p (hcm, fwdm, ahm, next + size c, si)
	    val key = (c, ti, codes)
	in
	    case HCM.find (hcm, key) of
		SOME i => let
		    val brnum = int_encode i
		in
		    ([i], CONCAT (backref, STRING brnum),
		     (hcm, PM.insert (fwdm, next, i),
		      ahm, next + size_backref + size brnum,
		      si_add (si', i)))
		end
	      | NONE =>
		([next], CONCAT (STRING c, pr),
		 (HCM.insert (hcm', key, next), fwdm', ahm', next', si'))
	end

    fun ah_share { find, insert } w v (hcm, fwdm, ahm, next, si) =
	case find (ahm, v) of
	    SOME i0 => let
		val i = getOpt (PM.find (fwdm, i0), i0)
		val brnum = int_encode i
	    in
		([i], CONCAT (backref, STRING brnum),
		 (hcm, fwdm, ahm, next + size_backref + size brnum,
		  si_add (si, i)))
	    end
	  | NONE => w v (hcm, fwdm, insert (ahm, v, next), next, si)

    fun w_lazy w thunk (hcm, fwdm, ahm, next, si) = let
	val v = thunk ()
	(* The larger the value of trialStart, the smaller the chance that
	 * the loop (see below) will run more than once.  However, some
	 * space may be wasted.  3 should avoid this most of the time.
	 * (Experience shows: 2 doesn't.) *)
	val trialStart = 3
	(* This loop is ugly, but we don't expect it to run very often.
	 * It is needed because we must first write the length of the
	 * encoding of the thunk's value, but that encoding depends
	 * on the length (or rather: on the length of the length). *)
	fun loop (nxt, ilen) = let
	    val (codes, pr, state) = w v (hcm, fwdm, ahm, nxt, si)
	    val sz = pre_size pr
	    val ie = int_encode sz
	    val iesz = size ie

	    (* Padding in front is better because the unpickler can
	     * simply discard all leading 0s and does not need to know
	     * about the pickler's setting of "trialStart". *)
	    val null = STRING "\000"
	    fun pad (pr, n) =
		if n = 0 then pr
		else pad (CONCAT (null, pr), n - 1)
	in
	    if ilen < iesz then loop (nxt + 1, ilen + 1)
	    else (codes, CONCAT (pad (STRING ie, ilen - iesz), pr), state)
	end
    in
	loop (next + trialStart, trialStart)
    end

    local
	val I = ~1
	val W = ~2
	val I32 = ~3
	val W32 = ~4
    in
	(* Even though the encoding could start with the
	 * backref character, we know that it isn't actually a backref
	 * because % suppresses back-references.
	 * Of course, this must be taken care of by unpickle-util! *)
	fun w_int i = % I (int_encode i)
	fun w_word w = % W (word_encode w)
	fun w_int32 i32 = % I32 (int32_encode i32)
	fun w_word32 w32 = % W32 (word32_encode w32)
    end

    local
	val L = ~5
	fun chop5 l = let
	    fun ch (a :: b :: c :: d :: e :: r, cl) =
		ch (r, (e, d, c, b, a) :: cl)
	      | ch (r, cl) = (rev r, cl)
	in
	    ch (rev l, [])
	end
    in
	fun w_list w l = let
	    val op $ = dollar L
	    fun wc [] = % L "N"
	      | wc ((a, b, c, d, e) :: r) =
		"C" $ [w a, w b, w c, w d, w e, wc r]
	in
	    case chop5 l of
		([], []) => % L "0"
	      | ([a], []) => "1" $ [w a]
	      | ([a, b], []) => "2" $ [w a, w b]
	      | ([a, b, c], []) => "3" $ [w a, w b, w c]
	      | ([a, b, c, d], []) => "4" $ [w a, w b, w c, w d]
	      | ([], r) => "5" $ [wc r]
	      | ([a], r) => "6" $ [w a, wc r]
	      | ([a, b], r) => "7" $ [w a, w b, wc r]
	      | ([a, b, c], r) => "8" $ [w a, w b, w c, wc r]
	      | ([a, b, c, d], r) => "9" $ [w a, w b, w c, w d, wc r]
	      | _ => raise Fail "PickleUtil.w_list: impossible chop"
	end
    end

    local
	val O = ~6
    in
	fun w_option arg = let
	    val op $ = dollar O
	    fun wo w NONE = % O "n"
	      | wo w (SOME i) = "s" $ [w i]
	in
	    wo arg
	end
    end

    local
	val P = ~7
    in
	fun w_pair (wa, wb) (a, b) = let
	    val op $ = dollar P
	in
	    "p" $ [wa a, wb b]
	end
    end

    local
	val S = ~8
    in
	fun w_string s = let
	    val op $ = dollar S
	    (* The dummy_pickle is a hack to get strings to be identified
	     * automatically. They don't have "natural" children, so normally
	     * % would suppress the backref.  The dummy pickle produces no
	     * codes and no output, but it is there to make $ believe that
	     * there are children. *)
	    fun dummy_pickle state = ([], nullbytes, state)
	    fun esc #"\\" = "\\\\"
	      | esc #"\"" = "\\\""
	      | esc #"\255" = "\\\255"	(* need to escape backref char *)
	      | esc c = String.str c
	in
	    (concat ["\"", String.translate esc s, "\""]) $ [dummy_pickle]
	end
    end

  (* for now, we pickle IntInf.int by pickling its string representation *)
    fun w_intinf n = w_string (IntInf.toString n)

    local
	val B = ~9
    in
	fun w_bool true = % B "t"
	  | w_bool false = % B "f"
    end

    local
	fun pr2s (pr, next, si) = let

	    (* This puts a code string in front of the list of
	     * code strings that follow.  It also takes care of
	     * setting the high bit where necessary (see below),
	     * updates the current position and the list of remaining
	     * shared positions. *)
	    fun add ("", p, h, t, l) = (p, h :: t, l)
	      | add (s, p, h, t, l) = let
		    val len = size s
		    val p' = p - len
		in
		    if p' = h then let
			val fst =
			    String.str
			      (Char.chr
			        (Char.ord (String.sub (s, 0)) + 128))
			fun ret x = (p', t, x)
		    in
			if len > 1 then
			    ret (fst :: String.extract (s, 1, NONE) :: l)
			else ret (fst :: l)
		    end
		    else (p', h :: t, s :: l)
		end

	    (* Fast flattening -- when we are out of shared codes. *)
	    fun fflat (STRING s, l) = s :: l
	      | fflat (CONCAT (x, STRING s), l) = fflat (x, s :: l)
	      | fflat (CONCAT (x, CONCAT (y, z)), l) =
		fflat (CONCAT (CONCAT (x, y), z), l)

	    (* Flattening runs in linear time.
	     * We simultaneously use this loop to set the high bits in
	     * codes that correspond to shared nodes.  The positions of
	     * these codes are given by high-to-low sorted list of
	     * integers. *)
	    fun flat (x, (_, [], l)) = fflat (x, l)
	      | flat (STRING s, (p, h :: t, l)) =
		#3 (add (s, p, h, t, l))
	      | flat (CONCAT (x, STRING s), (p, h :: t, l)) =
		flat (x, add (s, p, h, t, l))
	      | flat (CONCAT (x, CONCAT (y, z)), phtl) =
		flat (CONCAT (CONCAT (x, y), z), phtl)
	in
	    concat (flat (pr, (next, rev (si_list si), [])))
	end
    in
	fun pickle emptyMap p = let
	    val (_, pr, (_, _, _, next, si)) =
		 p (HCM.empty, PM.empty, emptyMap, 0, si_empty)
	in
	    pr2s (pr, next, si)
	end
    end

    type ('b_ahm, 'a_ahm) map_lifter =
        { extract: 'a_ahm -> 'b_ahm, patchback: 'a_ahm * 'b_ahm -> 'a_ahm }

    fun lift_pickler { extract, patchback } wb b (hcm, fwdm, a_ahm, next, si) =
	let val b_ahm = extract a_ahm
	    val (codes, pr, (hcm', fwdm', b_ahm', next', si')) =
		wb b (hcm, fwdm, b_ahm, next, si)
	    val a_ahm' = patchback (a_ahm, b_ahm')
	in
	    (codes, pr, (hcm', fwdm', a_ahm', next', si'))
	end

    (* for export *)
    nonfix $
    val $ = dollar
end
