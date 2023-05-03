(* unpickle-util.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the new "generic" unpickle utility.  It replaces Andrew Appel's
 * original "shareread" module.  The main difference is that instead of
 * using a "universal" type together with numerous injections and projections
 * it uses separate maps.  This approach proves to be a lot more light-weight.
 *
 * The benefits are:
 *   - no projections, hence no "match nonexhaustive" warnings and...
 *   - ...no additional run-time overhead (all checking is done during
 *     the map membership test which is common to both implementations)
 *   - no necessity for those many tiny "fn"-functions that litter the
 *        original code, resulting in...
 *   - ...a more "natural" style for writing the actual unpickling code
 *        that makes for shorter source code
 *   - ...a lot less generated machine code (less than 50% of the original
 *        version)
 *   - ...slightly faster operation (around 15% speedup)
 *        (My guess is that it is a combination of fewer projections and
 *         fewer generated thunks that make the code run faster.)
 *
 * July 1999, Matthias Blume
 *
 * We now use the high bit in char codes of shareable nodes to indicate
 * that actual sharing has occured.  If the high bit is not set, we do
 * no longer bother to insert the node into its sharing map.  This
 * improves unpickling speed (e.g., for autoloading) by about 25% and
 * saves tons of memory.
 *
 * October 2000, Matthias Blume
 *)

signature UNPICKLE_UTIL =
  sig

    exception Format

    type 'v map				(* one for each type *)
    type session			(* encapsulates unpickling state *)

    (* Make a type-specific sharing map using "mkMap".
     *
     * Be sure to create such maps only locally, otherwise you have a
     * space leak.
     *
     * The ML type system will prevent you from accidentially using the
     * same map for different types, so don't worry.  But using TOO MANY
     * maps (i.e., more than one map for the same type) will likely
     * cause problems because the unpickler might try to look for a
     * back reference that is in a different map than the one where the
     * value is actually registered.
     *
     * By the way, this warning is not unique to the many-maps approach.
     * The same thing would happen with the original "universal domain"
     * unpickler if you declare two different constructors for the
     * same type.  Given that there are about 100 types (and thus
     * 100 constructors or maps) in the SML/NJ environment pickler,
     * the possibility for such a mistake is not to be dismissed. *)
    val mkMap : unit -> 'v map

    type 'v reader = unit -> 'v

    (* A "charGetter" is the mechanism that gets actual characters from
     * the pickle.  For ordinary pickles, the unpickler will never call
     * "seek".  Moreover, the same is true if you read the pickles created
     * by pickleN sequentially from the first to the last (i.e., not
     * "out-of-order"). "cur" determines the current position and must be
     * implemented. *)
    type charGetter =
	{ read: char reader, seek: int -> unit, cur: unit -> int }

    (* the string is the pickle string *)
    val stringGetter : string -> charGetter

    (* stringGetter' is a souped-up stringGetter:
     *  It takes a function to produce (and re-produce) the pickle string
     *  on demand and returns the actual charGetter together with a
     *  "dropper" -- a function to let go of the pickle string.
     *  When suspended unpickling resumes after the string got dropped,
     *  the charGetter will automatically re-fetch the pickle string
     *  using the function provided. *)
    val stringGetter' : string option * (unit -> string) ->
	{ getter: charGetter, dropper: unit -> unit }

    (* open the unpickling session - everything is parameterized by this;
     * the charGetter provides the bytes of the pickle *)
    val mkSession : charGetter -> session

    (* The typical style is to write a "reader" function for each type
     * The reader function uses a local helper function that takes the
     * first character of a pickle (this is usually the discriminator that
     * was given to $ or % in the pickler) and returns the unpickled
     * value.  The function recursively calls other "reader" functions.
     * To actually get the value from the pickle, pass the helper
     * to "share" -- together with the current session and the
     * type-specific map.  "share" will take care of back-references
     * (using the map) and pass the first character to your helper
     * when necessary.  The standard pattern for writing a "t reader"
     * therefore is:
     *
     * val session = UnpickleUtil.mkSession pickle
     * fun share m f = UnpickleUtil.share session m f
     * ...
     * val t_map = Unpickleutil.mkMap ()
     * ...
     * fun r_t () = let
     *     fun t #"a" = ... (* case a *)
     *       | t #"b" = ... (* case b *)
     *       ...
     *       | _ = raise UnpickleUtil.Format
     * in
     *     share t_map t
     * end
     *)
    val share : session -> 'v map -> (char -> 'v) -> 'v

    (* If you know that you don't need a map because there can be no
     * sharing, then you can use "nonshare" instead of "share". *)
    val nonshare : session -> (char -> 'v) -> 'v

    (* making readers for some common types *)
    val r_int : session -> int reader
    val r_int32 : session -> Int32.int reader
    val r_word : session -> word reader
    val r_word32 : session -> Word32.word reader
    val r_bool : session -> bool reader
    val r_string : session -> string reader
    val r_intinf : session -> IntInf.int reader

    (* readers for parametric types need their own map *)
    val r_list : session -> 'v list map -> 'v reader -> 'v list reader
    val r_option : session -> 'v option map -> 'v reader -> 'v option reader
    val r_pair :
	session -> ('a * 'b) map -> 'a reader * 'b reader -> ('a * 'b) reader

    (* The laziness generated here is in the unpickling.  In other words
     * unpickling state is not discarded until the last lazy value has been
     * forced. *)
    val r_lazy : session -> 'a reader -> (unit -> 'a) reader
end

structure UnpickleUtil :> UNPICKLE_UTIL = struct

    structure M = IntRedBlackMap

    exception Format

    type 'v map = ('v * int) M.map ref
    type state = string map

    type 'v reader = unit -> 'v

    type charGetter =
	{ read: char reader, seek: int -> unit, cur: unit -> int }

    type session = { state: state, getter: charGetter }

    fun mkMap () = ref M.empty

    fun stringGetter pstring = let
	val pos = ref 0
	fun rd () = let
	    val p = !pos
	in
	    pos := p + 1;
	    String.sub (pstring, p) handle Subscript => raise Format
	end
	fun sk p = pos := p
	fun cur () = !pos
    in
	{ read = rd, seek = sk, cur = cur }
    end

    fun stringGetter' (initial, fetchString) = let
	val pos = ref 0
	val pstring_r = ref initial
	fun grabString () =
	    case !pstring_r of
		SOME s => s
	      | NONE => let
		    val s = fetchString ()
		in
		    pstring_r := SOME s;
		    s
		end
	fun dropper () = pstring_r := NONE
	fun rd () = let
	    val s = grabString ()
	    val p = !pos
	in
	    pos := p + 1;
	    String.sub (s, p) handle Subscript => raise Format
	end
	fun sk p = pos := p
	fun cur () = !pos
    in
	{ getter = { read = rd, seek = sk, cur = cur }, dropper = dropper }
    end

    local
	fun f_anyint rd () = let
	    val & = Word8.andb
	    infix &
	    val large = Word8.toLargeWord
	    fun loop n = let
		val w8 = Byte.charToByte (rd ())
	    in
		if (w8 & 0w128) = 0w0 then
		    (n * 0w64 + large (w8 & 0w63), (w8 & 0w64) <> 0w0)
		else loop (n * 0w128 + large (w8 & 0w127))
	    end
	in
	    loop 0w0
	end

	fun f_largeword cvt rd () =
	    case f_anyint rd () of
		(w, false) => (cvt w handle _ => raise Format)
	      | _ => raise Format

	fun f_largeint cvt rd () = let
	    val (wpos, negative) = f_anyint rd ()
	    (* The negation must be done in word domain to prevent
	     * overflow on minInt. For the same reason we must then
	     * use toIntX. *)
	    val w = if negative then 0w0 - wpos else wpos
	    val i = LargeWord.toLargeIntX w
	in
	    cvt i handle _ => raise Format
	end
    in
	val f_int = f_largeint Int.fromLarge
	val f_int32 = f_largeint Int32.fromLarge
	val f_word = f_largeword Word.fromLargeWord
	val f_word32 = f_largeword Word32.fromLargeWord
    end

    fun mkSession charGetter =
	({ state = mkMap (), getter = charGetter }: session)

    fun share { state, getter = { read, seek, cur } } m r = let
	(* "firsttime" is guaranteed to be called with a character
	 * that has the high-bit set. *)
	fun firsttime (pos, c) = let
	    val v = r (Char.chr (Char.ord c - 128))
	    val pos' = cur ()
	in
	    m := M.insert (!m, pos, (v, pos'));
	    v
	end
	val c = read ()
    in
	if Char.ord c < 128 then
	    (* High-bit is not set, so this is not a shared node.
	     * Therefore, it can't possibly be in the map, and
	     * we can call r directly. *)
	    r c
	else if c = #"\255" then let
	    val pos = f_int read ()
	in
	    case M.find (!m, pos) of
		SOME (v, _) => v
	      | NONE => let
		    val here = cur ()
		in
		    seek pos;
		    (* It is ok to use "read" here because
		     * there won't be back-references that jump
		     * to other back-references.
		     * (Since we are jumping to something that
		     * was shared, it has the high-bit set, so
		     * calling "firsttime" is ok.) *)
		    firsttime (pos, read())
		    before seek here
		end
	end
	else let
	    (* Must subtract one to get back in front of c. *)
	    val pos = cur () - 1
	in
	    case M.find (!m, pos) of
		SOME (v, pos') => (seek pos'; v)
	      | NONE => firsttime (pos, c)
	end
    end

    (* "nonshare" gets around backref detection.  Certain integer
     * encodings may otherwise be mis-identified as back references.
     * Moreover, unlike in the case of "share" we don't need a map
     * for "nonshare".  This could be used as an optimization for
     * types that are known to never be shared anyway (bool, etc.). *)
    fun nonshare (s: session) f = f (#read (#getter s) ())

    local
	fun f2r f_x (s: session) = f_x (#read (#getter s))
    in
	val r_int = f2r f_int
	val r_int32 = f2r f_int32
	val r_word = f2r f_word
	val r_word32 = f2r f_word32
    end

    fun r_lazy session r () = let
	val memo = ref (fn () => raise Fail "UnpickleUtil.r_lazy")
	val { getter = { cur, seek, ... }, ... } = session
	(* the size may have leading 0s because of padding *)
	fun getSize () = let
	    val sz = r_int session ()
	in
	    if sz = 0 then getSize () else sz
	end
	val sz = getSize ()		(* size of v *)
	val start = cur ()		(* start of v *)
	fun thunk () = let
	    val wherever = cur ()	(* remember where we are now *)
	    val _ = seek start		(* go to start of v *)
	    val v = r ()		(* read v *)
	in
	    seek wherever;		(* go back to where we were *)
	    memo := (fn () => v);	(* memoize *)
	    v
	end
    in
	memo := thunk;
	seek (start + sz);		(* as if we had read the value *)
	(fn () => !memo ())
    end

    fun r_list session m r () = let
	fun r_chops () = let
	    fun rcl #"N" = []
	      | rcl #"C" = r () :: r () :: r () :: r () :: r () :: r_chops ()
	      | rcl _ = raise Format
	in
	    share session m rcl
	end
	fun rl #"0" = []
	  | rl #"1" = [r ()]
	  | rl #"2" = [r (), r ()]
	  | rl #"3" = [r (), r (), r ()]
	  | rl #"4" = [r (), r (), r (), r ()]
	  | rl #"5" = r_chops ()
	  | rl #"6" = r () :: r_chops ()
	  | rl #"7" = r () :: r () :: r_chops ()
	  | rl #"8" = r () :: r () :: r () :: r_chops ()
	  | rl #"9" = r () :: r () :: r () :: r () :: r_chops ()
	  | rl _ = raise Format
    in
	share session m rl
    end

    fun r_option session m r () = let
	fun ro #"n" = NONE
	  | ro #"s" = SOME (r ())
	  | ro _ = raise Format
    in
	share session m ro
    end

    fun r_pair session m (r_a, r_b) () = let
	fun p #"p" = (r_a (), r_b ())
	  | p _ = raise Format
    in
	share session m p
    end

    fun r_bool session () = let
	fun rb #"t" = true
	  | rb #"f" = false
	  | rb _ = raise Format
    in
	nonshare session rb
    end

    fun r_string session () = let
	val { state = m, getter } = session
	val { read, ... } = getter
	fun rs #"\"" =
	    let fun loop (l, #"\"") = String.implode (rev l)
		  | loop (l, #"\\") = loop (read () :: l, read ())
		  | loop (l, c) = loop (c :: l, read ())
	    in
		loop ([], read ())
	    end
	  | rs _ = raise Format
    in
	share session m rs
    end

  (* for now, we pickle IntInf.int by pickling its string representation *)
    fun r_intinf session () = let
	  val s = r_string session ()
	  in
	    case IntInf.fromString s
	     of SOME n => n
	      | NONE => raise Format
	    (* end case *)
	  end

end
