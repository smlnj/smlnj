(* format-comb.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 *   Well-typed "printf" for SML, aka "Unparsing Combinators".
 *     This code was written by Matthias Blume (2002).  Inspiration
 *     obtained from Olivier Danvy's "Functional Unparsing" work.
 *
 * See format-comb-sig.sml for details.
 *)
structure FormatComb :> FORMAT_COMB =
  struct

    type 'a format         = string list -> 'a
    type ('a, 'b) fragment = 'a format -> 'b format
    type 'a glue           = ('a, 'a) fragment
    type ('a, 't) element  = ('a, 't -> 'a) fragment
    type 'a gg = 'a glue * 'a glue

    type place = int * int -> int
    fun left (a, i)   = a - i
    fun center (a, i) = Int.quot (a - i, 2)
    fun right (a, i)  = 0

    local
	(* Generic padding, trimming, and fitting.  Nestability
	 * is achieved by remembering the current state s, passing
	 * a new empty one to the fragment, adjusting the output
	 * from that, and fitting the result back into the remembered
	 * state. ("States" are string lists and correspond to
	 * output coming from fragments to the left of the current point.) *)
	fun ptf adj pl n fr fm s = let
	    fun work s' = let
		val x' = concat (rev s')
		val sz = size x'
	    in
		adj (x', sz, n, pl (sz, n)) :: s
	    end
	in
	    (fr (fm o work)) []
	end

	val padRight = StringCvt.padRight #" "
	val padLeft = StringCvt.padLeft #" "
	fun pad0 (s, sz, n, off) = padRight n (padLeft (sz - off) s)
	fun trim0 (s, _, n, off) = String.substring (s, off, n)
	fun pad1 (arg as (s, sz, n, _)) = if n < sz then s else pad0 arg
	fun trim1 (arg as (s, sz, n, _)) = if n > sz then s else trim0 arg
	fun fit1 (arg as (_, sz, n, _)) = (if n < sz then trim0 else pad0) arg
    in
	fun format' rcv fr   = fr (rcv o rev) []
        fun format fr        = format' concat fr

	fun using cvt fm x a = fm (cvt a :: x)

        fun int fm           = using Int.toString fm
	fun real fm          = using Real.toString fm
	fun bool fm          = using Bool.toString fm
	fun string fm        = using (fn x => x) fm
	fun string' fm       = using String.toString fm
	fun char fm          = using String.str fm
	fun char' fm         = using Char.toString fm

	fun int' rdx fm      = using (Int.fmt rdx) fm
	fun real' rfmt fm    = using (Real.fmt rfmt) fm

	fun pad place        = ptf pad1 place
	fun trim place       = ptf trim1 place
	fun fit place        = ptf fit1 place
    end

    fun padl n = pad left n
    fun padr n = pad right n

    fun glue e a fm x = e fm x a
    fun elem gm fm x a = gm a fm x

    fun nothing fm    = fm
    fun text s        = glue string s
    fun sp n          = pad left n nothing
    fun nl fm         = text "\n" fm
    fun tab fm        = text "\t" fm

    fun seqg (foldr : ('x * 'a gg -> 'a gg) -> 'a gg -> 'c -> 'a gg) sep g s =
	#2 (foldr (fn (x, (f, r)) => (sep, g x o f o r)) (nothing, nothing) s)
    fun listg g l = text "[" o seqg List.foldr (text ", ") g l o text "]"
    fun optiong g NONE = text "NONE"
      | optiong g (SOME a) = text "SOME(" o g a o text ")"

    fun seq foldr sep e = elem (seqg foldr sep (glue e))
    fun list e = elem (listg (glue e))
    fun option e = elem (optiong (glue e))

  end
