(* char-buffer.sml
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure CharBuffer :> MONO_BUFFER
                          where type elem = Char.char
                            and type vector = CharVector.vector
                            and type slice = CharVectorSlice.slice
                            and type array = CharArray.array
                            and type array_slice = CharArraySlice.slice
  = struct

    structure A = InlineT.CharArray
    structure V = InlineT.CharVector
    structure Word = InlineT.Word
    structure Int = InlineT.Int

  (* fast add/subtract avoiding the overflow test *)
    infix 6 -- ++
    fun x -- y = Int.fast_sub(x, y)
    fun x ++ y = Int.fast_add(x, y)

    type elem = char

    type vector = string
    type slice = CharVectorSlice.slice
    type array = A.array
    type array_slice = CharArraySlice.slice

    val emptyV : vector = ""

    datatype buf = BUF of {
        content : array ref,    (* array for holding content *)
        len : int ref,          (* current length of content *)
        initLen : int           (* initial size *)
      }

  (* default initial size *)
    val defaultInitLen = 4096

  (* maximum number of elements that a buffer can contain *)
    val maxLen = Core.max_length

  (* create a new buffer; the argument is a hit as to the requested capacity.
   * Use zero for the default size.
   *)
    fun new hint = let
          val n = if (hint < 0) orelse (maxLen < hint)
                then raise Size
                else if (hint = 0) then defaultInitLen
                else hint
          in
            BUF{
                content = ref(A.create n),
                len = ref 0,
                initLen = n
              }
          end

    fun contents (BUF{len = ref 0, ...}) = emptyV
      | contents (BUF{content=ref arr, len=ref n, ...}) = let
	  val v = Assembly.A.create_s n
	  fun cpy i = if (i < n)
		then (V.update(v, i, A.sub(arr, i)); cpy (i ++ 1))
		else ()
	  in
	    cpy 0;
	    v
	  end

    fun copy {src=BUF{content=ref src, len=ref n, ...}, dst, di} =
	  if (0 <= di) andalso Word.<(Word.fromInt(di ++ n), Word.fromInt(A.length dst))
	    then let
	      fun cpy (di, si) = if (si < n)
		    then (A.update(dst, di, A.sub(src, si)); cpy (di ++ 1, si ++ 1))
		    else ()
	      in
		cpy (di, 0)
	      end
	    else raise Subscript

    fun length (BUF{len=ref n, ...}) = n

    fun sub (BUF{content=ref arr, len=ref n, ...}, i) =
          if Word.<(Word.fromInt i, Word.fromInt n)
            then raise Subscript
            else A.sub(arr, i)

    fun clear (BUF{len, ...}) = (len := 0)

    fun reset (BUF{content, len, initLen}) = (
          len := 0;
          if (A.length(!content) <> initLen)
            then content := A.create initLen
            else ())

  (* lower bound on growth *)
    val minGrowAmount = 4096
  (* limit on extra growth *)
    val extraGrowthLimit = 256 * 1024

  (* ensure that the content array has space for at least amt additional
   * elements.  We assume that the resulting length will *not* exceed `maxLen`
   *)
    fun ensureCapacity (content as ref arr, len, amt) = let
	  val capacity = A.length arr
          in
            if (capacity < len ++ amt)
              then let
	      (* compute the amount to increase the capacity of the buffer.  We grow
	       * the buffer by 1.5 subject to certain limits.
	       *  - we grow by at least `amt` elements
	       *  - we grow by at least `minGrowAmount` elements
	       *  - we grow by no more than `amt+extraGrowthLimit` elements.
	       *)
		val growAmt = let
		      val half = Int.rshift(capacity, 0w1) (* 50% of current capacity *)
		      in
			if (amt >= half)
			  then Int.max(amt, minGrowAmount)
			else if (extraGrowthLimit <= half -- amt)
			  then amt ++ extraGrowthLimit
			  else half
		      end
		val newSz = Int.min (maxLen, capacity ++ growAmt)
                val newArr = A.create newSz
		fun cpy i = if (i < len)
		      then (A.update(newArr, i, A.sub(arr, i)); cpy (i ++ 1))
		      else ()
                in
                  cpy 0;
                  content := newArr
                end
              else ()
          end

    fun reserve (_, 0) = ()
      | reserve (BUF{content, len=ref len, ...}, n) =
          if (n < 0) then raise Size
          else ensureCapacity (content, len, n)

    fun add1 (BUF{content, len as ref n, ...}, elem) =
	  if (n < maxLen)
	    then (
	      ensureCapacity (content, n, 1);
	      A.update(!content, n, elem);
	      len := n ++ 1)
	    else raise Subscript

    fun addVec (BUF{content, len as ref n, ...}, src) = let
	  val srcLen = V.length src
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, V.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

    fun addSlice (BUF{content, len as ref n, ...}, slice) = let
	  val (src, si, srcLen) = CharVectorSlice.base slice
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, V.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

    fun addArr (BUF{content, len as ref n, ...}, src) = let
	  val srcLen = A.length src
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, A.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

    fun addArrSlice (BUF{content, len as ref n, ...}, slice) = let
	  val (src, si, srcLen) = CharArraySlice.base slice
	  fun cpy (dst, di, si) = if (si < srcLen)
		then (A.update(dst, di, A.sub(src, si)); cpy (dst, di ++ 1, si ++ 1))
		else ()
	  in
	    if (maxLen -- !len < srcLen)
	      then raise Subscript
	      else (
		ensureCapacity(content, n, srcLen);
		cpy (!content, n, 0);
		len := n ++ srcLen)
	  end

  end
