(* hash.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Hashing combinators; loosely based on the CCHash module
 *)

signature HASHING =
  sig

(* QUESTION: should the "hash state" be abstract? *)
    (* a hash function that accumulates an intermediate value *)
    type 'a t = 'a * Word64.word -> Word64.word

    (* a hash function that produces a hash of a value *)
    type 'a hash = 'a -> word

    val finish : 'a t -> 'a hash

    (* hash functions for Basis types *)
    val bool : bool t
    val bool' : bool hash
    val char : char t
    val char' : char hash
    val int : int t
    val int' : int hash
    val int32 : Int32.int t
    val int32' : Int32.int hash
    val int64 : Int64.int t
    val int64' : Int64.int hash
    val intInf : IntInf.int t
    val intInf' : IntInf.int hash
    val word : word t
    val word' : word hash
    val word8 : Word8.word t
    val word8' : Word8.word hash
    val word32 : Word32.word t
    val word32' : Word32.word hash
    val word64 : Word64.word t
    val word64' : Word64.word hash
    val substring : substring t
    val substring' : substring hash
    val string : string t
    val string' : string hash
    val real : real t
    val real' : real hash
    val real64 : Real64.real t
    val real64' : Real64.real hash

    val word8Array : Word8Array.array t
    val word8Array' : Word8Array.array hash
    val word8ArraySlice : Word8ArraySlice.slice t
    val word8ArraySlice' : Word8ArraySlice.slice hash
    val word8Vector : Word8Vector.vector t
    val word8Vector' : Word8Vector.vector hash
    val word8VectorSlice : Word8VectorSlice.slice t
    val word8VectorSlice' : Word8VectorSlice.slice hash
    val charArray : CharArray.array t
    val charArray' : CharArray.array hash
    val charArraySlice : CharArraySlice.slice t
    val charArraySlice' : CharArraySlice.slice hash
    val realArray : RealArray.array t
    val realArray' : RealArray.array hash
    val realArraySlice : RealArraySlice.slice t
    val realArraySlice' : RealArraySlice.slice hash
    val realVector : RealVector.vector t
    val realVector' : RealVector.vector hash
    val realVectorSlice : RealVectorSlice.slice t
    val realVectorSlice' : RealVectorSlice.slice hash

    val option : 'a t -> 'a option t
    val either : ('a t * 'b t) -> ('a, 'b) Either.either t
    val list : 'a t -> 'a list t
    val array : 'a t -> 'a array t
    val arraySlice : 'a t -> 'a ArraySlice.slice t
    val vector : 'a t -> 'a vector t
    val vectorSlice : 'a t -> 'a VectorSlice.slice t

    (* generic hasher for indexed sequences *)
    val sequence : {
            length : 'a -> int,
            get : 'a * int -> 'b,
            hash : 'b t
          } -> 'a t

    (* generic hasher for streams *)
    val stream : {
            get : 'a -> ('b * 'a) option,
            hash : 'b t
          } -> 'a t

    val pair : ('a t * 'b t) -> ('a * 'b) t
    val triple : ('a t * 'b t * 'c t) -> ('a * 'b * 'c) t
    val quad : ('a t * 'b t * 'c t * 'd t) -> ('a * 'b * 'c * 'd) t

    (* combine hash values *)
    val combine2 : Word64.word * Word64.word -> Word64.word
    val combine3 : Word64.word * Word64.word * Word64.word -> Word64.word
    val combine4 : Word64.word * Word64.word * Word64.word * Word64.word -> Word64.word
    val combine5 : Word64.word * Word64.word * Word64.word * Word64.word * Word64.word -> Word64.word
    val combine6 : Word64.word * Word64.word * Word64.word * Word64.word * Word64.word * Word64.word -> Word64.word

  end

structure Hash : HASHING =
  struct

    structure W64 = Word64
    structure W = Word

    type 'a t = 'a * Word64.word -> Word64.word

    type 'a hash = 'a -> word

    (* FNV parameters for 64-bit hashing;.  See
     *
     *     https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
     *
     * and
     *
     *     https://datatracker.ietf.org/doc/html/draft-eastlake-fnv-35.html
     *
     * for more information.
     *)
    val fnvPrime : W64.word = 0wx00000100000001b3
    val fnvBasis : W64.word = 0wxcbf29ce484222325

    (* utility functions *)
    fun wTo64 w = W64.fromLarge(W.toLarge w)
    fun wFrom64 w = W.fromLarge(W64.toLarge w)
    fun byte w = W64.andb(w, 0wxFF)
    fun shl8 w = W64.>>(w, 0w8)

    (* the FNV-1a hashing function for a byte `b` and accumulated hash `acc`. *)
    fun hashByte (b, acc) = W64.xorb(acc, b) * fnvPrime

    fun +> (acc, b) = hashByte (acc, b)
    infix +>

    (* hash `n` bytes of a 64-bit word *)
    fun hashW64 (0, w, acc) = acc
      | hashW64 (n, w, acc) = hashW64 (n-1, W64.>>(w, 0w8), acc +> byte w)

    fun bool (false, acc) = hashByte(0w0, acc)
      | bool (true, acc) = hashByte(0w1, acc)

    fun char (c, acc) = hashByte(W64.fromInt(ord c), acc)

    val bytesPerWord = Unsafe.wordSize() div 8

    fun finish (h : 'a t) (x : 'a) = wFrom64 (h (x, fnvBasis))

    fun int (i, acc) = hashW64 (bytesPerWord, W64.fromInt i, acc)

    fun int32 (i, acc) = hashW64 (4, W64.fromLargeInt(Int32.toLarge i), acc)
    fun int64 (i, acc) = hashW64 (8, W64.fromLargeInt(Int64.toLarge i), acc)
    fun intInf (0, acc) = hashByte(0w0, acc)
      | intInf (i, acc) = let
          val (i, acc) = if (i < 0) then (~i, hashByte(0w1, acc)) else (i, acc)
          fun lp (0, acc) = acc
            | lp (i, acc) = let
                val b = W64.andb(W64.fromLargeInt i, 0wxff)
                in
                  lp (IntInf.~>>(i, 0w8), hashByte(b, acc))
                end
          in
            lp (i, acc)
          end

    fun word (w, acc) = hashW64 (bytesPerWord, wTo64 w, acc)
    fun word8 (w, acc) = hashByte (Word8.toLarge w, acc)
    fun word32 (w, acc) = hashW64 (4, W64.fromLarge(Word32.toLarge w), acc)
    fun word64 (w, acc) = hashW64 (8, W64.fromLarge(Word64.toLarge w), acc)

    fun real (r, acc) = hashW64 (8, Unsafe.Real64.castToWord r, acc)
    val real64 = real

    local
      fun hashSeq (length, get, hash) (seq, acc : W64.word) : W64.word = let
            fun lp (0, acc) = acc
              | lp (i, acc) = lp (i-1, hash(get(seq, i), acc))
            in
              lp (length seq - 1, acc)
            end
    in
    val word8Array = hashSeq (Word8Array.length, Word8Array.sub, word8)
    val word8ArraySlice = hashSeq (Word8ArraySlice.length, Word8ArraySlice.sub, word8)
    val word8Vector = hashSeq (Word8Vector.length, Word8Vector.sub, word8)
    val word8VectorSlice = hashSeq (Word8VectorSlice.length, Word8VectorSlice.sub, word8)
    val charArray = hashSeq (CharArray.length, CharArray.sub, char)
    val charArraySlice = hashSeq (CharArraySlice.length, CharArraySlice.sub, char)
    val string = hashSeq (CharVector.length, CharVector.sub, char)
    val substring = hashSeq (CharVectorSlice.length, CharVectorSlice.sub, char)
    val realArray = hashSeq (RealArray.length, RealArray.sub, real)
    val realArraySlice = hashSeq (RealArraySlice.length, RealArraySlice.sub, real)
    val realVector = hashSeq (RealVector.length, RealVector.sub, real)
    val realVectorSlice = hashSeq (RealVectorSlice.length, RealVectorSlice.sub, real)
    fun array hashFn = hashSeq (Array.length, Array.sub, hashFn)
    fun arraySlice hashFn = hashSeq (ArraySlice.length, ArraySlice.sub, hashFn)
    fun vector hashFn = hashSeq (Vector.length, Vector.sub, hashFn)
    fun vectorSlice hashFn = hashSeq (VectorSlice.length, VectorSlice.sub, hashFn)

    fun sequence {length, get, hash} = hashSeq (length, get, hash)
    end (* local *)

    fun stream {get, hash} = let
          fun lp (strm, acc : W64.word) = (case get strm
                 of NONE => acc
                  | SOME(x, strm) => lp (strm, hash(x, acc))
                (* end case *))
          in
            lp
          end

    fun option _ (NONE, acc) = acc
      | option hf (SOME x, acc) = hf(x, acc)

    fun either (hf, _) (Either.INL x, acc) = hf(x, hashByte(0w1, acc))
      | either (_, hf) (Either.INR x, acc) = hf(x, hashByte(0w2, acc))

    fun list hf = stream {get = List.getItem, hash = hf}

    (* combine hash values *)
    fun combine2 (h1, h2) = let
          fun lp (0, _, _, acc) = acc
            | lp (n, h1, h2, acc) =
                lp (n-1, shl8 h1, shl8 h2, acc +> byte h1 +> byte h2)
          in
            lp (8, h1, h2, fnvBasis)
          end

    fun combine hFn (x, acc) = combine2 (acc, hFn x)

    fun combine3 (h1, h2, h3) = let
          fun lp (0, _, _, _, acc) = acc
            | lp (n, h1, h2, h3, acc) =
                lp (n-1, shl8 h1, shl8 h2, shl8 h3,
                  acc +> byte h1 +> byte h2 +> byte h3)
          in
            lp (8, h1, h2, h3, fnvBasis)
          end

    fun combine4 (h1, h2, h3, h4) = let
          fun lp (0, _, _, _, _, acc) = acc
            | lp (n, h1, h2, h3, h4, acc) =
                lp (n-1, shl8 h1, shl8 h2, shl8 h3, shl8 h4,
                  acc +> byte h1 +> byte h2 +> byte h3 +> byte h4)
          in
            lp (8, h1, h2, h3, h4, fnvBasis)
          end

    fun combine5 (h1, h2, h3, h4, h5) = combine3(h1, h2, combine3(h3, h4, h5))

    fun combine6 (h1, h2, h3, h4, h5, h6) =
          combine4(h1, h2, h3, combine3(h4, h5, h6))

    fun pair (hf1, hf2) ((a, b), acc) =
          hf2 (b, hf1 (a, acc))
    fun triple (hf1, hf2, hf3) ((a, b, c), acc) =
          hf3 (c, hf2 (b, hf1 (a, acc)))
    fun quad (hf1, hf2, hf3, hf4) ((a, b, c, d), acc) =
          hf4 (d, hf3 (c, hf2 (b, hf1 (a, acc))))

    (* finished hash functions *)
    val bool' = finish bool
    val char' = finish char
    val int' = finish int
    val int32' = finish int32
    val int64' = finish int64
    val intInf' = finish intInf
    val word' = finish word
    val word8' = finish word8
    val word32' = finish word32
    val word64' = finish word64
    val substring' = finish substring
    val string' = finish string
    val real' = finish real
    val real64' = finish real64

    val word8Array' = finish word8Array
    val word8ArraySlice' = finish word8ArraySlice
    val word8Vector' = finish word8Vector
    val word8VectorSlice' = finish word8VectorSlice
    val charArray' = finish charArray
    val charArraySlice' = finish charArraySlice
    val realArray' = finish realArray
    val realVector' = finish realVector
    val realArraySlice' = finish realArraySlice
    val realVectorSlice' = finish realVectorSlice

  end
