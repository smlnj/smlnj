(* hashing-sig.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Interface to hashing combinators; loosely based on the CCHash module.  This
 * interface is designed to support any computaion of a summary value by iterating
 * over the bytes of a value.  Applications include computing hash keys, as well
 * as CRC or SHA signatures.
 *)

signature HASHING =
  sig

    (* a `hstate` is used to hold the accumulated information *)
    type hstate
    (* a result of hashing (e.g., `word` for a hash key) *)
    type hash

    (* a hash function that accumulates an intermediate value *)
    type 'a t = 'a * hstate -> hstate

    (* a hash function that produces a hash of a value *)
    type 'a hfun = 'a -> hash

    val finish : 'a t -> 'a hfun

    (* hash functions for Basis types *)
    val bool : bool t
    val bool' : bool hfun
    val char : char t
    val char' : char hfun
    val int : int t
    val int' : int hfun
    val int32 : Int32.int t
    val int32' : Int32.int hfun
    val int64 : Int64.int t
    val int64' : Int64.int hfun
    val intInf : IntInf.int t
    val intInf' : IntInf.int hfun
    val word : word t
    val word' : word hfun
    val word8 : Word8.word t
    val word8' : Word8.word hfun
    val word32 : Word32.word t
    val word32' : Word32.word hfun
    val word64 : Word64.word t
    val word64' : Word64.word hfun
    val substring : substring t
    val substring' : substring hfun
    val string : string t
    val string' : string hfun
    val real : real t
    val real' : real hfun
    val real64 : Real64.real t
    val real64' : Real64.real hfun

    val word8Array : Word8Array.array t
    val word8Array' : Word8Array.array hfun
    val word8ArraySlice : Word8ArraySlice.slice t
    val word8ArraySlice' : Word8ArraySlice.slice hfun
    val word8Vector : Word8Vector.vector t
    val word8Vector' : Word8Vector.vector hfun
    val word8VectorSlice : Word8VectorSlice.slice t
    val word8VectorSlice' : Word8VectorSlice.slice hfun
    val charArray : CharArray.array t
    val charArray' : CharArray.array hfun
    val charArraySlice : CharArraySlice.slice t
    val charArraySlice' : CharArraySlice.slice hfun
    val realArray : RealArray.array t
    val realArray' : RealArray.array hfun
    val realArraySlice : RealArraySlice.slice t
    val realArraySlice' : RealArraySlice.slice hfun
    val realVector : RealVector.vector t
    val realVector' : RealVector.vector hfun
    val realVectorSlice : RealVectorSlice.slice t
    val realVectorSlice' : RealVectorSlice.slice hfun

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

    (* combine hash states *)
    val combine2 : hstate * hstate -> hstate
    val combine3 : hstate * hstate * hstate -> hstate
    val combine4 : hstate * hstate * hstate * hstate -> hstate
    val combine5 : hstate * hstate * hstate * hstate * hstate -> hstate
    val combine6 : hstate * hstate * hstate * hstate * hstate * hstate -> hstate

  end
