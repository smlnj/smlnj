(* unsafe.sig
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Unsafe operations on ML values.
 *)

signature UNSAFE =
  sig

    structure CInterface : CINTERFACE
    structure Object : UNSAFE_OBJECT
    structure Pointer : UNSAFE_POINTER

    structure Vector : UNSAFE_VECTOR
    structure Array  : UNSAFE_ARRAY

    structure CharVector : UNSAFE_MONO_VECTOR
      where type vector = CharVector.vector
      where type elem = CharVector.elem
    structure CharArray : UNSAFE_MONO_ARRAY
      where type array = CharArray.array
      where type elem = CharArray.elem

    structure Word8Vector : UNSAFE_MONO_VECTOR
      where type vector = Word8Vector.vector
      where type elem = Word8Vector.elem
    structure Word8Array : UNSAFE_MONO_ARRAY
      where type array = Word8Array.array
      where type elem = Word8Array.elem

(** once we have flat real vectors, we can include this substructure
    structure Real64Vector : UNSAFE_MONO_VECTOR
      where type vector = Real64Vector.vector
      where type elem = Real64Vector.elem
**)
    structure Real64Array : UNSAFE_MONO_ARRAY
      where type array = Real64Array.array
      where type elem = Real64Array.elem

  (* unsafe word packing *)
    structure PackWord16Big : PACK_WORD
    structure PackWord16Little : PACK_WORD
    structure PackWord32Big : PACK_WORD
    structure PackWord32Little : PACK_WORD
(* TODO: add 64-bit structures *)

  (* access to internal representation of the IntInf.int type *)
    structure IntInf : sig
        datatype rep = BI of { negative : bool, digits : word list }
	val concrete : IntInf.int -> rep
	val abstract : rep -> IntInf.int
      (* number of bits per digit *)
	val baseBits : int
      end

  (* convert real to bits (experimental) *)
    val realToBits : real -> Word64.word
  (* assembly-code function for scaling reals *)
    val scalb : real * int -> real

    val getHdlr : unit -> 'a Cont.cont
    val setHdlr : 'a Cont.cont -> unit

    val getVar : unit -> 'a
    val setVar : 'a -> unit

    val blastRead : Word8Vector.vector -> 'a
    val blastWrite : 'a -> Word8Vector.vector

    val boxed : 'a -> bool

    val cast : 'a -> 'b

    datatype runDynEnv
      = NILrde
      | CONSrde of Word8Vector.vector * Object.object * runDynEnv

    val pStruct : runDynEnv ref

    val topLevelCont : unit Cont.cont ref

    val sigHandler : ((int * int * unit Cont.cont) -> unit Cont.cont) ref

  end;
