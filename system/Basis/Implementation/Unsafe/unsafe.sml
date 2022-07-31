(* unsafe.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Unsafe operations on ML values.
 *)

structure Unsafe :> UNSAFE =
  struct

    structure CInterface = CInterface
    structure Object = UnsafeObject
    structure Pointer = UnsafePointer

    structure Vector =
      struct
	val sub = InlineT.PolyVector.sub
	val create = Core.Assembly.A.create_v
      end
    structure Array =
      struct
	val sub = InlineT.PolyArray.sub
	val update = InlineT.PolyArray.update
	val create = Core.Assembly.A.array
      end

    structure CharVector =
      struct
	type vector = CharVector.vector
	type elem = CharVector.elem
	val sub = InlineT.CharVector.sub
	val update = InlineT.CharVector.update
	val create = Core.Assembly.A.create_s
      end
    structure CharArray =
      struct
	type array = CharArray.array
	type elem = CharArray.elem
	val sub = InlineT.CharArray.sub
	val update = InlineT.CharArray.update
	val create : int -> array = InlineT.cast Core.Assembly.A.create_b
      end

    structure Word8Vector =
      struct
	type vector = Word8Vector.vector
	type elem = Word8Vector.elem
	val sub = InlineT.Word8Vector.sub
	val update = InlineT.Word8Vector.update
	val create : int -> vector = InlineT.cast Core.Assembly.A.create_s
      end
    structure Word8Array =
      struct
	type array = Word8Array.array
	type elem = Word8Array.elem
	val sub = InlineT.Word8Array.sub
	val update = InlineT.Word8Array.update
	val create = Core.Assembly.A.create_b
      end

(** once we have flat real vectors, we can include this substructure
    structure Real64Vector =
      struct
	type vector = Real64Vector.vector
	type elem = Real64Vector.elem
	val sub : (vector * int) -> elem
	val update : (vector * int * elem) -> unit
	val create : int -> vector
      end
**)
    structure Real64Array =
      struct
	type array = Real64Array.array
	type elem = Real64Array.elem
	val sub = InlineT.Real64Array.sub
	val update = InlineT.Real64Array.update
	val create = Core.Assembly.A.create_r
      end

  (* unsafe word packing *)
    structure PackWord16Big = UnsafePackWord16Big
    structure PackWord16Little = UnsafePackWord16Little
    structure PackWord32Big = UnsafePackWord32Big
    structure PackWord32Little = UnsafePackWord32Little
(* TODO: add 64-bit structures *)

  (* access to internal representation of the IntInf.int type *)
    structure IntInf =
      struct
	datatype rep = datatype CoreIntInf.rep
	val concrete = CoreIntInf.concrete
	val abstract = CoreIntInf.abstract
	val baseBits = WordImp.toIntX CoreIntInf.baseBits
      end

  (* convert real to bits (experimental) *)
    val realToBits = InlineT.Real64.toBits
  (* assembly-code function for scaling reals *)
    val scalb = Core.Assembly.A.scalb

    val getVar = InlineT.getvar
    val setVar = InlineT.setvar

    val getHdlr = InlineT.gethdlr
    val setHdlr = InlineT.sethdlr

    val blastRead : Word8Vector.vector -> 'a =
	(fn x => CInterface.c_function "SMLNJ-RunT" "blastIn" x)
    val blastWrite : 'a -> Word8Vector.vector =
	(fn x => CInterface.c_function "SMLNJ-RunT" "blastOut" x)

    val boxed = InlineT.boxed

    val cast = InlineT.cast

    (* actual representation of pStruct *)
    datatype runDynEnv
      = NILrde
      | CONSrde of Word8Vector.vector * Object.object * runDynEnv

    val pStruct : runDynEnv ref = InlineT.cast Assembly.pstruct

    val topLevelCont = ref(InlineT.isolate (fn () => ()))

    val sigHandler = Assembly.sighandler

  end;
