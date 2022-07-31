(* prim-io-fn.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor PrimIO (

    structure Vector : MONO_VECTOR
    structure Array : MONO_ARRAY
    structure VectorSlice : MONO_VECTOR_SLICE
    structure ArraySlice : MONO_ARRAY_SLICE
      sharing type Vector.vector = Array.vector =
		   VectorSlice.vector = ArraySlice.vector
      sharing type Vector.elem = Array.elem =
		   VectorSlice.elem = ArraySlice.elem
      sharing type ArraySlice.vector_slice = VectorSlice.slice
    val someElem : Vector.elem
    eqtype pos
    val compare : (pos * pos) -> order

  ) : PRIM_IO = struct

    type 'a event = 'a CML.event

    structure A = Array
    structure V = Vector

    type elem = A.elem
    type vector = V.vector
    type array = A.array
    type array_slice = ArraySlice.slice
    type vector_slice = VectorSlice.slice
    type pos = pos

    val compare = compare

    datatype reader = RD of {
	name       : string,
	chunkSize  : int,
	readVec    : int -> vector,
        readArr    : array_slice -> int,
	readVecEvt : int -> vector event,
	readArrEvt : array_slice -> int event,
	avail      : unit -> Position.int option,
	getPos     : (unit -> pos) option,
	setPos     : (pos -> unit) option,
        endPos     : (unit -> pos) option,
	verifyPos  : (unit -> pos) option,
	close      : unit -> unit,
	ioDesc     : OS.IO.iodesc option
      }

    datatype writer = WR of {
	name        : string,
	chunkSize   : int,
	writeVec    : vector_slice -> int,
	writeArr    : array_slice -> int,
	writeVecEvt : vector_slice -> int event,
	writeArrEvt : array_slice -> int event,
	getPos      : (unit -> pos) option,
	setPos      : (pos -> unit) option,
        endPos      : (unit -> pos) option,
	verifyPos  : (unit -> pos) option,
	close       : unit -> unit,
	ioDesc      : OS.IO.iodesc option
      }

  end (* PrimIO *)
