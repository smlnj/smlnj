(* prim-io-sig.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the CML equivalent of the SMLBL's PRIM_IO signature.  The
 * differences are that we use event-valued interfaces instead of
 * non-blocking operations, and that the operations are not optional.
 *)

signature PRIM_IO =
  sig

    type 'a event = 'a CML.event

    type array
    type vector
    type elem
    type vector_slice
    type array_slice
    eqtype pos

    val compare : (pos * pos) -> order

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
	verifyPos   : (unit -> pos) option,
	close       : unit -> unit,
	ioDesc      : OS.IO.iodesc option
      }

  end;

