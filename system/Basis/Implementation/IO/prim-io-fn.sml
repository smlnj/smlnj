(* prim-io-fn.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor PrimIO (

    structure Vector : MONO_VECTOR
    structure VectorSlice : MONO_VECTOR_SLICE
    structure Array : MONO_ARRAY
    structure ArraySlice : MONO_ARRAY_SLICE
    sharing type Vector.elem = VectorSlice.elem = Array.elem = ArraySlice.elem
    sharing type Vector.vector = VectorSlice.vector =
		 Array.vector = ArraySlice.vector
    sharing type VectorSlice.slice = ArraySlice.vector_slice
    sharing type Array.array = ArraySlice.array
    val someElem : Vector.elem
    eqtype pos
    val compare : pos * pos -> order

  ) :> PRIM_IO
        where type elem = Vector.elem
        where type vector = Vector.vector
	where type vector_slice = VectorSlice.slice
	where type array = Array.array
	where type array_slice = ArraySlice.slice
	where type pos = pos

  = struct

    structure A = Array
    structure AS = ArraySlice
    structure V = Vector
    structure VS = VectorSlice

    type elem = A.elem
    type vector = V.vector
    type vector_slice = VS.slice
    type array = A.array
    type array_slice = AS.slice

    type pos = pos

    val compare = compare

    datatype reader = RD of {
	name      : string,
	chunkSize : int,
	readVec   : (int -> vector) option,
        readArr   : (array_slice -> int) option,
	readVecNB : (int -> vector option) option,
	readArrNB : (array_slice -> int option) option,
	block     : (unit -> unit) option,
	canInput  : (unit -> bool) option,
	avail     : unit -> Position.int option,
	getPos    : (unit -> pos) option,
	setPos    : (pos -> unit) option,
        endPos    : (unit -> pos) option,
	verifyPos : (unit -> pos) option,
	close     : unit -> unit,
	ioDesc    : OS.IO.iodesc option
      }

    datatype writer = WR of {
	name       : string,
	chunkSize  : int,
	writeVec   : (vector_slice -> int) option,
	writeArr   : (array_slice -> int) option,
	writeVecNB : (vector_slice -> int option) option,
	writeArrNB : (array_slice -> int option) option,
	block      : (unit -> unit) option,
	canOutput  : (unit -> bool) option,
	getPos     : (unit -> pos) option,
	setPos     : (pos -> unit) option,
        endPos     : (unit -> pos) option,
	verifyPos  : (unit -> pos) option,
	close      : unit -> unit,
	ioDesc     : OS.IO.iodesc option
      }

    fun blockingOperation (f, block) x = (block (); Option.valOf (f x))

    fun nonblockingOperation (read, canInput) x =
	  if (canInput()) then SOME(read x) else NONE

    fun augmentReader (RD rd) = let
	  fun readaToReadv reada n = let
		val a = A.array(n, someElem)
		val n = reada (AS.full a)
		in
	          AS.vector (AS.slice (a, 0, SOME n))
		end
	  fun readaToReadvNB readaNB n = let
		val a = A.array(n, someElem)
		in
		  case readaNB (AS.full a)
		   of SOME n' => SOME(AS.vector (AS.slice(a, 0, SOME n')))
		    | NONE => NONE
		  (* end case *)
		end
	  fun readvToReada readv asl = let
		val (a, start, nelems) = AS.base asl
		val v = readv nelems
		val len = V.length v
		in
		  A.copyVec {dst=a, di=start, src=v};
		  len
		end
	  fun readvToReadaNB readvNB asl = let
		val (a, start, nelems) = AS.base asl
		in
		  case readvNB nelems
		   of SOME v => let
			val len = V.length v
		        in
			  A.copyVec {dst=a, di=start, src=v};
			  SOME len
		        end
		    | NONE => NONE
		  (* end case *)
		end
	  val readVec' = (case rd
		 of {readVec=SOME f, ...} => SOME f
		  | {readArr=SOME f, ...} => SOME(readaToReadv f)
		  | {readVecNB=SOME f, block=SOME b, ...} =>
		      SOME(blockingOperation (f, b))
		  | {readArrNB=SOME f, block=SOME b, ...} =>
		      SOME(blockingOperation (readaToReadvNB f, b))
		  | _ => NONE
		(* end case *))
	  val readArr' = (case rd
		 of {readArr=SOME f, ...} => SOME f
		  | {readVec=SOME f, ...} => SOME(readvToReada f)
		  | {readArrNB=SOME f, block=SOME b, ...} =>
		      SOME(blockingOperation(f, b))
		  | {readVecNB=SOME f,block=SOME b, ...} =>
		      SOME(blockingOperation(readvToReadaNB f, b))
		  | _ => NONE
		(* end case *))
	  val readVecNB' = (case rd
		 of {readVecNB=SOME f, ...} => SOME f
		  | {readArrNB=SOME f, ...} => SOME(readaToReadvNB f)
		  | {readVec=SOME f, canInput=SOME can, ...} =>
		      SOME(nonblockingOperation(f, can))
		  | {readArr=SOME f, canInput=SOME can, ...} =>
		      SOME(nonblockingOperation(readaToReadv f, can))
		  | _ => NONE
		(* end case *))
	  val readArrNB' = (case rd
		 of {readArrNB=SOME f, ...} => SOME f
		  | {readVecNB=SOME f, ...} => SOME(readvToReadaNB f)
		  | {readArr=SOME f, canInput=SOME can, ...} =>
		      SOME(nonblockingOperation (f, can))
		  | {readVec=SOME f, canInput=SOME can, ...} =>
		      SOME(nonblockingOperation (readvToReada f, can))
		  | _ => NONE
		(* end case *))
	  in RD{
	      name= #name rd, chunkSize= #chunkSize rd,
	      readVec=readVec', readArr=readArr',
	      readVecNB=readVecNB', readArrNB=readArrNB',
	      block= #block rd, canInput = #canInput rd, avail = #avail rd,
	      getPos = #getPos rd, setPos = #setPos rd, endPos = #endPos rd,
	      verifyPos = #verifyPos rd,
	      close= #close rd,
	      ioDesc= #ioDesc rd
	    }
	  end

    fun augmentWriter (WR wr) = let
	  fun writevToWritea writev asl = writev (VS.full (AS.vector asl))
	  fun writeaToWritev writea vsl =
	      case VS.length vsl of
		  0 => 0
		| n => let val a = A.array (n, VS.sub (vsl, 0))
		       in
			   AS.copyVec { src = VS.subslice (vsl, 1, NONE),
					dst = a, di = 1 };
			   writea (AS.full a)
		       end
	  fun writeaToWritevNB writeaNB vsl =
	      case VS.length vsl of
		  0 => SOME 0
		| n => let val a = A.array (n, VS.sub (vsl, 0))
		       in
			   AS.copyVec { src = VS.subslice (vsl, 1, NONE),
					dst = a, di = 1 };
			   writeaNB (AS.full a)
		       end
	  val writeVec' = (case wr
		 of {writeVec=SOME f, ...} => SOME f
		  | {writeArr=SOME f, ...} => SOME(writeaToWritev f)
		  | {writeVecNB=SOME f, block=SOME b, ...} =>
		      SOME(fn i => (b(); Option.valOf(f i)))
		  | {writeArrNB=SOME f, block=SOME b, ...} =>
		      SOME(fn x => (b(); writeaToWritev (Option.valOf o f) x))
		  | _ => NONE
		(* end case *))
	  val writeArr' = (case wr
		 of {writeArr=SOME f, ...} => SOME f
		  | {writeVec=SOME f, ...} => SOME(writevToWritea f)
		  | {writeArrNB=SOME f, block=SOME b, ...} => SOME(blockingOperation (f, b))
		  | {writeVecNB=SOME f,block=SOME b, ...} =>
		      SOME(blockingOperation (writevToWritea f, b))
		  | _ => NONE
		(* end case *))
	  val writeVecNB' = (case wr
		 of {writeVecNB=SOME f, ...} => SOME f
		  | {writeArrNB=SOME f, ...} => SOME(writeaToWritevNB f)
		  | {writeVec=SOME f, canOutput=SOME can, ...} =>
		      SOME(nonblockingOperation (f, can))
		  | {writeArr=SOME f, canOutput=SOME can, ...} =>
		      SOME(nonblockingOperation (writeaToWritev f, can))
		  | _ => NONE
		(* end case *))
	  val writeArrNB' = (case wr
		 of {writeArrNB=SOME f, ...} => SOME f
		  | {writeVecNB=SOME f, ...} => SOME(writevToWritea f)
		  | {writeArr=SOME f, canOutput=SOME can, ...} =>
		      SOME(nonblockingOperation (f, can))
		  | {writeVec=SOME f, canOutput=SOME can, ...} =>
		      SOME(nonblockingOperation (writevToWritea f, can))
		  | _ => NONE
		(* end case *))
	  in WR{
	      name= #name wr, chunkSize= #chunkSize wr,
	      writeVec=writeVec', writeArr=writeArr',
	      writeVecNB=writeVecNB', writeArrNB=writeArrNB',
	      block= #block wr, canOutput = #canOutput wr,
	      getPos = #getPos wr, setPos = #setPos wr, endPos = #endPos wr,
	      verifyPos = #verifyPos wr,
	      close= #close wr,
	      ioDesc= #ioDesc wr
	    }
	  end

    fun openVector v = let
	  val pos = ref 0
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  val len = V.length v
	  fun avail () = SOME(PositionImp.fromInt(len - !pos))
	  fun readV n = let
		val p = !pos
		val m = IntImp.min (n, len - p)
		in
		  checkClosed ();
		  pos := p + m;
		  VS.vector (VS.slice (v, p, SOME m))
		end
	  fun readA asl = let
		val p = !pos
		val (buf, i, n) = AS.base asl
		val m = IntImp.min (n, len - p)
		in
		  checkClosed ();
		  pos := p + m;
		  AS.copyVec { src = VS.slice (v, p, SOME m), dst = buf, di = i };
		  m
		end
	  fun checked k () = (checkClosed (); k)
	  in
	    (* random access not supported because pos type is abstract *)
	    RD{
		name = "<vector>",
		chunkSize = len,
		readVec = SOME readV,
		readArr = SOME readA,
		readVecNB = SOME (SOME o readV),
		readArrNB = SOME (SOME o readA),
		block = SOME checkClosed,
		canInput = SOME (checked true),
		avail = avail,
		getPos = NONE,
		setPos = NONE,
		endPos = NONE,
		verifyPos = NONE,
		close = fn () => closed := true,
		ioDesc = NONE
	      }
	  end

    fun nullRd () = let
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  fun checked k _ = (checkClosed (); k)
	  in
	    RD{
		name = "<null>",
		chunkSize = 1,
		readVec = SOME (checked (V.fromList [])),
		readArr = SOME (checked 0),
		readVecNB = SOME (checked (SOME (V.fromList []))),
		readArrNB = SOME (checked (SOME 0)),
		block = SOME checkClosed,
		canInput = SOME (checked true),
		avail = fn () => SOME 0,
		getPos = NONE,
		setPos = NONE,
		endPos = NONE,
		verifyPos = NONE,
		close = fn () => closed := true,
		ioDesc = NONE
	      }
	  end

    fun nullWr () = let
	  val closed = ref false
	  fun checkClosed () = if !closed then raise IO.ClosedStream else ()
	  fun checked k () = k
	  fun writeVec vsl = (checkClosed (); VS.length vsl)
	  fun writeArr asl = (checkClosed (); AS.length asl)
	  in
	    WR{
		name = "<null>",
		chunkSize = 1,
		writeVec = SOME writeVec,
		writeArr = SOME writeArr,
		writeVecNB = SOME (SOME o writeVec),
		writeArrNB = SOME (SOME o writeArr),
		block = SOME checkClosed,
		canOutput = SOME (checked true),
		getPos = NONE,
		setPos = NONE,
		endPos = NONE,
		verifyPos = NONE,
		close = fn () => closed := true,
		ioDesc = NONE
	      }
	  end

  end (* PrimIO *)
