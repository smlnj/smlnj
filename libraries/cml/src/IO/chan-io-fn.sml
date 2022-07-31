(* chan-io-fn.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

functor ChanIOFn (
    structure PrimIO : PRIM_IO
    structure V : MONO_VECTOR
    structure VS : MONO_VECTOR_SLICE
    structure A : MONO_ARRAY
    structure AS : MONO_ARRAY_SLICE
      sharing type A.array = AS.array = PrimIO.array
      sharing type A.vector = V.vector = AS.vector = VS.vector = PrimIO.vector
      sharing type VS.slice = AS.vector_slice = PrimIO.vector_slice
      sharing type AS.slice = PrimIO.array_slice
  ) : sig

    structure PrimIO : PRIM_IO

    val mkReader : PrimIO.vector CML.chan -> PrimIO.reader
    val mkWriter : PrimIO.vector CML.chan -> PrimIO.writer

  end = struct

    structure SV = SyncVar

    structure PrimIO = PrimIO

    val vextract = VS.vector o VS.slice

  (* create a reader that is connected to the output port of a channel. *)
    fun mkReader ch = let
	  val closedFlg = SV.iVar()
	  val isClosedEvt =
		CML.wrap(SV.iGetEvt closedFlg, fn () => raise IO.ClosedStream)
	  datatype req
	    = RD of (int * unit CML.event * V.vector CML.chan)
	    | CLOSE
	  val reqCh = Mailbox.mailbox()
	  fun readVecEvt 0 = CML.alwaysEvt(V.fromList[])
	    | readVecEvt n = if (n < 0)
		then raise General.Subscript
		else CML.withNack (fn nack => let
		  val replCh = CML.channel()
		  in
		    Mailbox.send (reqCh, RD(n, nack, replCh));
		    CML.choose [
			CML.recvEvt replCh,
			isClosedEvt
		      ]
		  end)
	  fun readArrEvt asl = let
	        val (buf, i, n) = AS.base asl
		in
		  CML.wrap (readVecEvt n, fn v => (
		    A.copyVec{dst=buf, di=i, src=v};
		    V.length v))
		end
	  fun close () = Mailbox.send(reqCh, CLOSE)
	  fun getData NONE = let
		val v = CML.recv ch
		in
		  if (V.length v > 0) then v else getData NONE
		end
	    | getData (SOME v) = v
	  fun server buf = (case (Mailbox.recv reqCh)
		 of RD(n, nack, replCh) => let
		      val v = getData buf
		      in
			if (V.length v > n)
			  then let
			    val v' = vextract (v, 0, SOME n)
			    in
			      CML.select [
				  CML.wrap (nack, fn () => server(SOME v)),
				  CML.wrap (CML.sendEvt(replCh, v),
				    fn () =>
				       server(SOME(vextract(v, n, NONE))))
				]
			    end
			  else CML.select [
			      CML.wrap (nack, fn () => server(SOME v)),
			      CML.wrap (CML.sendEvt(replCh, v), fn () => server NONE)
			    ]
		      end
		  | CLOSE => (SV.iPut(closedFlg, ()); closedServer())
		(* end case *))
	  and closedServer () = (ignore(Mailbox.recv reqCh); closedServer())
	  in
	    ignore(CML.spawnc server NONE);
	    PrimIO.RD{
		name       = "<channel>",
		chunkSize  = 1024,			(* ?? *)
		readVec    = CML.sync o readVecEvt,
        	readArr    = CML.sync o readArrEvt,
		readVecEvt = readVecEvt,
		readArrEvt = readArrEvt,
		avail      = fn () => NONE,		(* ?? *)
		getPos     = NONE,
		setPos     = NONE,
        	endPos     = NONE,
		verifyPos  = NONE,
		close      = close,
		ioDesc     = NONE
	      }
	  end

  (* create a writer that is connected to the input port of a channel. *)
    fun mkWriter ch = let
	  val closedFlg = SV.iVar()
	  val closedEvt =
		CML.wrap (SV.iGetEvt closedFlg, fn () => raise IO.ClosedStream)
	  val ch' = CML.channel()
	  fun buffer () = CML.select [
		  CML.wrap (CML.recvEvt ch', fn v => (
		    if (V.length v > 0) then CML.send(ch, v) else ();
		    buffer())),
		  closedEvt
		]
	  fun writeVecEvt arg = let val v = VS.vector arg
		in
		  CML.choose [
		      closedEvt,
		      CML.wrap (CML.sendEvt (ch', v), fn () => V.length v)
		    ]
		end
	  fun writeArrEvt arg = let val v = AS.vector arg
		in
		  CML.choose [
		      closedEvt,
		      CML.wrap (CML.sendEvt (ch', v), fn () => V.length v)
		    ]
		end
	  fun close () = SV.iPut(closedFlg, ())
	  in
	    ignore(CML.spawn(fn () => ignore(buffer())));
	    PrimIO.WR{
		name        = "<channel>",
		chunkSize   = 1024,
		writeVec    = CML.sync o writeVecEvt,
		writeArr    = CML.sync o writeArrEvt,
		writeVecEvt = writeVecEvt,
		writeArrEvt = writeArrEvt,
		getPos      = NONE,
		setPos      = NONE,
        	endPos      = NONE,
		verifyPos   = NONE,
		close       = close,
		ioDesc      = NONE
	      }
	  end

  end;
