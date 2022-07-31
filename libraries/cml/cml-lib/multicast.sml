(* multicast.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Asynchronous multicast (one-to-many) channels.  This implementation
 * is based on a condition variable implementation of multicast channels.
 * See Chapter 5 of "Concurrent Programming in ML" for details.
 *)

structure Multicast : MULTICAST =
  struct

    structure V = SyncVar

    type 'a event = 'a CML.event

    datatype 'a mchan = MChan of ('a request CML.chan * 'a port CML.chan)

    and 'a port
      = Port of (('a * 'a mc_state V.ivar) CML.chan * 'a mc_state V.ivar V.mvar)

    and 'a request
      = Message of 'a
      | NewPort

    and 'a mc_state = MCState of ('a * 'a mc_state V.ivar)

    fun mkPort cv = let
	  val outCh = CML.channel()
	  val stateVar = V.mVarInit cv
	  fun tee cv = let
		val (MCState(v, nextCV)) = V.iGet cv
		in
		  CML.send (outCh, (v, nextCV));
		  tee nextCV
		end
	  in
	    CML.spawn (fn () => tee cv);
	    Port(outCh, stateVar)
	  end

    fun mChannel () = let
          val reqCh = CML.channel() and replyCh = CML.channel()
          fun server cv = (case (CML.recv reqCh)
		 of NewPort => (
		      CML.send (replyCh, mkPort cv);
		      server cv)
		  | (Message m) => let
		      val nextCV = V.iVar()
		      in
			V.iPut (cv, MCState(m, nextCV));
			server nextCV
		      end
		(* end case *))
          in
            CML.spawn (fn () => server (V.iVar()));
            MChan(reqCh, replyCh)
          end

    fun multicast (MChan(ch, _), m) = CML.send (ch, Message m)

    fun port (MChan(reqCh, replyCh)) = (
          CML.send (reqCh, NewPort);
          CML.recv replyCh)

    fun copy (Port(_, stateV)) = mkPort(V.mGet stateV)

    fun recvMsg stateV (v, nextCV) = (V.mSwap (stateV, nextCV); v)

    fun recv (Port(ch, stateV)) = recvMsg stateV (CML.recv ch)
    fun recvEvt (Port(ch, stateV)) = CML.wrap(CML.recvEvt ch, recvMsg stateV)

  end (* Multicast *)

