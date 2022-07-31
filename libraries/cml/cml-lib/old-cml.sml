(* old-cml.sml
 *
 * COPYRIGHT (c) 1990 by John H. Reppy.  See COPYRIGHT file for details.
 *
 * This is essentially the 0.9.8 version of the core CML interface.  The only
 * thing missing is poll and the low-level I/O synchronization.
 *)

structure OldCML : OLD_CML =
  struct

    structure NewCML = CML

    val version = let
	  val (major, minor, rev) = (case (#version_id CML.version)
		 of (a::b::c::_) => (a, b, c)
		  | [a, b] => (a, b, 0)
		  | [a] => (a, 0, 0)
		(* end case *))
	  in
	    {major = major, minor = minor, rev = rev, date = #date CML.version}
	  end
    val versionName = CML.banner

  (** events **)
    type 'a event = 'a CML.event

    val sync   = CML.sync
    val select = CML.select

    val choose = CML.choose

    val guard = CML.guard

    val wrap        = CML.wrap
    val wrapHandler = CML.wrapHandler

    fun wrapAbort (evt, abortAct) = CML.withNack (fn abortEvt => let
	    fun abortAct' () = (sync abortEvt; abortAct())
	    in
	      CML.spawn abortAct'; evt
	    end

    val always = CML.always
    val ALWAYS = always()

  (** threads **)
    type thread_id = CML.thread_id

    val spawn = CML.spawn

    val yield = CML.yield
    val exit = CML.exit

    val getTid = CML.getTid
    val sameThread = CML.sameTid
    val tidLessThan (tid1, tid2) = (case CML.compareTid(tid1, tid2)
	   of LESS => true
	    | _ => false
	  (* end case *))
    val tidToString = CML.tidToString

    val threadWait = CML.joinEvt

  (** condition variables **)
    type 'a cond_var = 'a SyncVar.ivar

    val condVar = SyncVar.iVar

    val writeVar = SyncVar.iPut
    exception WriteTwice = SyncVar.Put

    val readVar = SyncVar.iGet
    val readVarEvt = SyncVar.iGetEvt

  (** channels **)
    type 'a chan = 'a CML.chan

    val channel = CML.channel

    val send   = CML.send
    fun sendc ch msg = CML.send(ch, msg)
    val accept = CML.recv

    val sameChannel = CML.sameChannel

    val transmit  = CML.sendEvt
    fun transmitc ch msg = CML.sendEvt(ch, msg)
    val receive   = CML.recvEvt

  (** real-time synchronization **)
    val waitUntil = CML.atTimeEvt
    val timeout   = CML.timeOutEvt

  end (* structure OldCML *)
