(* socket.sig
 *
 * COPYRIGHT (c) 2006 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *)

local
    structure OS = OSImp
in

(* We start with a version of this signature that does not contain
 * any of the non-blocking operations: *)
signature SYNCHRONOUS_SOCKET =
  sig

  (* sockets are polymorphic; the instantiation of the type variables
   * provides a way to distinguish between different kinds of sockets.
   *)
    type ('af, 'sock_type) sock
    type 'af sock_addr

  (* witness types for the socket parameter *)
    type dgram
    type 'mode stream
    type passive	(* for passive streams *)
    type active		(* for active (connected) streams *)

  (* address families *)
    structure AF : sig
	type addr_family = NetHostDB.addr_family
	val list : unit -> (string * addr_family) list
	    (* list known address families *)
        val toString : addr_family -> string
	val fromString : string -> addr_family option
      end

  (* socket types *)
    structure SOCK : sig
	eqtype sock_type
	val stream : sock_type		(* stream sockets *)
	val dgram : sock_type		(* datagram sockets *)
	val list : unit -> (string * sock_type) list
	    (* list known socket types *)
	val toString : sock_type -> string
	val fromString : string -> sock_type option
      end

  (* socket control operations *)
    structure Ctl : sig

      (* get/set socket options *)
        val getDEBUG		: ('af, 'sock_type) sock -> bool
        val setDEBUG		: ('af, 'sock_type) sock * bool -> unit
        val getREUSEADDR	: ('af, 'sock_type) sock -> bool
        val setREUSEADDR	: ('af, 'sock_type) sock * bool -> unit
        val getKEEPALIVE	: ('af, 'sock_type) sock -> bool
        val setKEEPALIVE	: ('af, 'sock_type) sock * bool -> unit
        val getDONTROUTE	: ('af, 'sock_type) sock -> bool
        val setDONTROUTE	: ('af, 'sock_type) sock * bool -> unit
        val getLINGER		: ('af, 'sock_type) sock
				     -> Time.time option
        val setLINGER		: ('af, 'sock_type) sock * Time.time option
				    -> unit
        val getBROADCAST	: ('af, 'sock_type) sock -> bool
        val setBROADCAST	: ('af, 'sock_type) sock * bool -> unit
        val getOOBINLINE	: ('af, 'sock_type) sock -> bool
        val setOOBINLINE	: ('af, 'sock_type) sock * bool -> unit
        val getSNDBUF		: ('af, 'sock_type) sock -> int
        val setSNDBUF		: ('af, 'sock_type) sock * int -> unit
        val getRCVBUF		: ('af, 'sock_type) sock -> int
        val setRCVBUF		: ('af, 'sock_type) sock * int -> unit
        val getTYPE		: ('af, 'sock_type) sock -> SOCK.sock_type
        val getERROR		: ('af, 'sock_type) sock -> bool

	val getPeerName		: ('af, 'sock_type) sock -> 'af sock_addr
	val getSockName		: ('af, 'sock_type) sock -> 'af sock_addr
	val getNREAD		: ('af, 'sock_type) sock -> int
	val getATMARK		: ('af, active stream) sock -> bool
      end (* Ctl *)

  (* socket address operations *)
    val sameAddr     : 'af sock_addr * 'af sock_addr -> bool
    val familyOfAddr : 'af sock_addr -> AF.addr_family

  (* socket management *)
    val bind      : ('af, 'sock_type) sock * 'af sock_addr -> unit
    val listen    : ('af, passive stream) sock * int -> unit
    val accept    : ('af, passive stream) sock
		    -> ('af, active stream) sock * 'af sock_addr
    val connect   : ('af, 'sock_type) sock * 'af sock_addr -> unit
    val close     : ('af, 'sock_type) sock -> unit

    datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
    val shutdown : ('af, 'mode stream) sock * shutdown_mode -> unit

    type sock_desc
    val sockDesc : ('af, 'sock_type) sock -> sock_desc
    val sameDesc : sock_desc * sock_desc -> bool

    val select : {
	    rds : sock_desc list,
	    wrs : sock_desc list,
	    exs : sock_desc list,
	    timeout : Time.time option
	  } -> {
	    rds : sock_desc list,
	    wrs : sock_desc list,
	    exs : sock_desc list
	  }

    val ioDesc : ('af, 'sock_type) sock -> OS.IO.iodesc

  (* Sock I/O option types *)
    type out_flags = {don't_route : bool, oob : bool}
    type in_flags = {peek : bool, oob : bool}

  (* Sock output operations *)
    val sendVec	   : ('af, active stream) sock * Word8VectorSlice.slice
		        -> int
    val sendArr	   : ('a, active stream) sock * Word8ArraySlice.slice
			-> int
    val sendVec'   : ('a, active stream) sock * Word8VectorSlice.slice * out_flags
			-> int
    val sendArr'   : ('a, active stream) sock * Word8ArraySlice.slice * out_flags
			-> int

    val sendVecTo  : ('a, dgram) sock * 'a sock_addr * Word8VectorSlice.slice
			-> unit
    val sendArrTo  : ('a, dgram) sock * 'a sock_addr * Word8ArraySlice.slice
			-> unit
    val sendVecTo' : ('a, dgram) sock * 'a sock_addr * Word8VectorSlice.slice * out_flags
			-> unit
    val sendArrTo' : ('a, dgram) sock * 'a sock_addr * Word8ArraySlice.slice * out_flags
			-> unit

  (* Sock input operations *)
    val recvVec      : ('a, active stream) sock * int
			-> Word8Vector.vector
    val recvArr	     : ('a, active stream) sock * Word8ArraySlice.slice
			-> int
    val recvVec'     : ('a, active stream) sock * int * in_flags
			-> Word8Vector.vector
    val recvArr'     : ('a, active stream) sock * Word8ArraySlice.slice * in_flags
			-> int

    val recvVecFrom  : ('a, dgram) sock * int
			-> Word8Vector.vector * 'a sock_addr
    val recvArrFrom  : ('a, dgram) sock * Word8ArraySlice.slice
			-> int * 'a sock_addr
    val recvVecFrom' : ('a, dgram) sock * int * in_flags
			-> Word8Vector.vector * 'a sock_addr
    val recvArrFrom' : ('a, dgram) sock * Word8ArraySlice.slice * in_flags
			-> int * 'a sock_addr
  end

(* add non-blocking ops: *)
signature SOCKET =
  sig
    include SYNCHRONOUS_SOCKET

    val acceptNB  : ('af, passive stream) sock
		    -> (('af, active stream) sock * 'af sock_addr) option
    val connectNB : ('af, 'sock_type) sock * 'af sock_addr -> bool

    val sendVecNB  : ('af, active stream) sock * Word8VectorSlice.slice
		       -> int option
    val sendArrNB  : ('a, active stream) sock * Word8ArraySlice.slice
			-> int option
    val sendVecNB' : ('a, active stream) sock * Word8VectorSlice.slice * out_flags
			-> int option
    val sendArrNB' : ('a, active stream) sock * Word8ArraySlice.slice * out_flags
			-> int option

    val sendVecToNB: ('a, dgram) sock * 'a sock_addr * Word8VectorSlice.slice
			-> bool
    val sendArrToNB: ('a, dgram) sock * 'a sock_addr * Word8ArraySlice.slice
			-> bool
    val sendVecToNB':('a, dgram) sock * 'a sock_addr * Word8VectorSlice.slice * out_flags
			-> bool
    val sendArrToNB':('a, dgram) sock * 'a sock_addr * Word8ArraySlice.slice * out_flags
			-> bool

    val recvVecNB    : ('a, active stream) sock * int
			-> Word8Vector.vector option
    val recvArrNB    : ('a, active stream) sock * Word8ArraySlice.slice
			-> int option
    val recvVecNB'   : ('a, active stream) sock * int * in_flags
			-> Word8Vector.vector option
    val recvArrNB'   : ('a, active stream) sock * Word8ArraySlice.slice * in_flags
			-> int option

    val recvVecFromNB: ('a, dgram) sock * int
			-> (Word8Vector.vector * 'a sock_addr) option
    val recvArrFromNB: ('a, dgram) sock * Word8ArraySlice.slice
			-> (int * 'a sock_addr) option
    val recvVecFromNB':('a, dgram) sock * int * in_flags
			-> (Word8Vector.vector * 'a sock_addr) option
    val recvArrFromNB':('a, dgram) sock * Word8ArraySlice.slice * in_flags
			-> (int * 'a sock_addr) option
  end
end
