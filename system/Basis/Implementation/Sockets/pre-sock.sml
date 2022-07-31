(* pre-sock.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * These are some common type definitions used in the sockets library.  This
 * structure is called Sock, so that the signatures will compile.
 *)

local
    structure SysWord = SysWordImp
    structure Word8 = Word8Imp
    structure Word = WordImp
in
structure Socket = struct

  (* the raw representation address data *)
    type addr = Word8Vector.vector

  (* the raw representation of an address family *)
    type af = CInterface.system_const

  (* the raw representation of a socket:
   *   a file descriptor for now and a mutable flag indicating
   *   (with a value of true) if the socket is currently set
   *   to non-blocking *)
    type sockFD = int
    type socket = { fd: sockFD, nb: bool ref }

  (* sockets are polymorphic; the instantiation of the type variables
   * provides a way to distinguish between different kinds of sockets.
   *)
    datatype ('sock, 'af) sock = SOCK of socket
    datatype 'af sock_addr = ADDR of addr

  (* witness types for the socket parameter *)
    datatype dgram = DGRAM
    datatype 'a stream = STREAM
    datatype passive = PASSIVE
    datatype active = ACTIVE

    structure AF = struct
        datatype addr_family = AF of af
      end

    structure SOCK = struct
        (* socket types *)
        datatype sock_type = SOCKTY of CInterface.system_const
      end

    datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
    type sock_desc = OS.IO.iodesc

  (* Sock I/O option types *)
    type out_flags = {don't_route : bool, oob : bool}
    type in_flags = {peek : bool, oob : bool}

  (** Utility functions for parsing/unparsing network addresses **)
    local
      structure SysW = SysWord
      structure SCvt = StringCvt
      fun toW (getc, strm) = let
	    fun scan radix strm = (case (SysW.scan radix getc strm)
		   of NONE => NONE
		    | (SOME(w, strm)) => SOME(w, strm)
		  (* end case *))
	    in
	      case (getc strm)
	       of NONE => NONE
		| (SOME(#"0", strm')) => (case (getc strm')
		     of NONE => SOME(0w0, strm')
		      | (SOME((#"x" | #"X"), strm'')) => scan SCvt.HEX strm''
		      | _ => scan SCvt.OCT strm
		    (* end case *))
		| _ => scan SCvt.DEC strm
	      (* end case *)
	    end
  (* check that the word is representable in the given number of bits; raise
   * Overflow if not.
   *)
    fun chk (w, bits) =
	  if (SysW.>= (SysW.>>(0wxffffffff, Word.-(0w32, bits)), w))
	    then w
	    else raise General.Overflow
  (* Scan a sequence of numbers separated by #"." *)
    fun scan getc strm = (case toW (getc, strm)
	   of NONE => NONE
	    | SOME(w, strm') => scanRest getc ([w], strm')
	  (* end case *))
    and scanRest getc (l, strm) = (case getc strm
	   of SOME(#".", strm') => (case toW (getc, strm')
		 of NONE => SOME(List.rev l, strm)
		  | SOME(w, strm'') => scanRest getc (w::l, strm'')
		(* end case *))
	    | _ => SOME(List.rev l, strm)
	  (* end case *))
    in
    fun toWords getc strm = (case (scan getc strm)
	   of SOME([a, b, c, d], strm) =>
		SOME([chk(a, 0w8), chk(b, 0w8), chk(c, 0w8), chk(d, 0w8)], strm)
	    | SOME([a, b, c], strm) =>
		SOME([chk(a, 0w8), chk(b, 0w8), chk(c, 0w16)], strm)
	    | SOME([a, b], strm) =>
		SOME([chk(a, 0w8), chk(b, 0w24)], strm)
	    | SOME([a], strm) =>
		SOME([chk(a, 0w32)], strm)
	    | _ => NONE
	  (* end case *))
    fun fromBytes (a, b, c, d) = let
	  val fmt = Word8.fmt StringCvt.DEC
	  in
	    concat [fmt a, ".", fmt b, ".", fmt c, ".", fmt d]
	  end
    end

  end (* PreSock *)
end
