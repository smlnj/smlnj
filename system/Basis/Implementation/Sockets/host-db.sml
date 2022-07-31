(* host-db.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

local
    structure Word8 = Word8Imp
in
structure NetHostDBInternal :> sig
    (* export extra element for internal use by Basis implementation *)
    include NET_HOST_DB
    val INADDR : Socket.addr -> in_addr
    val unINADDR : in_addr -> Socket.addr
end
  where type addr_family = Socket.AF.addr_family
= struct

    structure SysW = SysWordImp

    fun netdbFun x = CInterface.c_function "SMLNJ-Sockets" x

    datatype in_addr = INADDR of Socket.addr
    type addr_family = Socket.AF.addr_family

    fun unINADDR (INADDR a) = a

    datatype entry = HOSTENT of {
	  name : string,
	  aliases : string list,
	  addrType : addr_family,
	  addrs : in_addr list
	}

    local
      fun conc field (HOSTENT a) = field a
    in
    val name = conc #name
    val aliases = conc #aliases
    val addrType = conc #addrType
    val addrs = conc #addrs
    val addr = List.hd o addrs
    end (* local *)

  (* Host DB query functions *)
    local
      type hostent = (string * string list * Socket.af * Socket.addr list)
      fun getHostEnt NONE = NONE
	| getHostEnt (SOME(name, aliases, addrType, addrs)) = SOME(HOSTENT{
	      name = name, aliases = aliases,
	      addrType = Socket.AF.AF addrType,
	      addrs = List.map INADDR addrs
	    })
      val getHostByName' : string -> hostent option = netdbFun "getHostByName"
      val getHostByAddr' : Socket.addr -> hostent option = netdbFun "getHostByAddr"
    in
    val getByName = getHostEnt o getHostByName'
    fun getByAddr (INADDR addr) = getHostEnt(getHostByAddr' addr)
    end (* local *)

    fun scan getc strm = let
	  fun w2b w = Word8.fromLargeWord(SysW.toLargeWord w)
	  fun getB (w, shft) = SysW.andb(SysW.>>(w, shft), 0wxFF)
	  fun mkAddr (a, b, c, d) = INADDR(Word8Vector.fromList[
		  w2b a, w2b b, w2b c, w2b d
		])
	  in
	    case (Socket.toWords getc strm)
	     of SOME([a, b, c, d], strm) =>
		  SOME(mkAddr(a, b, c, d), strm)
	      | SOME([a, b, c], strm) =>
		  SOME(mkAddr(a, b, getB(c, 0w8), getB(c, 0w0)), strm)
	      | SOME([a, b], strm) =>
		  SOME(mkAddr(a, getB(b, 0w16), getB(b, 0w8), getB(b, 0w0)), strm)
	      | SOME([a], strm) =>
		  SOME(mkAddr(getB(a, 0w24), getB(a, 0w16), getB(a, 0w8), getB(a, 0w0)), strm)
	      | _ => NONE
	    (* end case *)
	  end

    val fromString = StringCvt.scanString scan

    fun toString (INADDR addr) = let
	  fun get i = Word8Vector.sub(addr, i)
	  in
	    Socket.fromBytes(get 0, get 1, get 2, get 3)
	  end

    val getHostName : unit -> string = netdbFun "getHostName"

  end

(* restrict to NET_HOST_DB *)
structure NetHostDB : NET_HOST_DB = NetHostDBInternal

end
