(* net-db.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

local
  structure SysWord = SysWordImp
  structure Word8 = Word8Imp
in
structure NetDB : NET_DB =
  struct

    structure SysW = SysWord

    fun netdbFun x = CInterface.c_function "SMLNJ-Sockets" x

    datatype net_addr = NETADDR of SysW.word

    type addr_family = Socket.AF.addr_family

    datatype entry = NETENT of {
	name : string,
	aliases : string list,
	addrType : addr_family,
	addr : net_addr
      }

    local
      fun conc field (NETENT a) = field a
    in
    val name = conc #name
    val aliases = conc #aliases
    val addrType = conc #addrType
    val addr = conc #addr
    end (* local *)

  (* Network DB query functions *)
    local
      type netent = (string * string list * Socket.af * SysWord.word)

      fun getNetEnt NONE = NONE
	| getNetEnt (SOME(name, aliases, addrType, addr)) = SOME(NETENT{
	      name = name, aliases = aliases,
	      addrType = Socket.AF.AF addrType, addr = NETADDR addr
	    })
      val getNetByName' : string -> netent option
	    = netdbFun "getNetByName"
      val getNetByAddr' : (SysWord.word * Socket.af) -> netent option
	    = netdbFun "getNetByAddr"
    in
    val getByName = getNetEnt o getNetByName'
    fun getByAddr (NETADDR addr, Socket.AF.AF af) =
	  getNetEnt(getNetByAddr'(addr, af))
    end (* local *)

    fun scan getc strm = let
	  val (op +) = SysW.+
	  in
	    case (Socket.toWords getc strm)
	     of SOME([a, b, c, d], strm) =>
		  SOME(
		    NETADDR(SysW.<<(a, 0w24)+SysW.<<(b, 0w16)+SysW.<<(c, 0w8)+d),
		    strm)
	      | SOME([a, b, c], strm) =>
		  SOME(NETADDR(SysW.<<(a, 0w24)+SysW.<<(b, 0w16)+c), strm)
	      | SOME([a, b], strm) =>
		  SOME(NETADDR(SysW.<<(a, 0w24)+b), strm)
	      | SOME([a], strm) => SOME(NETADDR a, strm)
	      | _ => NONE
	    (* end case *)
	  end

    val fromString = StringCvt.scanString scan

    fun toString (NETADDR addr) = let
	  fun get n = Word8.fromLargeWord(SysW.toLargeWord((SysW.>>(addr, n))))
	  in
	    Socket.fromBytes (get 0w24, get 0w16, get 0w8, get 0w0)
	  end

  end

end (* local *)
