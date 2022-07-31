(* prot-db.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure NetProtDB : NET_PROT_DB =
  struct

    fun netdbFun x = CInterface.c_function "SMLNJ-Sockets" x

    datatype entry = PROTOENT of {
	  name : string,
	  aliases : string list,
	  protocol : int
	}

    local
      fun conc field (PROTOENT a) = field a
    in
    val name = conc #name
    val aliases = conc #aliases
    val protocol = conc #protocol
    end (* local *)

  (* Protocol DB query functions *)
    local
      type protoent = (string * string list * int)
      fun getProtEnt NONE = NONE
	| getProtEnt (SOME(name, aliases, protocol)) = SOME(PROTOENT{
	      name = name, aliases = aliases, protocol = protocol
	    })
      val getProtByName' : string -> protoent option = netdbFun "getProtByName"
      val getProtByNumber' : int -> protoent option = netdbFun "getProtByNum"
    in
    val getByName = getProtEnt o getProtByName'
    val getByNumber = getProtEnt o getProtByNumber'
    end (* local *)

  end

