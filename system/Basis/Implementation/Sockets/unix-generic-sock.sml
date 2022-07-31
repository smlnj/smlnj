(* generic-sock.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure GenericSock : GENERIC_SOCK =
  struct
    structure S = Socket

    fun sockFn x = CInterface.c_function "SMLNJ-Sockets" x

    val c_socket	: (int * int * int) -> S.sockFD
	  = sockFn "socket"
    val c_socketPair	: (int * int * int) -> (S.sockFD * S.sockFD)
	  = sockFn "socketPair"

    fun fd2sock fd = S.SOCK { fd = fd, nb = ref false }

  (* create sockets using default protocol *)
    fun socket (S.AF.AF(af, _), S.SOCK.SOCKTY(ty, _)) =
	  fd2sock (c_socket (af, ty, 0))
    fun socketPair (S.AF.AF(af, _), S.SOCK.SOCKTY(ty, _)) = let
	  val (s1, s2) = c_socketPair (af, ty, 0)
	  in
	    (fd2sock s1, fd2sock s2)
	  end

  (* create sockets using the specified protocol *)
    fun socket' (S.AF.AF(af, _), S.SOCK.SOCKTY(ty, _), prot) =
	  fd2sock (c_socket (af, ty, prot))
    fun socketPair' (S.AF.AF(af, _), S.SOCK.SOCKTY(ty, _), prot) = let
	  val (s1, s2) = c_socketPair (af, ty, prot)
	  in
	    (fd2sock s1, fd2sock s2)
	  end

  end
