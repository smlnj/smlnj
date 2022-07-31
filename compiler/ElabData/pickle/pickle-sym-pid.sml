(* pickle-sym-pid.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure PickleSymPid :>
  sig
    val w_symbol : ('ahm, Symbol.symbol) PickleUtil.pickler
    val w_pid : ('ahm, PersStamps.persstamp) PickleUtil.pickler
  end =

struct

  structure PU = PickleUtil

  local
      val S = ~100	    (* dangerous! coordinate with PickleUtil! *)
  in
      fun w_symbol s = let
          val $ = PU.$ S
          infix $
          val ns =
              case Symbol.nameSpace s of
                  Symbol.VALspace => "a"
                | Symbol.TYCspace => "b"
                | Symbol.SIGspace => "c"
                | Symbol.STRspace => "d"
                | Symbol.FCTspace => "e"
                | Symbol.FSIGspace => "f"
                | Symbol.FIXspace => "g"
                | Symbol.LABspace => "h"
                | Symbol.TYVspace => "i"
      in
          ns $ [PU.w_string (Symbol.name s)]
      end
  end

  local
      val P = ~101
  in
      fun w_pid p = let
          val $ = PU.$ P
          infix $
      in
          "p" $ [PU.w_string (Byte.bytesToString (PersStamps.toBytes p))]
      end
  end

end (* structure PickleSymPid *)
