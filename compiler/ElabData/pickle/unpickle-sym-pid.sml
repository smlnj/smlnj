(* unpickle-sym-pid.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure UnpickleSymPid :>
  sig
    val r_symbol : UnpickleUtil.session * string UnpickleUtil.reader ->
	Symbol.symbol UnpickleUtil.reader
    val r_pid : UnpickleUtil.session * string UnpickleUtil.reader ->
	PersStamps.persstamp UnpickleUtil.reader
  end =

struct

  fun r_symbol (session, r_string) = let
      val m = UnpickleUtil.mkMap ()
      fun s () = let
          fun sym con = con (r_string ())
          fun rs  #"a" = sym Symbol.varSymbol
            | rs #"b" = sym Symbol.tycSymbol
            | rs #"c" = sym Symbol.sigSymbol
            | rs #"d" = sym Symbol.strSymbol
            | rs #"e" = sym Symbol.fctSymbol
            | rs #"f" = sym Symbol.fsigSymbol
            | rs #"g" = sym Symbol.fixSymbol
            | rs #"h" = sym Symbol.labSymbol
            | rs #"i" = sym Symbol.tyvSymbol
            | rs _ = raise UnpickleUtil.Format
      in
          UnpickleUtil.share session m rs
      end
  in
      s
  end

  fun r_pid (session, r_string) = let
      val m = UnpickleUtil.mkMap ()
      fun p () = let
          fun rp #"p" =
              PersStamps.fromBytes (Byte.stringToBytes (r_string ()))
            | rp _ = raise UnpickleUtil.Format
      in
          UnpickleUtil.share session m rp
      end
  in
      p
  end

end (* structure UnpickleSymPid *)
