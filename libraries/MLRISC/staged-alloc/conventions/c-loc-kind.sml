structure CLocKind =
  struct

  (* kinds of locations for passing C arguments *)
    datatype loc_kind
      = GPR                (* general-purpose registers *)
      | FPR                (* floating-point registers *)
      | STK                (* stack locations *)
      | FSTK               (* floating-point stack locations *)

  end
