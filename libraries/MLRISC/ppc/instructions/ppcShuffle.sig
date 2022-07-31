signature PPCSHUFFLE = sig
  structure I : PPCINSTR

  type t = {tmp:I.ea option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end
