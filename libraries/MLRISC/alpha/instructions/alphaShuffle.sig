signature ALPHASHUFFLE = sig
  structure I : ALPHAINSTR

  type t = {tmp:I.ea option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

