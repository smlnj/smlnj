(* hppaShuffle.sig -- shuffle src registers into destination registers *)

signature HPPASHUFFLE = sig
  structure I : HPPAINSTR
 
  type t = {tmp:I.ea option, dst:CellsBasis.cell list, src:CellsBasis.cell list}

  val shuffle : t -> I.instruction list
  val shufflefp : t -> I.instruction list
end

