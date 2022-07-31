signature HPPA_MILLICODE = sig
  structure I : HPPAINSTR
  structure CB : CELLS_BASIS =  CellsBasis

  val divu : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val mulo : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val divo : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val mulu : {rs:CB.cell, rt:CB.cell, rd:CB.cell} -> I.instruction list
  val cvti2s : {rs:CB.cell, fd:CB.cell} -> I.instruction list
  val cvti2d : {rs:CB.cell, fd:CB.cell} -> I.instruction list
  val cvti2q : {rs:CB.cell, fd:CB.cell} -> I.instruction list
end

