signature MLRISC_GEN =
  sig
    structure T : MLTREE
    val gen : (Label.label * T.stm list * T.mlrisc list) -> unit
  end
