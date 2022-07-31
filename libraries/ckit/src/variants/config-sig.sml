signature CONFIG =
sig
  val DFLAG : bool

  structure ParseControl : PARSECONTROL
  structure TypeCheckControl : TYPECHECKCONTROL
end