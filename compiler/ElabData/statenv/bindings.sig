(* bindings.sig
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature BINDINGS =
  sig

    datatype binding
      = VALbind of Variable.var
      | CONbind of Types.datacon
      | TYCbind of Types.tycon
      | SIGbind of Modules.Signature
      | STRbind of Modules.Structure
      | FSGbind of Modules.fctSig
      | FCTbind of Modules.Functor
      | FIXbind of Fixity.fixity

  (* used for statenv sorting in env/statenv.sml *)
    val binderGt : (Symbol.symbol * binding) * (Symbol.symbol * binding) -> bool

    val bindingSymbol : binding -> Symbol.symbol

  end (* signature BINDINGS *)
