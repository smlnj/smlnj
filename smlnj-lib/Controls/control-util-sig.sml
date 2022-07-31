(* control-util-sig.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature CONTROL_UTIL =
  sig

    structure Cvt : sig
      (* for primitive types, using respective {from,to}String functions: *)
	val int : int Controls.value_cvt
	val bool : bool Controls.value_cvt
	val real : real Controls.value_cvt

      (* comma-separated tokens *)
	val stringList : string list Controls.value_cvt

      (* for completeness' sake: *)
	val string : string Controls.value_cvt
      end

    structure EnvName : sig
      (* convert lower case to upper case and #"-" to #"_", add prefix *)
	val toUpper : string -> string -> string
      end

  end
