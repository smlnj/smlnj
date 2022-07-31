(* sparcinstr-ext.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * a trivial extension to the Sparc instruction set.
 *)
structure SparcInstrExt = struct
    datatype ('s, 'r, 'f, 'c) sext =
	UNIMP of int
      | SAVE of ('r * 'r * 'r)
      | RESTORE of ('r * 'r * 'r)

end
