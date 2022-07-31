(* Copyright (c) 1998 by Lucent Technologies *)

signature PARSECONTROL =
    sig
	val symbolLength: int
	val typedefsScoped: bool
	val prototypesAllowed: bool
	val templatesAllowed: bool
        val trailingCommaInEnum : {error:bool,warning:bool}
	val newFundefsAllowed: bool
	val voidAllowed: bool
	val voidStarAllowed: bool
	val constAllowed: bool
	val volatileAllowed: bool
	val violation : string -> unit
	val Dkeywords : bool
        val parseDirective : bool
	val underscoreKeywords : bool option
	(* NONE -> accept as normal identifiers;
	 * SOME true -> accept as keywords;
	 * SOME false -> reject as error *)
    end

