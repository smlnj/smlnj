(* Copyright (c) 1998 by Lucent Technologies *)

signature SIZEOF = sig

  val warningsOn  : unit -> unit (* default *) 
  val warningsOff : unit -> unit 
  val byteSizeOf  : {sizes: Sizes.sizes, err: string -> unit,
		     warn: string -> unit, bug: string->unit}
        -> Tables.tidtab -> Ast.ctype -> {bytes:int, byteAlignment:int}

  val reset : unit -> unit
      (* reset memoization table *)

(* DBM: following not yet used? *)

  val bitSizeOf   : {sizes: Sizes.sizes, err: string -> unit,
		     warn: string -> unit, bug: string->unit}
        -> Tables.tidtab -> Ast.ctype
        -> {bits:int, bitAlignment:int}

  val fieldOffsets: {sizes: Sizes.sizes, err: string -> unit,
		     warn: string -> unit, bug: string->unit}
        -> Tables.tidtab -> Ast.ctype
        -> ({memberOpt:Ast.member option, bitOffset:int} list) option

  (* looks up a field in the list returned by fieldOffsets *)
  val getField: {sizes: Sizes.sizes, err: string -> unit,
		 warn: string -> unit, bug: string->unit}
        -> Ast.member * {memberOpt: Ast.member option, bitOffset:int} list
        -> {memberOpt: Ast.member option, bitOffset: int}
    
end (* signature SIZEOF *)
