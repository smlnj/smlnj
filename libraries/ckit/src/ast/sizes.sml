(* sizes.sml *)

(* sizes.sml contains a default version of sizes;
   other versions will be available in a sizes database,
   or can be automatically generated (using sizes.c) *)

structure Sizes : SIZES =
struct
  type layout = {bits:int, align:int}
  type sizes = {char: layout,
		short: layout,
		int: layout,
		long: layout,
		longlong: layout,
		float: layout,
		double: layout,
		longdouble: layout,
		pointer: layout,
		min_struct: layout,
		min_union: layout,
		onlyPackBitFields: bool,
		ignoreUnnamedBitFieldAlignment: bool}
    
  val defaultSizes : sizes = 
    {char = {bits=8, align=8},
     short= {bits=16,align=16},
     int  = {bits=32,align=32},
     long = {bits=32,align=32},
     longlong = {bits=64,align=64},  (* default guess -- highly architecture dependent *)
     float    = {bits=32,align=32},
     double   = {bits=64,align=64},
     longdouble = {bits=64,align=64},
     pointer  = {bits=32,align=32},
     min_struct = {bits = 8, align = 8},
     min_union = {bits = 8, align = 8},
     onlyPackBitFields = false,
     ignoreUnnamedBitFieldAlignment = true}

end (* structure Sizes *)
