(* Copyright (c) 1998 by Lucent Technologies *)

signature SIZES =
  sig
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
  val defaultSizes: sizes
end
