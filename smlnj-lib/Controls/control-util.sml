(* control-util.sml
 *
 * COPYRIGHT (c) 2015 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ControlUtil : CONTROL_UTIL =
  struct

    structure Cvt = struct
        val int = { tyName = "int",
		    fromString = Int.fromString,
		    toString = Int.toString }
        val bool = { tyName = "bool",
		     fromString = fn s => (case String.map Char.toUpper s
			 of "FALSE" => SOME false
			  | "TRUE" => SOME true
			  | "NO" => SOME false
			  | "YES" => SOME true
			  | _ => NONE
			(* end case *)),
		     toString = Bool.toString }
        val real = { tyName = "real",
		     fromString = Real.fromString,
		     toString = Real.toString }

	val stringList = {
		tyName = "string list",
		fromString = SOME o String.fields (fn c => c = #","),
		toString = (
		  fn [] => ""
		   | [x] => x
		   | x::r => concat(x :: List.foldr (fn (y, l) => ","::y::l) [] r)
		  (* end fn *))
	      }

	val string : string Controls.value_cvt =
	    { tyName = "string",
	      fromString = SOME,
	      toString = fn x => x }
      end

    structure EnvName = struct
	fun toUpper prefix s =
	    prefix ^ String.map (fn #"-" => #"_" | c => Char.toUpper c) s
      end

  end
