(* smlnj-lib/PrettyPrint/examples/expdecl.sml, for PrettyPrint, Version 8.3 *)

(* (JHR, 2022-0606)

Let's imagine the following type

datatype exp
= Let of dcl list * exp list
| ...

and dcl
= Val of string * exp
| ...

How would you format expressions so that you could get the following renders?

	let val x = ...
	    val y = ...
	in x + y end

	let val x = ... in x end
*)

local open PrettyPrint
in

datatype exp
  = Let of dcl list * exp list
  | Var of string
  | Num of int
  | Plus of exp * exp

and dcl
  = Val of string * exp

fun formatExp (Var s) = text s
  | formatExp (Num n) = integer n
  | formatExp (Plus (exp1, exp2)) =
      pcat (hcat (formatExp exp1, text "+"),
	    indent 2 (formatExp exp2))
  | formatExp (Let (dcls, exps)) =
      tryFlat
        (vblock
	   [hcat (text "let", fmtDcls dcls),
            indent 1 (hcat (text "in", formatExps exps)),
            text "end"])

and formatExps (exps: exp list) =
    tryFlat (vsequence semicolon (map formatExp exps))

and fmtDcl (Val (name, exp)) =
    pcat (hblock [text "val", text name, text "="],
	  indent 4 (formatExp exp))

and fmtDcls dcls = vblock (map fmtDcl dcls)

end; (* local *)

(* examples *)

val exp1 = Let ([Val ("x", Num 1), Val ("y", Num 2)], [Plus (Var "x", Num 3), Var "y"]);

fun test fmt n = printFormatLW n fmt;

val test1 = test (formatExp exp1);
