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

local
  structure PP = PrettyPrint
  fun kw s = PP.style (Atom.atom "kw") (PP.text s)
  val letKW = kw "let"
  val valKW = kw "val"
  val inKW = kw "in"
  val endKW = kw "end"
in

datatype exp
  = Let of dcl list * exp list
  | Var of string
  | Num of int
  | Plus of exp * exp

and dcl
  = Val of string * exp

fun formatExp (Var s) = PP.text s
  | formatExp (Num n) = PP.text(Int.toString n)
  | formatExp (Plus(exp1, exp2)) = PP.pBlock [
        PP.hBlock [formatExp exp1, PP.text "+"],
        PP.indent 2 (formatExp exp2)
      ]
  | formatExp (Let (dcls, exps)) = let
      val body = formatExps exps
      in
        PP.tryFlat (PP.vBlock [
            PP.hBlock[letKW, fmtDcls dcls],
(* NOTE: changed from original version to fix bug in layout *)
            PP.alt (
              PP.hBlock [inKW, body, endKW],
              PP.vBlock [inKW, PP.indent 2 body, endKW])
          ])
      end

and formatExps (exps: exp list) =
      PP.tryFlat (PP.sequenceWithMap {align=PP.V, sep=PP.text ";", fmt=formatExp} exps)

and fmtDcl (Val (name, exp)) = PP.pBlock [
	PP.hBlock [valKW, PP.text name, PP.text "="],
	PP.indent 4 (formatExp exp)
      ]

and fmtDcls dcls = PP.vBlock (List.map fmtDcl dcls)

end; (* local *)

(* examples *)

val exp1 = Let ([Val ("x", Num 1), Val ("y", Num 2)], [Plus (Var "x", Num 3), Var "y"]);

fun test fmt n = printFormatLW n fmt;

val test1 = test (formatExp exp1);
