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
  structure F = Formatting
  fun kw s = F.styled (F.STY "kw") (F.text s)
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

fun formatExp (Var s) = F.text s
  | formatExp (Num n) = F.text(Int.toString n)
  | formatExp (Plus(exp1, exp2)) = F.pBlock [
        F.hBlock [formatExp exp1, F.text "+"],
        F.indent 2 (formatExp exp2)
      ]
  | formatExp (Let (dcls, exps)) = let
      val body = formatExps exps
      in
        F.tryFlat (F.vBlock [
            F.hBlock[letKW, fmtDcls dcls],
(* NOTE: changed from original version to fix bug in layout *)
            F.alt (
              F.hBlock [inKW, body, endKW],
              F.vBlock [inKW, F.indent 2 body, endKW])
          ])
      end

and formatExps (exps: exp list) =
      F.tryFlat (F.sequenceWithMap {align=F.V, sep=F.text ";", fmt=formatExp} exps)

and fmtDcl (Val (name, exp)) = F.pBlock [
	F.hBlock [valKW, F.text name, F.text "="],
	F.indent 4 (formatExp exp)
      ]

and fmtDcls dcls = F.vBlock (List.map fmtDcl dcls)

end; (* local *)

(* examples *)

val exp1 = Let ([Val ("x", Num 1), Val ("y", Num 2)], [Plus (Var "x", Num 3), Var "y"]);

fun test fmt n = printFormatLW n fmt;

val test1 = test (formatExp exp1);
