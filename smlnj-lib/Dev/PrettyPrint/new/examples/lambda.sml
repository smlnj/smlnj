(* lambda.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

local
  structure F = Formatting

  val lambdaTok = F.TOK{name = "λ", measure = 1}
  val darrowTok = F.TOK{name = "⇒", measure = 1}

  val sp = F.block[F.BRK(F.Space 1)]

in

datatype lexp
  = V of string
  | ABS of string * lexp
  | APP of lexp * lexp

fun pp (V s) = F.text s
  | pp (ABS(x, e)) = F.cBlock [
        F.text "(", F.token lambdaTok, sp, F.text x, sp,
        F.token darrowTok, sp, pp e, F.text ")"
      ]
  | pp (APP(e1, e2)) = F.hBlock [pp e1, pp e2]

fun lambdaTest1 n = let
      val f = ABS("x", APP(V "x", V "x"))
      in
        printFormatLW n (pp (APP(f, f)))
      end

end (* local *)
