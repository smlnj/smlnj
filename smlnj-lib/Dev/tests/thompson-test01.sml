(* thompson-test01.sml
 *
 * Test for Development Issue #376 (ThompsonEngine does not implement `^`)
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

CM.make "../../RegExp/regexp-lib.cm";

structure R = RegExpFn(
  structure P = AwkSyntax
  structure E = ThompsonEngine);

val r = R.compileString "^b";

(* expect `NONE` *)
val () = (case StringCvt.scanString (R.find r) "ab"
         of NONE => print "OK\n"
          | SOME _ => print "FAIL\n"
        (* end case *));
