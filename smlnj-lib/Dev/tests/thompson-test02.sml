(* thompson-test02.sml
 *
 * Test for Development Issue #377 (ThompsonEngine can't match `Interval (r, m, SOME m)`)
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *)

CM.make "../../RegExp/regexp-lib.cm";

structure R = RegExpFn(
  structure P = AwkSyntax
  structure E = ThompsonEngine);

val r = R.compileString "a{2}b";

(* expect `SOME` *)
val () = (case StringCvt.scanString (R.find r) "aab"
         of SOME(MatchTree.Match({len=3,pos=0},[])) => print "OK\n"
          | SOME _ => print "FAIL: invalid match\n"
          | NONE => print "FAIL: no match\n"
        (* end case *));
(* expect `NONE` *)
val () = (case StringCvt.scanString (R.find r) "ab"
         of SOME _ => print "FAIL: unexpected match\n"
          | NONE => print "OK\n"
        (* end case *));
