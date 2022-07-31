(* engines.sml
 *
 * COPYRIGHT (c) 2008 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure ThompsonRE = TestFn (
    val engineName = "Thompson's engine"
    structure RE = RegExpFn(
      structure P = AwkSyntax
      structure E = ThompsonEngine))

structure DfaRE = TestFn (
    val engineName = "DFA engine"
    structure RE = RegExpFn(
      structure P = AwkSyntax
      structure E = DfaEngine))

structure BackTrackRE = TestFn (
    val engineName = "Back-tracking engine"
    structure RE = RegExpFn(
      structure P = AwkSyntax
      structure E = BackTrackEngine))
