(* syntax-sig.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This is the abstract syntax tree used to represent regular expressions.
 * It serves as the glue between different front-ends (implementing
 * different RE specification languages), and different back-ends (implementing
 * different compilation/searching algorithms).
 *)

signature REGEXP_SYNTAX =
  sig

    exception CannotCompile

    structure CharSet : ORD_SET where type Key.ord_key = char

    datatype syntax
      = Group of syntax
      | Alt of syntax list
      | Concat of syntax list
      | Interval of (syntax * int * int option)
      | MatchSet of CharSet.set
      | NonmatchSet of CharSet.set
      | Char of char
      | Begin                   (* Matches beginning of stream *)
      | End                     (* Matches end of stream *)

  (* shorthand for standard RE forms *)
    val optional : syntax -> syntax	(* == Interval(re, 0, SOME 1) *)
    val closure : syntax -> syntax	(* == Interval(re, 0, NONE) *)
    val posClosure : syntax -> syntax	(* == Interval(re, 1, NONE) *)

    val fromRange : char * char -> CharSet.set
    val addRange : CharSet.set * char * char -> CharSet.set

    val allChars : CharSet.set

  (* POSIX character sets (plus a couple) *)
    val alnum : CharSet.set	(* letters and digits *)
    val alpha : CharSet.set	(* letters *)
    val ascii : CharSet.set	(* 0 <= ord c <= 127 *)
    val blank : CharSet.set	(* #"\t" and space *)
    val cntl : CharSet.set	(* non-printable characters *)
    val digit : CharSet.set	(* decimal digits *)
    val graph : CharSet.set	(* visible characters (does not include space) *)
    val lower : CharSet.set	(* lower-case letters *)
    val print : CharSet.set	(* printable characters (includes space) *)
    val punct : CharSet.set	(* visible characters other than letters and digits *)
    val space : CharSet.set	(* #"\t", #"\r", #"\n", #"\v", #"\f", and space *)
    val upper : CharSet.set	(* upper-case letters *)
    val word : CharSet.set	(* letters, digit, and underscore *)
    val xdigit : CharSet.set	(* hexadecimal digits *)

  end;
