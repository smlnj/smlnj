(* trilean.sml
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Three-valued logic to represent false/true/unknown values.  The logical
 * connectives preserve the "unknowness" of their arguments.
 *
 * The name is taken from a similar Java class
 * (https://nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/util/Trilean.html)
 *)

structure Trilean : TRILEAN =
  struct

    datatype t = F | T | U

    fun toString F = "false"
      | toString T = "true"
      | toString U = "unknown"

    local
      datatype suffix = DONE | NEXT of char * suffix | ACCEPT of t * suffix
    in
    fun scan (getc : (char, 'a) StringCvt.reader) : (t, 'a) StringCvt.reader = let
          fun scanSuffix (inS0 : 'a, tri : t, suffix) = let
                fun lp (inS : 'a, [] : char list) = SOME(tri, inS)
                  | lp (inS, c::cs) = (case getc inS
                       of NONE => SOME(tri, inS0)
                        | SOME(c', inS') => if (Char.toLower c = c')
                            then lp (inS', cs)
                            else SOME(tri, inS0)
                      (* end case *))
                in
                  lp (inS0, suffix)
                end
          fun st0 inS = (case getc inS
                 of SOME(c, inS') => if Char.isSpace c
                      then st0 inS'
                      else (case Char.toLower c
                         of #"f" => stF inS
                          | #"t" => stT inS
                          | #"u" => stU inS
                          | _ => NONE
                        (* end case *))
                  | NONE => NONE
                (* end case *))
          (* match "f[alse]" having seen "f" *)
          and stF inS0 = scanSuffix (inS0, F, [#"a", #"l", #"s", #"e"])
          (* match "t[rue]" having seen "t" *)
          and stT inS0 = scanSuffix (inS0, T, [#"r", #"u", #"e"])
          (* match "unk[nown]" having seen "u" *)
          and stU inS0 = let
                fun stUNK inS = scanSuffix (inS, U, [#"n", #"o", #"w", #"n"])
                fun stUN inS = (case getc inS
                       of SOME(#"k", inS') => stUNK inS'
                        | SOME(#"K", inS') => stUNK inS'
                        | _ => SOME(U, inS0)
                      (* end case *))
                in
                  case getc inS0
                   of SOME(#"n", inS') => stUN inS'
                    | SOME(#"N", inS') => stUN inS'
                    | _ => SOME(U, inS0)
                  (* end case *)
                end
          in
            st0
          end
    end (* local *)

    val fromString = StringCvt.scanString scan

    fun fromBool true = T
      | fromBool false = F

    fun toBool F = SOME false
      | toBool T = SOME true
      | toBool U = NONE

    fun isTrue T = true
      | isTrue _ = false

    fun isFalse F = true
      | isFalse _ = false

    fun isKnown U = false
      | isKnown _ = true

    fun isUnknown U = true
      | isUnknown _ = false

    fun not3 T = F
      | not3 F = T
      | not3 U = U

    fun and3 (T, T) = T
      | and3 (U, _) = U
      | and3 (_, U) = U
      | and3 _ = F

    fun or3 (F, F) = F
      | or3 (U, _) = U
      | or3 (_, U) = U
      | or3 _ = T

    fun exists3 pred xs = let
          fun chk ([], acc) = acc
            | chk (x::xr, acc) = (case (pred x, acc)
                 of (T, _) => T
                  | (_, U) => chk (xr, U)
                  | (U, _) => chk (xr, U)
                  | (F, _) => chk (xr, F)
                (* end case *))
          in
            chk (xs, F)
          end

    fun all3 pred xs = let
          fun chk ([], acc) = acc
            | chk (x::xr, acc) = (case (pred x, acc)
                 of (F, _) => F
                  | (_, U) => chk (xr, U)
                  | (U, _) => chk (xr, U)
                  | (T, _) => chk (xr, T)
                (* end case *))
          in
            chk (xs, T)
          end

  end
