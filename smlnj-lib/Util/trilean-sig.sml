(* trilean-sig.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Three-valued logic to represent false/true/unknown values.  The logical
 * connectives preserve the "unknowness" of their arguments.
 *
 * The name is taken from a similar Java class
 * (https://nlp.stanford.edu/nlp/javadoc/javanlp/edu/stanford/nlp/util/Trilean.html)
 *)

signature TRILEAN =
  sig

    (* three-valued logic: false, true, or unknown *)
    datatype t = F | T | U

    (* return string representation ("false", "true", or "unknown") *)
    val toString : t -> string

    (* convert a string to a Trilean value; we accept "f", "t", "u",
     * "unk", "false", "true", and "unknown" (case insensitive) as valid
     * strings.
     *)
    val fromString : string -> t option
    val scan : (char,'a) StringCvt.reader -> (t,'a) StringCvt.reader

    (* convert bool to trilean value *)
    val fromBool : bool -> t

    (* convert a trilean value to an optional boolean, where `U` is mapped to `NONE` *)
    val toBool : t -> bool option

    (* predicates for testing values *)
    val isTrue : t -> bool
    val isFalse : t -> bool
    val isKnown : t -> bool
    val isUnknown : t -> bool

    (* logical connectives *)
    val not3 : t -> t
    val and3 : t * t -> t
    val or3 : t * t -> t

    (* `exists3 pred xs` returns `T` if there exists an element `x` in `xs` for
     * which `pred x` returns `T`, and `F` if `pred` returns `F` for all elements
     * in `xs`; otherwise it returns `U`.
     *)
    val exists3 : ('a -> t) -> 'a list -> t

    (* `all3 pred xs` returns `T` if `pred x` returns `T` for every `x` in `xs`,
     * it returns `F` if `pred x` returns `F` for at least one `x` in `xs`, and
     * otherwise returns `U`.
     *)
    val all3 : ('a -> t) -> 'a list -> t

  end
