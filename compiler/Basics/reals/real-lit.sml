(* real-lit.sml
 *
 * Internal representation of floating-point literals with limited
 * support for arithmetic.
 *
 * COPYRIGHT (c) 2018 John Reppy (http://cs.uchicago.edu/~jhr)
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * This code is part of the SML Compiler Utilities, which can be found at
 *
 *      https://github.com/JohnReppy/sml-compiler-utils
 *)

structure RealLit :> sig

    type t

    exception NaN

  (* predicates *)
    val isZero : t -> bool              (* true for 0 or -0 *)
    val isNeg : t -> bool               (* true for negative numbers (incl. -0) *)
    val isNan : t -> bool               (* true for NaNs *)
    val isFinite : t -> bool            (* true for non infinities/NaNs *)

  (* return the representation of +/-0.0, where zero true is -0.0 *)
    val zero : bool -> t

  (* plus and minus one *)
    val one : t
    val m_one : t

  (* special IEEE float values *)
    val nan : t         (* some quiet NaN *)
    val posInf : t      (* positive infinity *)
    val negInf : t      (* negative infinity *)

  (* operations on literals as if they were reals; raise NaN if an argument is a NaN *)
    val lessThan : t * t -> bool        (* comparison in real ordering; note that
                                         * -0.0 is not less than +0.0.
                                         *)
    val negate : t -> t                 (* negation *)
    val abs : t -> t                    (* absolute value *)

  (* equality, comparisons, and hashing functions *)
    val same : (t * t) -> bool
    val compare : (t * t) -> order (* not ordering on reals *)
    val hash : t -> word

  (* create a real literal from pieces: isNeg is true if the number is negative,
   * whole is the whole-number part, frac is the fractional part, and exp is the
   * exponent.  This function may raise Overflow, when the exponent of the
   * normalized representation is too small or too large.
   *)
    val real : {isNeg : bool, whole : string, frac : string, exp : IntInf.int} -> t

  (* create a floating-point literal from a sign, decimal fraction, and exponent *)
    val fromDigits : {isNeg : bool, digits : int list, exp : IntInf.int} -> t

  (* create a floating-point literal from an integer *)
    val fromInt : IntInf.int -> t

  (* make a REAL token from a string; raises Fail if the string is not a valid
   * SML string literal.  Note that Successor ML allows '_' as a separator character
   * in numeric literals, so we handle that too.
   *)
    val fromString : string -> t

  (* a concrete representation of the literal; note that +/-0 will have the
   * representation of digits=[], exp=0.
   *)
    datatype rep
      = PosInf          (* positive infinity *)
      | NegInf          (* negative infinity *)
      | QNaN            (* some quiet NaN *)
      | Flt of {isNeg : bool, digits : int list, exp : IntInf.int}

  (* reveal the representation of the literal *)
    val toRep : t -> rep

  (* return a string representation of a literal.  Note that this conversion uses "-" to
   * denote negative numbers (not "~").
   *)
    val toString : t -> string

  (* external representation (for pickling) *)
    val toBytes : t -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> t

  end = struct

    structure SS = Substring
    structure W = Word
    structure W8V = Word8Vector

  (* The value {isNeg, digits=[d0, ..., dn], exp} represents the number
   *
   *    [+/-] 0.d0...dn * 10^exp
   *
   * where the sign is negative if isNeg is true.  We require that dn <> 0.
   * +/- zero is represented by the empty digit sequence.
   *)
    datatype rep
      = PosInf          (* positive infinity *)
      | NegInf          (* negative infinity *)
      | QNaN            (* some quiet NaN *)
      | Flt of {isNeg : bool, digits : int list, exp : IntInf.int}

    type t = rep

    exception NaN

    fun toRep lit = lit

    fun isZero (Flt{digits=[], ...}) = true
      | isZero _ = false

    fun isNeg NegInf = true
      | isNeg (Flt{isNeg, ...}) = isNeg
      | isNeg _ = false

    fun isNan QNaN = true
      | isNan _ = false

    fun isFinite (Flt _) = true
      | isFinite _ = false

    fun zero isNeg = Flt{isNeg = isNeg, digits = [], exp = 0}

    val one = Flt{isNeg = false, digits = [1], exp = 1}
    val m_one = Flt{isNeg = true, digits = [1], exp = 1}

  (* special real literals *)
    val nan = QNaN
    val posInf = PosInf
    val negInf = NegInf

    fun lessThan (QNaN, _) = raise NaN
      | lessThan (_, QNaN) = raise NaN
      | lessThan (_, NegInf) = false
      | lessThan (NegInf, _) = true
      | lessThan (PosInf, _) = false
      | lessThan (_, PosInf) = false
      | lessThan (Flt{digits=[], ...}, Flt{digits=[], ...}) = false
      | lessThan (Flt{isNeg=true, ...}, Flt{isNeg=false, ...}) = true
      | lessThan (Flt{isNeg=false, ...}, Flt{isNeg=true, ...}) = false
      | lessThan (Flt{isNeg, digits=d1, exp=e1}, Flt{digits=d2, exp=e2, ...}) =
        (* both have same sign *)
          if (e1 < e2) then not isNeg
          else if (e2 < e1) then isNeg
          else (case List.collate Int.compare (d1, d2)
             of LESS => not isNeg
              | EQUAL => false
              | GREATER => isNeg
            (* end case *))

  (* negate a real literal *)
    fun negate PosInf = NegInf
      | negate NegInf = PosInf
      | negate QNaN = raise NaN
      | negate (Flt{isNeg, digits, exp}) =
          Flt{isNeg = not isNeg, digits = digits, exp = exp}

  (* return the absolute value of a literal *)
    fun abs PosInf = PosInf
      | abs NegInf = PosInf
      | abs QNaN = raise NaN
      | abs (Flt{digits, exp, ...}) = Flt{isNeg=false, digits=digits, exp=exp}

  (* equality, comparisons, and hashing functions *)
    fun same (NegInf, NegInf) = true
      | same (PosInf, PosInf) = true
      | same (QNaN, QNaN) = true
      | same (Flt f1, Flt f2) =
          (#isNeg f1 = #isNeg f2) andalso (#exp f1 = #exp f2)
          andalso (#digits f1 = #digits f2)
      | same _ = false

    fun compare (NegInf, NegInf) = EQUAL
      | compare (NegInf, _) = LESS
      | compare (_, NegInf) = GREATER
      | compare (PosInf, PosInf) = EQUAL
      | compare (PosInf, _) = LESS
      | compare (_, PosInf) = GREATER
      | compare (QNaN, QNaN) = EQUAL
      | compare (QNaN, _) = LESS
      | compare (_, QNaN) = GREATER
      | compare (Flt f1, Flt f2) = (case (#isNeg f1, #isNeg f2)
           of (false, true) => GREATER
            | (true, false) => LESS
            | _ => (case IntInf.compare(#exp f1, #exp f2)
                 of EQUAL => let
                      fun cmp ([], []) = EQUAL
                        | cmp ([], _) = LESS
                        | cmp (_, []) = GREATER
                        | cmp (d1::r1, d2::r2) = (case Int.compare(d1, d2)
                             of EQUAL => cmp(r1, r2)
                              | order => order
                            (* end case *))
                      in
                        cmp (#digits f1, #digits f2)
                      end
                  | order => order
                (* end case *))
          (* end case *))

    fun hash PosInf = 0w1
      | hash NegInf = 0w3
      | hash QNaN = 0w5
      | hash (Flt{isNeg, digits, exp}) = let
          fun hashDigits ([], h, _) = h
            | hashDigits (d::r, h, i) =
                hashDigits (r, W.<<(W.fromInt d, i+0w4), W.andb(i+0w1, 0wxf))
          in
            hashDigits(digits, W.fromLargeInt exp, 0w0)
          end

    fun real {isNeg, whole, frac, exp} = let
          fun cvtDigit (c, l) = (Char.ord c - Char.ord #"0") :: l
          fun isZero #"0" = true | isZero _ = false
        (* whole digits with leading zeros removed *)
          val whole = SS.dropl isZero (SS.full whole)
        (* fractional digits with trailing zeros removed *)
          val frac = SS.dropr isZero (SS.full frac)
        (* normalize by stripping leading zero digits *)
          fun normalize {isNeg, digits=[], exp} = zero isNeg
            | normalize {isNeg, digits=0::r, exp} =
                normalize {isNeg=isNeg, digits=r, exp=exp-1}
            | normalize flt = Flt flt
          in
            case SS.foldr cvtDigit (SS.foldr cvtDigit [] frac) whole
             of [] => zero isNeg
              | digits => normalize {
                    isNeg = isNeg,
                    digits = digits,
                    exp = exp + IntInf.fromInt(SS.size whole)
                  }
            (* end case *)
          end

  (* helper function to strip trailing zeros from a list of digits *)
    fun stripZeros {isNeg, digits, exp} = let
          fun strip [] = []
            | strip (0::ds) = (case strip ds
                 of [] => []
                  | ds => 0::ds
                (* end case *))
            | strip (d::ds) = d :: strip ds
          in
            case strip digits
             of [] => zero isNeg
              | digits => Flt{isNeg=isNeg, digits=digits, exp=exp}
            (* end case *)
          end

  (* create a floating-point literal from a sign, decimal fraction, and exponent *)
    fun fromDigits arg = let
        (* normalize by stripping leading zero digits *)
          fun normalize {isNeg, digits=[], exp} = zero isNeg
            | normalize {isNeg, digits=0::r, exp} =
                normalize {isNeg=isNeg, digits=r, exp=exp-1}
            | normalize arg = stripZeros arg
          in
            normalize arg
          end

    fun fromInt 0 = zero false
      | fromInt n = let
          val (isNeg, n) = if (n < 0) then (true, ~n) else (false, n)
          fun toDigits (n, ds) = if n < 10
                then IntInf.toInt n :: ds
                else let
                  val (q, r) = IntInf.quotRem(n, 10)
                  in
                    toDigits(q, IntInf.toInt r :: ds)
                  end
          val digits = toDigits(n, [])
          in
            stripZeros {
                isNeg = isNeg,
                digits = digits,
                exp = IntInf.fromInt(List.length digits)
              }
          end

    local
      structure SS = Substring
    (* is a character #"0" or a #"_" (Successor ML) *)
      fun isZero #"0" = true
        | isZero #"_" = true (* Successor ML *)
        | isZero _ = false
    (* convert a character in the range #"0" .. #"9" to an integer *)
      fun mkDigit c = Char.ord c - Char.ord #"0"
    in
    fun fromString s = let
          val num = SS.full s
          val (isNeg, rest) = (case SS.getc num
                 of SOME(#"~", r) => (true, r)
                  | _ => (false, num)
                (* end case *))
        (* get the digits for the whole number part in reverse order with leading
         * 0's stripped, the number of digits in the whole part, and the remaining
         * substring.
         *)
          val (whole, wholeLen, rest) = let
                fun get (ss, len, digits) = (case SS.getc ss
                       of SOME(#"_", ss) => get (ss, len, digits)
                        | SOME(c, ss') => if Char.isDigit c
                            then get (ss', len+1, mkDigit c :: digits)
                            else (digits, len, ss)
                        | NONE => (digits, len, ss)
                      (* end case *))
                in
                  get (SS.dropl isZero rest, 0, [])
                end
        (* get the fractional digits with trailing 0's stripped and the remaining
         * substring
         *)
          val (frac, rest) = (case SS.getc rest
                 of SOME(#".", ss) => let
                      val (frac, rest) =
                            SS.splitl
                              (fn #"e" => false | #"E" => false | _ => true)
                                ss
                      fun get (ss, digits) = (case SS.getc ss
                             of SOME(#"_", ss) => get (ss, digits)
                              | SOME(c, ss') => get (ss', mkDigit c :: digits)
                              | NONE => List.rev digits
                            (* end case *))
                      in
                        (get (SS.dropr isZero frac, []), rest)
                      end
                  | _ => ([], rest) (* empty or else #"e" or #"E" *)
                (* end case *))
        (* construct the digit list *)
          val digits = List.revAppend(whole, frac)
        (* get the exponent *)
          val exp = if Substring.isEmpty rest orelse List.null digits
                then 0
                else let
                  val rest = (Substring.triml 1 rest) (* trim #"e" or #"E" *)
                  fun get (ss, exp) = (case SS.getc ss
                         of SOME(#"_", ss) => get (ss, exp)
                          | SOME(d, ss) => get (ss, 10*exp + IntInf.fromInt(mkDigit d))
                          | NONE => exp
                        (* end case *))
                  in
                    case SS.getc rest
                     of SOME(#"~", ss) => ~(get (ss, 0))
                      | SOME _ => get (rest, 0)
                      | NONE => 0
                    (* end case *)
                  end
          in
            Flt{isNeg=isNeg, digits=digits, exp=exp + wholeLen}
          end
    end (* local *)

    fun toString PosInf = "+inf"
      | toString NegInf = "~inf"
      | toString QNaN = "nan"
      | toString (Flt{isNeg, digits=[], ...}) = if isNeg then "~0.0" else "0.0"
      | toString (Flt{isNeg, digits, exp}) = let
          val s = if isNeg then "~0." else "0."
          val e = if exp < 0
                then ["e~", IntInf.toString(~exp)]
                else ["e", IntInf.toString exp]
          in
            concat(s :: List.foldr (fn (d, ds) => Int.toString d :: ds) e digits)
          end

  (***** external representation (for pickling) *****
   *
   * The representation we use is a sequence of bytes:
   *
   *    [sign, d0, ..., dn, exp0, ..., exp3]
   *
   * where
   *    sign    == 0 or 1
   *    di      == ith digit
   *    expi    == ith byte of exponent (exp0 is lsb, exp3 is msb).
   *
   * we encode Infs and NaNs using the sign byte:
   *
   *    2       == PosInf
   *    3       == NegInf
   *    4       == QNaN
   *
   * NOTE: we could pack the sign and digits into 4-bit nibbles, but we are keeping
   * things simple for now.
   *)

    fun toBytes PosInf = Word8Vector.fromList [0w2]
      | toBytes NegInf = Word8Vector.fromList [0w3]
      | toBytes QNaN = Word8Vector.fromList [0w4]
      | toBytes (Flt{isNeg, digits, exp}) = let
          val sign = if isNeg then 0w1 else 0w0
          val digits = List.map Word8.fromInt digits
          val exp' = W.fromLargeInt exp
          fun byte i = Word8.fromLargeWord(W.toLargeWord((W.>>(exp', 0w8*i))))
          val exp = [byte 0w3, byte 0w2, byte 0w1, byte 0w0]
          in
            Word8Vector.fromList(sign :: (digits @ exp))
          end

    fun fromBytes v = let
          fun error () = raise Fail "Bogus real-literal pickle"
          val len = W8V.length v
          in
            if (len = 1)
              then (case W8V.sub(v, 0) (* special real value *)
                 of 0w2 => PosInf
                  | 0w3 => NegInf
                  | 0w4 => QNaN
                  | _ => error()
                (* end case *))
              else let
                val ndigits = W8V.length v - 5
                val _ = if (ndigits < 0) then error() else ()
                val isNeg = (case W8V.sub(v, 0)
                       of 0w0 => false
                        | 0w1 => true
                        | _ => error()
                      (* end case *))
                fun digit i = let val d = Word8.toInt(W8V.sub(v, i+1))
                      in
                        if (d < 10) then d else error()
                      end
                fun byte i = W.<<(
                      W.fromLargeWord(Word8.toLargeWord(W8V.sub(v, ndigits+1+i))),
                      W.fromInt(8*(3-i)))
                val exp = W.toLargeIntX(W.orb(byte 3, W.orb(byte 2, W.orb(byte 1, byte 0))))
                in
                  Flt{isNeg = isNeg, digits = List.tabulate(ndigits, digit), exp = exp}
                end
          end

  end

