(* fast-parser.sml
 *
 * A "fast" parser for JSON input with minimal error checking.  It is meant
 * for applications that expect to only get correct input and where performance
 * is important.  For an example, an LSP server might use this library, since
 * it would assume that its clients produce correct JSON.
 *
 * The Unicode escape-sequence parsing is based, in part, on an implementation
 * written by Skye Soss.
 *
 * COPYRIGHT (c) 2023 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FastJSONParser : sig

    (* syntax-error codes *)
    datatype error_code
      = InvalidCharacter
      | InvalidLiteral
      | NumberTooLarge
      | InvalidNumber
      | InvalidArray
      | InvalidObject
      | ExpectedKey
      | ExpectedColon
      | CommentsNotAllowed
      | UnclosedComment
      | UnclosedString
      | InvalidEscape
      | InvalidUTF8
      | IncompleteUTF8
      | InvalidUnicodeSurrogatePair
      | InvalidUnicodeEscape
      | NonPrintingASCII

    (* return a string representation of an error code *)
    val errorMessage : error_code -> string

    (* options that control parsing behavior *)
    type 'a options = {
        (* flag to enable C-style comments in the input *)
        comments : bool,
        (* limit on the number of digits allowed in an integer literal.  If the
         * limit is exceeded, then the `NumberToLarge` error code is returned.
         * This mechanism avoids a potential DOS attack.  A value of NONE is
         * effectively infinite.
         *)
        maxDigits : int option,
        (* error handler; given the error code and stream at the point of the error *)
        error : error_code * 'a -> unit
      }

    (* the default options: `{comments=true, maxDigits=SOME 19, error=ef}`,
     * where `ef` is a function that raises `SyntaxError`.  Note that
     * 19 digits is sufficient to handle 64-bit integers.
     *)
    val defaultOptions : options

    exception SyntaxError of string

    (* `parseWithOpts opts getc strm` will return `(jv, rest)`, where `jv` is the
     * JSON value parsed from the input stream `strm` and `rest` is the rest of the
     * stream.   The `opts` argument controls the parser behavior as described
     * above and the `getc` argument is a reader for the stream type.
     * If there is any error in the input, then the `SyntaxError` exception is
     * raised.
     *)
    val parseWithOpts : 'strm options
          -> (char, 'strm) StringCvt.reader
          -> 'strm
          -> (JSON.value * 'strm)

    (* `parse getc strm` is equivalent to the expression
     * `parse defaultOptions getc strm`
     *)
    val parse : (char, 'strm) StringCvt.reader -> 'strm -> (JSON.value * 'strm)


  end = struct

    structure W = Word

    (* error codes *)
    datatype error_code
      = InvalidCharacter
      | InvalidLiteral
      | NumberTooLarge
      | InvalidNumber
      | InvalidArray
      | InvalidObject
      | ExpectedKey
      | ExpectedColon
      | CommentsNotAllowed
      | UnclosedComment
      | UnclosedString
      | InvalidEscape
      | InvalidUTF8
      | IncompleteUTF8
      | InvalidUnicodeSurrogatePair
      | InvalidUnicodeEscape
      | NonPrintingASCII

    (* return a string representation of an error code *)
    fun errorMessage InvalidCharacter = "invalid character"
      | errorMessage InvalidLiteral = "invalid literal identifier"
      | errorMessage NumberTooLarge = "number exceeds maximum number of digits"
      | errorMessage InvalidNumber = "invalid number syntax"
      | errorMessage InvalidArray = "invalid array syntax; expected ',' or ']'"
      | errorMessage InvalidObject = "invalid object syntax; expected ',' or '}'"
      | errorMessage ExpectedKey = "invalid object syntax; expected key"
      | errorMessage ExpectedColon = "invalid object syntax; expected ':'"
      | errorMessage CommentsNotAllowed = "JSON comments not allowed"
      | errorMessage UnclosedComment = "unclosed comment"
      | errorMessage UnclosedString = "unclosed string"
      | errorMessage InvalidEscape = "invalid escape sequence"
      | errorMessage InvalidUTF8 = "invalid UTF-8"
      | errorMessage IncompleteUTF8 = "incomplete UTF-8"
      | errorMessage InvalidUnicodeSurrogatePair = "invalid Unicode surrogate pair"
      | errorMessage InvalidUnicodeEscape = "invalid Unicode escape sequence"
      | errorMessage NonPrintingASCII = "non-printing ASCII character"

    type 'a options = {
        comments : bool,
        maxDigits : int option,
        error : error_code * 'a -> unit
      }

    val defaultOptions = {
            comments=true,
            maxDigits=SOME 19, (* enough for 64-bit signed integers *)
            error = fn (ec, _) => raise SyntaxError(errorMessage ec)
          }

    exception SyntaxError of string

    (* a maximum number of digits used when `maxDigits` is `NONE` *)
    val defaultMaxDigits = (case Int.maxInt
           of SOME n => n
            | NONE => Word.toIntX(Word.<<(0w1, Word.fromInt(Word.wordSize-1))-0w1)
          (* end case *))

    (* fast (no overflow checking) increment/decrement operations *)
    fun inc n = W.toIntX(W.fromInt n + 0w1)
    fun dec n = W.toIntX(W.fromInt n - 0w1)

    (* local copy of list reverse that the compiler can inline *)
    fun reverse xs = let
          fun rev' ([], ys) = ys
            | rev' (x::xs, ys) = rev' (xs, x::ys)
          in
            rev' (xs, [])
          end

    (* make a string from a list of characters in reverse order; the first argument
     * is the number of characters, which must be equal or greater than the length
     * of the input list
     *)
    fun mkString (_, []) = ""
      | mkString (n, cs) = let
          val s = Unsafe.CharVector.create n
          fun init (_, []) = s
            | init (i, c::cs) = (
                Unsafe.CharVector.update(s, i, c);
                init (dec i, cs))
          in
            init (dec n, cs)
          end

    fun parseWithOpts {comments, maxDigits, error} getc = let
          fun error' (ec, inS) = (error(ec, inS); raise SyntaxError(errorMessage ec))
          val maxDigits = Option.getOpt (maxDigits, defaultMaxDigits)
          fun next inS = (case getc inS
                 of SOME(c, inS') => (c, inS')
                  | NONE => (#"\000", inS)
                (* end case *))
          (* skip white space *)
          fun skipWS inS = (case next inS
                 of (#" ", inS) => skipWS inS
                  | (#"\t", inS) => skipWS inS
                  | (#"\r", inS) => skipWS inS
                  | (#"\n", inS) => skipWS inS
                  | res => res
                (* end case *))
          fun matchC (inS, c) = let
                val (c', inS') = next inS
                in
                  if (c = c')
                    then inS'
                    else error'(InvalidLiteral, inS)
                end
          (* parse a JSON value *)
          fun parseValue inS = (case skipWS inS
                 of (#"[", inS) => parseArray inS
                  | (#"{", inS) => parseObject inS
                  | (#"-", inS) => (case next inS
                       of (#"0", inS') => scanNumber(inS, inS', true, 0)
                        | (#"1", inS') => scanNumber(inS, inS', true, 1)
                        | (#"2", inS') => scanNumber(inS, inS', true, 2)
                        | (#"3", inS') => scanNumber(inS, inS', true, 3)
                        | (#"4", inS') => scanNumber(inS, inS', true, 4)
                        | (#"5", inS') => scanNumber(inS, inS', true, 5)
                        | (#"6", inS') => scanNumber(inS, inS', true, 6)
                        | (#"7", inS') => scanNumber(inS, inS', true, 7)
                        | (#"8", inS') => scanNumber(inS, inS', true, 8)
                        | (#"9", inS') => scanNumber(inS, inS', true, 9)
                        | _ => error'(InvalidNumber, inS)
                      (* end case *))
                  | (#"0", inS') => scanNumber(inS, inS', false, 0)
                  | (#"1", inS') => scanNumber(inS, inS', false, 1)
                  | (#"2", inS') => scanNumber(inS, inS', false, 2)
                  | (#"3", inS') => scanNumber(inS, inS', false, 3)
                  | (#"4", inS') => scanNumber(inS, inS', false, 4)
                  | (#"5", inS') => scanNumber(inS, inS', false, 5)
                  | (#"6", inS') => scanNumber(inS, inS', false, 6)
                  | (#"7", inS') => scanNumber(inS, inS', false, 7)
                  | (#"8", inS') => scanNumber(inS, inS', false, 8)
                  | (#"9", inS') => scanNumber(inS, inS', false, 9)
                  | (#"\"", inS) => scanStringValue inS
                  | (#"f", inS) => let (* match "a" "l" "s" "e" *)
                      val inS = matchC (inS, #"a")
                      val inS = matchC (inS, #"l")
                      val inS = matchC (inS, #"s")
                      val inS = matchC (inS, #"e")
                      in
                        (JSON.BOOL false, inS)
                      end
                  | (#"n", inS) => let (* match "u" "l" "l" *)
                      val inS = matchC (inS, #"u")
                      val inS = matchC (inS, #"l")
                      val inS = matchC (inS, #"l")
                      in
                        (JSON.NULL, inS)
                      end
                  | (#"t", inS) => let (* match "r" "u" "e" *)
                      val inS = matchC (inS, #"r")
                      val inS = matchC (inS, #"u")
                      val inS = matchC (inS, #"e")
                      in
                        (JSON.BOOL true, inS)
                      end
                  | (#"/", inS) => if comments
                      then parseValue (skipComment inS)
                      else error'(CommentsNotAllowed, inS)
                  | _ => error'(InvalidCharacter, inS)
                (* end case *))
          (* parse a JSON array assuming that the '[' has been consumed *)
          and parseArray inS = let
                (* loop to scan one or more items *)
                fun lp (inS, items) = let
                        val (item, inS) = parseValue inS
                        val items = item::items
                        in
                          case skipWS inS
                           of (#",", inS) => lp (inS, items)
                            | (#"]", inS) => (JSON.ARRAY(reverse items), inS)
                            | _ => error'(InvalidArray, inS)
                          (* end case *)
                        end
                in
                  case skipWS inS
                   of (#"]", inS) => (JSON.ARRAY[], inS)
                    | _ => lp (inS, [])
                  (* end case *)
                end
          (* parse a JSON object assuming that the '[' has been consumed *)
          and parseObject inS = let
                (* loop to scan one or more key-value pairs *)
                fun lp (inS, items) = (case skipWS inS
                       of (#"\"", inS) => let
                            val (key, inS) = scanString inS
                            in
                              case skipWS inS
                               of (#":", inS) => let
                                    val (v, inS) = parseValue inS
                                    val items = (key, v)::items
                                    in
                                      case skipWS inS
                                       of (#",", inS) => lp (inS, items)
                                        | (#"}", inS) => (JSON.OBJECT(reverse items), inS)
                                        | _ => error'(InvalidObject, inS)
                                      (* end case *)
                                    end
                                | _ => error'(ExpectedColon, inS)
                              (* end case *)
                            end
                        | _ => error'(ExpectedKey, inS)
                      (* end case *))
                in
                  case skipWS inS
                   of (#"}", inS) => (JSON.OBJECT[], inS)
                    | _ => lp (inS, [])
                  (* end case *)
                end
          (* scan a string value assuming that the first quote has been consumed *)
          and scanString start = let
                fun c2w c = W.fromInt(ord c)
                fun w2c w = Char.chr(W.toInt w)
                fun scan (inS, n, cs) = (case next inS
                       of (#"\000", _) => error'(UnclosedString, start)
                        | (#"\"", inS) => (mkString(n, cs), inS)
                        | (#"\\", inS) => scanEscape (inS, n, cs)
                        | (c, inS) => if (#" " <= c) andalso (c < #"\127")
                            (* printable ASCII character *)
                            then scan (inS, inc n, c::cs)
                            (* either non-printable ASCII or UTF-8 byte sequence *)
                            else scanUTF8 (inS, c, c2w c, n, cs)
                      (* end case *))
                and scanEscape (inS, n, cs) = let
                      fun return (inS, c) = scan (inS, inc n, c::cs)
                      in
                        case next inS
                         of (#"\"", inS) => return (inS, #"\"")
                          | (#"\\", inS) => return (inS, #"\\")
                          | (#"/", inS) => return (inS, #"/")
                          | (#"b", inS) => return (inS, #"\008") (* backspace *)
                          | (#"f", inS) => return (inS, #"\012") (* form feed *)
                          | (#"n", inS) => return (inS, #"\010") (* line feed *)
                          | (#"r", inS) => return (inS, #"\013") (* carriage return *)
                          | (#"t", inS) => return (inS, #"\009") (* tab *)
                          | (#"u", inS) => scanUnicodeEscape (inS, n, cs)
                          | _ => error'(InvalidEscape, inS)
                        (* end case *)
                      end
                (* scan a Unicode escape sequence; we have already consumed the "\u"
                 * prefix, so we just need to parse the four hex digits followed by
                 * a possible second escape sequence for a surrogate pair.  The result
                 * is encoded as a UTF-8 byte sequence.
                 *)
                and scanUnicodeEscape (inS, n, cs) = let
                      fun getDigit inS = (case next inS
                             of (#"0", inS) => (0w0, inS)
                              | (#"1", inS) => (0w1, inS)
                              | (#"2", inS) => (0w2, inS)
                              | (#"3", inS) => (0w3, inS)
                              | (#"4", inS) => (0w4, inS)
                              | (#"5", inS) => (0w5, inS)
                              | (#"6", inS) => (0w6, inS)
                              | (#"7", inS) => (0w7, inS)
                              | (#"8", inS) => (0w8, inS)
                              | (#"9", inS) => (0w9, inS)
                              | (#"a", inS) => (0w10, inS)
                              | (#"A", inS) => (0w10, inS)
                              | (#"b", inS) => (0w11, inS)
                              | (#"B", inS) => (0w11, inS)
                              | (#"c", inS) => (0w12, inS)
                              | (#"C", inS) => (0w12, inS)
                              | (#"d", inS) => (0w13, inS)
                              | (#"D", inS) => (0w13, inS)
                              | (#"e", inS) => (0w14, inS)
                              | (#"E", inS) => (0w14, inS)
                              | (#"f", inS) => (0w15, inS)
                              | (#"F", inS) => (0w15, inS)
                              | _ => error'(InvalidUnicodeEscape, inS)
                            (* end case *))
                      fun getDigits inS = let
                            (* get four digits *)
                            val (d0, inS) = getDigit inS
                            val (d1, inS) = getDigit inS
                            val (d2, inS) = getDigit inS
                            val (d3, inS) = getDigit inS
                            val n = W.<<(d0, 0w24)
                                  + W.<<(d1, 0w16)
                                  + W.<<(d2, 0w8)
                                  + d3
                            in
                              (n, inS)
                            end
                      val (u0, inS) = getDigits inS
                      (* get the second 16-bit code point of a surrogate pair *)
                      fun scanLowSurrogate inS = (
                           (* match "\uxxxx" *)
                            case next inS
                             of (#"\\", inS) => (case next inS
                                   of (#"u", inS) => let
                                        val (u1, inS) = getDigits inS
                                        in
                                          if (u1 < 0wxDC00) orelse (0wxDFFF < u1)
                                            then error'(InvalidUnicodeSurrogatePair, inS)
                                            (* convert pair to a Unicode code point
                                             * and then to UTF-8 bytes.
                                             *)
                                            else toUTF8 (inS,
                                              0wx10000
                                                + W.<<(u0 - 0wxD800, 0w10)
                                                + (u1 - 0wxDC00))
                                        end
                                    | _ => error'(InvalidUnicodeSurrogatePair, inS)
                                  (* end case *))
                              | _ => error'(InvalidUnicodeSurrogatePair, inS)
                            (* end case *))
                      (* convert a word to a UTF-8 sequence *)
                      and toUTF8 (inS, w) = if (w <= 0wx7f)
                              then scan (inS, inc n, w2c w :: cs)
                            else if (w <= 0wx7ff)
                              then scan (inS,
                                n+2,
                                w2c(W.orb(0wxc0, W.>>(w, 0w6)))
                                  :: w2c(W.orb(0wx80, W.andb(w, 0wx3f)))
                                  :: cs)
                            else if (w <= 0wxffff)
                              then scan (inS,
                                n+3,
                                w2c(W.orb(0wxe0, W.>>(w, 0w12)))
                                  :: w2c(W.orb(0wx80, W.andb(W.>>(w, 0w6), 0wx3f)))
                                  :: w2c(W.orb(0wx80, W.andb(w, 0wx3f)))
                                  :: cs)
                            else if (w <= 0wx10ffff)
                              then scan (inS,
                                n+4,
                                w2c(W.orb(0wxf0, W.>>(w, 0w18)))
                                  :: w2c(W.orb(0wx80, W.andb(W.>>(w, 0w12), 0wx3f)))
                                  :: w2c(W.orb(0wx80, W.andb(W.>>(w, 0w6), 0wx3f)))
                                  :: w2c(W.orb(0wx80, W.andb(w, 0wx3f)))
                                  :: cs)
                              else error'(InvalidUnicodeEscape, inS)
                      in
                        if (u0 < 0wxD800)
                          then toUTF8 (inS, u0)
                        else if (u0 <= 0wxDBFF)
                          then scanLowSurrogate inS
                          else error'(InvalidUnicodeEscape, inS)
                      end (* scanUnicodeEscape *)
                (* a simple state machine for scanning a valid UTF-8 byte sequence.  See
                 * https://unicode.org/mail-arch/unicode-ml/y2003-m02/att-0467/01-The_Algorithm_to_Valide_an_UTF-8_String
                 * for a description of the state machine.
                 *)
                and scanUTF8 (inS, chr0, byte0, n, cs) = let
                      fun getByte inS = (case next inS
                             of (#"\000", _) => error'(IncompleteUTF8, inS)
                              | (c, inS') => (c2w c, c, inS')
                            (* end case *))
                      fun inRange (minB : word, b, maxB) = ((b - minB) <= maxB - minB)
                      (* handles last byte for all multi-byte sequences *)
                      fun stateA (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wx80, b, 0wxbf)
                                then scan (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      (* handles second/third byte for three/four-byte sequences *)
                      and stateB (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wx80, b, 0wxbf)
                                then stateA (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      (* byte0 = 0b1110_0000 (3-byte sequence) *)
                      and stateC (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wxa0, b, 0wxbf)
                                then stateA (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      (* byte0 = 0b1110_1101 (3-byte sequence) *)
                      and stateD (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wx80, b, 0wx9f)
                                then stateA (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      (* byte0 = 0b1111_0001 .. 0b1111_0011 (4-byte sequence) *)
                      and stateE (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wx80, b, 0wxbf)
                                then stateB (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      (* byte0 = 0b1111_0000 (4-byte sequence) *)
                      and stateF (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wx90, b, 0wxbf)
                                then stateB (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      (* byte0 = 0b1111_1000 (4-byte sequence) *)
                      and stateG (inS, n, chrs) = let
                            val (b, c, inS) = getByte inS
                            in
                              if inRange(0wx80, b, 0wx8f)
                                then stateB (inS, inc n, c::chrs)
                                else error'(InvalidUTF8, inS)
                            end
                      in
                        if (byte0 <= 0wx7f)
                          (* this case only occurs for non-printing ASCII characters *)
                          then error'(NonPrintingASCII, inS)
                        else if inRange(0wxc2, byte0, 0wxdf)
                          then stateA (inS, n, cs)
                        else if inRange(0wxe1, byte0, 0wxec)
                        orelse inRange(0wxee, byte0, 0wxef)
                          then stateB (inS, n, cs)
                        else if (byte0 = 0wxe0)
                          then stateC (inS, n, cs)
                        else if (byte0 = 0wxed)
                          then stateD (inS, n, cs)
                        else if inRange(0wxf1, byte0, 0wxf3)
                          then stateE (inS, n, cs)
                        else if (byte0 = 0wxf0)
                          then stateF (inS, n, cs)
                        else if (byte0 = 0wxf4)
                          then stateG (inS, n, cs)
                          else error'(InvalidUTF8, inS)
                      end (* scanUTF8 *)
                in
                  scan (start, 0, [])
                end (* scanString *)
          and scanStringValue inS = let
                val (s, inS) = scanString inS
                in
                  (JSON.STRING s, inS)
                end
          (* scan an integer or floating-point number.  If the number of digits
           * for an integer literal exceeds the `maxDigits` limit, then we signal
           * a `NumberTooLarge` error.
           *)
          and scanNumber (startInS, inS, isNeg, firstDigit) = let
                (* make a JSON `FLOAT` value from pieces.  The lists of digits
                 * are in reverse order and are in the range [0..9].
                 *)
(* TODO: In 110.99.5, we can use the following version of the mkFloat function *)
(*
                fun mkFloat (sign, whole, frac, exp, inS) = let
                      val f = (Real.fromDecimal {
                              class = IEEEReal.NORMAL,
                              sign = sign,
                              digits = List.revAppend(whole, reverse frac),
                              exp = exp + List.length whole
                            }) handle Overflow => if sign then Real.negInf else Real.posInf
                      in
                        if Real.isFinite f
                          then (JSON.FLOAT f, inS)
                          else error' (NumberTooLarge, startInS)
                      end
*)
                fun mkFloat (false, [], [], _, inS) = (JSON.FLOAT 0.0, inS)
                  | mkFloat (true, [], [], _, inS) = (JSON.FLOAT ~0.0, inS)
                  | mkFloat (sign, whole, frac, exp, inS) = let
                      (* given a list of digits in reverse order, convert them
                       * to strings and append them onto `frags`.
                       *)
                      fun cvtAndAppend (digits, frags) =
                            List.foldl (fn (d, fs) => Int.toString d :: fs)
                              frags digits
                      val frags = if (exp <> 0) then ["E", Int.toString exp] else []
                      val frags = (case (whole, frac)
                             of ([], _) => "0." :: cvtAndAppend (frac, frags)
                              | (_, []) => cvtAndAppend (whole, frags)
                              | _ => cvtAndAppend (whole,
                                  "." :: cvtAndAppend (frac, frags))
                            (* end case *))
                      in
                        case Real.fromString (String.concat frags)
                         of SOME f => Real.isFinite f
                              then if sign
                                then (JSON.FLOAT(~f), inS)
                                else (JSON.FLOAT f, inS)
                              else error' (NumberTooLarge, startInS)
                          | NONE => raise Fail "impossible: ill-formed float"
                        (* end case *)
                      end
                (* scan an integer or the whole part of a float *)
                fun scanWhole (inS, digits) = (case next inS
                       of (#"0", inS) => scanWhole (inS, 0::digits)
                        | (#"1", inS) => scanWhole (inS, 1::digits)
                        | (#"2", inS) => scanWhole (inS, 2::digits)
                        | (#"3", inS) => scanWhole (inS, 3::digits)
                        | (#"4", inS) => scanWhole (inS, 4::digits)
                        | (#"5", inS) => scanWhole (inS, 5::digits)
                        | (#"6", inS) => scanWhole (inS, 6::digits)
                        | (#"7", inS) => scanWhole (inS, 7::digits)
                        | (#"8", inS) => scanWhole (inS, 8::digits)
                        | (#"9", inS) => scanWhole (inS, 9::digits)
                        | (#".", inS) => scanFrac (inS, digits)
                        | (#"e", inS) => scanExp (inS, digits, [])
                        | (#"E", inS) => scanExp (inS, digits, [])
                        | _ => let
                            fun cvt ([], _, n) = if isNeg
                                  then (JSON.INT(~n), inS)
                                  else (JSON.INT n, inS)
                              | cvt (d::ds, k, n) = if (k <= maxDigits)
                                  then cvt (ds, inc k, 10*n + IntInf.fromInt d)
                                  else error'(NumberTooLarge, startInS)
                            in
                              cvt (reverse digits, 0, 0)
                            end
                      (* end case *))
                (* scan the fractional part of a real; the '.' has already been
                 * consumed.
                 *)
                and scanFrac (inS, wDigits) = let
                      fun scanF (inS, fDigits) = (case next inS
                             of (#"0", inS) => scanF (inS, 0::fDigits)
                              | (#"1", inS) => scanF (inS, 1::fDigits)
                              | (#"2", inS) => scanF (inS, 2::fDigits)
                              | (#"3", inS) => scanF (inS, 3::fDigits)
                              | (#"4", inS) => scanF (inS, 4::fDigits)
                              | (#"5", inS) => scanF (inS, 5::fDigits)
                              | (#"6", inS) => scanF (inS, 6::fDigits)
                              | (#"7", inS) => scanF (inS, 7::fDigits)
                              | (#"8", inS) => scanF (inS, 8::fDigits)
                              | (#"9", inS) => scanF (inS, 9::fDigits)
                              | (#"e", inS) => scanExp (inS, wDigits, fDigits)
                              | (#"E", inS) => scanExp (inS, wDigits, fDigits)
                              | _ => mkFloat (isNeg, wDigits, fDigits, 0, inS)
                            (* end case *))
                      in
                        scanF (inS, [])
                      end
                (* scan the exponent part of a real; the "e"/"E" has already been
                 * consumed.
                 *)
                and scanExp (inS, whole, frac) = let
                      val (expSign, exp, seenDigit, inS) = (case next inS
                             of (#"-", inS) => (~1, 0, false, inS)
                              | (#"+", inS) => (1, 0, false, inS)
                              | (#"0", inS) => (1, 0, true, inS)
                              | (#"1", inS) => (1, 1, true, inS)
                              | (#"2", inS) => (1, 2, true, inS)
                              | (#"3", inS) => (1, 3, true, inS)
                              | (#"4", inS) => (1, 4, true, inS)
                              | (#"5", inS) => (1, 5, true, inS)
                              | (#"6", inS) => (1, 6, true, inS)
                              | (#"7", inS) => (1, 7, true, inS)
                              | (#"8", inS) => (1, 8, true, inS)
                              | (#"9", inS) => (1, 9, true, inS)
                              | _ => error' (InvalidNumber, startInS)
                            (* end case *))
                      fun scanE (inS, seenDigit, exp) = (case next inS
                             of (#"0", inS) => scanE (inS, true, 10 * exp)
                              | (#"1", inS) => scanE (inS, true, 10 * exp + 1)
                              | (#"2", inS) => scanE (inS, true, 10 * exp + 2)
                              | (#"3", inS) => scanE (inS, true, 10 * exp + 3)
                              | (#"4", inS) => scanE (inS, true, 10 * exp + 4)
                              | (#"5", inS) => scanE (inS, true, 10 * exp + 5)
                              | (#"6", inS) => scanE (inS, true, 10 * exp + 6)
                              | (#"7", inS) => scanE (inS, true, 10 * exp + 7)
                              | (#"8", inS) => scanE (inS, true, 10 * exp + 8)
                              | (#"9", inS) => scanE (inS, true, 10 * exp + 9)
                              | _ => if seenDigit
                                  then mkFloat (isNeg, whole, frac, expSign * exp, inS)
                                  else error' (InvalidNumber, startInS)
                            (* end case *))
                      in
                        scanE (inS, seenDigit, exp)
                          handle Overflow => error' (NumberTooLarge, startInS)
                      end
                in
                  if (firstDigit = 0)
                    then (case next inS
                       of (#".", inS) => scanFrac(inS, [])
                        | (#"e", inS) => scanExp(inS, [], [])
                        | (#"E", inS) => scanExp(inS, [], [])
                        | _ => (JSON.INT 0, inS)
                      (* end case *))
                    else scanWhole (inS, [firstDigit])
                end (* scanNumber *)
          (* skip over a C-style comment; the initial '/' has been consumed *)
          and skipComment inS = let
                fun skip inS = (case getc inS
                       of NONE => error'(UnclosedComment, inS)
                        | SOME(#"*", inS) => let
                            (* look for "/" (possibly preceded by stars) *)
                            fun lp inS = (case getc inS
                                  of SOME(#"/", inS) => inS
                                   | SOME(#"*", inS) => lp inS
                                   | SOME(_, inS) => skip inS
                                   | NONE => error'(UnclosedComment, inS)
                                (* end case *))
                            in
                              lp inS
                            end
                        | SOME(_, inS) => skip inS
                      (* end case *))
                in
                  case next inS
                   of (#"*", inS) => skip inS
                    | _ => error'(InvalidCharacter, inS)
                  (* end case *)
                end
          in
            parseValue
          end

    fun parse getc = parseWithOpts defaultOptions getc

  end
