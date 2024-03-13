(* fast-stream-parser.sml
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

structure FastJSONSteamParser : sig

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
        maxDigits : int option
      }

    (* callback functions for the different parsing events *)
    type 'ctx callbacks = {
        (* get a character of input *)
        getc : 'ctx -> (char * 'ctx) option,
        (* 'null' *)
	null : 'ctx -> 'ctx,
        (* 'true' or 'false' *)
	boolean : 'ctx * bool -> 'ctx,
        (* integer literal with no more than `maxDigits` digits *)
	integer : 'ctx * IntInf.int -> 'ctx,
        (* integer literal with more than `maxDigits` digits *)
	intlit : 'ctx * string -> 'ctx,
        (* floating-point literal *)
	float : 'ctx * real -> 'ctx,
        (* string literal *)
	string : 'ctx * string -> 'ctx,
        (* '{' token *)
	startObject : 'ctx -> 'ctx,
        (* object key (including ':') *)
	objectKey : 'ctx * string -> 'ctx,
        (* '}' token *)
	endObject : 'ctx -> 'ctx,
        (* '[' token *)
	startArray : 'ctx -> 'ctx,
        (* ']' token *)
	endArray : 'ctx -> 'ctx,
        (* error handler; given the error code and stream at the point of the error *)
	error : 'ctx * error_code -> unit
      }

    (* parse JSON input *)
    val parseWithOpts : options -> 'ctx callbacks -> 'ctx -> 'ctx

    (* parse using the default options *)
    val parse : 'ctx callbacks -> 'ctx -> 'ctx

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
        maxDigits : int option
      }

    val defaultOptions = {comments=true, maxDigits=SOME 19}

    type 'ctx callbacks = {
        getc : 'ctx -> (char * 'ctx) option,
	null : 'ctx -> 'ctx,
	boolean : 'ctx * bool -> 'ctx,
	integer : 'ctx * IntInf.int -> 'ctx,
	intlit : 'ctx * string -> 'ctx,
	float : 'ctx * real -> 'ctx,
	string : 'ctx * string -> 'ctx,
	startObject : 'ctx -> 'ctx,
	objectKey : 'ctx * string -> 'ctx,
	endObject : 'ctx -> 'ctx,
	startArray : 'ctx -> 'ctx,
	endArray : 'ctx -> 'ctx,
	error : 'ctx * error_code -> unit
      }

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

    fun parse (cb : 'a callbacks) (src as Src{srcMap, strm, ...}, ctx) = let

    fun parse (cb : 'a callbacks) ctx = let
          val getc = #getc cb
          fun error' (ec, ctx) = (
                #error cb (ec, ctx);
                raise SyntaxError(errorMessage ec))
          val maxDigits = Option.getOpt (maxDigits, defaultMaxDigits)
          fun next ctx = (case getc ctx
                 of SOME(c, ctx') => (c, ctx')
                  | NONE => (#"\000", ctx)
                (* end case *))
          (* skip white space *)
          fun skipWS ctx = (case next ctx
                 of (#" ", ctx) => skipWS ctx
                  | (#"\t", ctx) => skipWS ctx
                  | (#"\r", ctx) => skipWS ctx
                  | (#"\n", ctx) => skipWS ctx
                  | res => res
                (* end case *))
          fun matchC (ctx, c) = let
                val (c', ctx') = next ctx
                in
                  if (c = c')
                    then ctx'
                    else error'(InvalidLiteral, ctx)
                end
          (* parse a JSON value *)
          fun parseValue ctx = (case skipWS ctx
                 of (#"[", ctx) => parseArray ctx
                  | (#"{", ctx) => parseObject ctx
                  | (#"-", ctx) => (case next ctx
                       of (#"0", ctx) => scanNumber(ctx, true, #"0")
                        | (#"1", ctx) => scanNumber(ctx, true, #"1")
                        | (#"2", ctx) => scanNumber(ctx, true, #"2")
                        | (#"3", ctx) => scanNumber(ctx, true, #"3")
                        | (#"4", ctx) => scanNumber(ctx, true, #"4")
                        | (#"5", ctx) => scanNumber(ctx, true, #"5")
                        | (#"6", ctx) => scanNumber(ctx, true, #"6")
                        | (#"7", ctx) => scanNumber(ctx, true, #"7")
                        | (#"8", ctx) => scanNumber(ctx, true, #"8")
                        | (#"9", ctx) => scanNumber(ctx, true, #"9")
                        | _ => error'(InvalidCharacter, ctx)
                      (* end case *))
                  | (#"0", ctx) => scanNumber(ctx, false, #"0")
                  | (#"1", ctx) => scanNumber(ctx, false, #"1")
                  | (#"2", ctx) => scanNumber(ctx, false, #"2")
                  | (#"3", ctx) => scanNumber(ctx, false, #"3")
                  | (#"4", ctx) => scanNumber(ctx, false, #"4")
                  | (#"5", ctx) => scanNumber(ctx, false, #"5")
                  | (#"6", ctx) => scanNumber(ctx, false, #"6")
                  | (#"7", ctx) => scanNumber(ctx, false, #"7")
                  | (#"8", ctx) => scanNumber(ctx, false, #"8")
                  | (#"9", ctx) => scanNumber(ctx, false, #"9")
                  | (#"\"", ctx) => scanStringValue ctx
                  | (#"f", ctx) => let (* match "a" "l" "s" "e" *)
                      val ctx = matchC (ctx, #"a")
                      val ctx = matchC (ctx, #"l")
                      val ctx = matchC (ctx, #"s")
                      val ctx = matchC (ctx, #"e")
                      in
                        #boolean cb (ctx, false)
                      end
                  | (#"n", ctx) => let (* match "u" "l" "l" *)
                      val ctx = matchC (ctx, #"u")
                      val ctx = matchC (ctx, #"l")
                      val ctx = matchC (ctx, #"l")
                      in
                        #null cb ctx
                      end
                  | (#"t", ctx) => let (* match "r" "u" "e" *)
                      val ctx = matchC (ctx, #"r")
                      val ctx = matchC (ctx, #"u")
                      val ctx = matchC (ctx, #"e")
                      in
                        #boolean cb (ctx, true)
                      end
                  | (#"/", ctx) => if comments
                      then parseValue (skipComment ctx)
                      else error'(CommentsNotAllowed, ctx)
                  | _ => error'(InvalidCharacter, ctx)
                (* end case *))
          (* parse a JSON array assuming that the '[' has been consumed *)
          and parseArray ctx = let
                (* loop to scan one or more items *)
                fun lp ctx = let
                        val ctx = parseValue ctx
                        in
                          case skipWS ctx
                           of (#",", ctx) => lp (ctx, items)
                            | (#"]", ctx) => #endArray cb ctx
                            | _ => error'(InvalidArray, ctx)
                          (* end case *)
                        end
                val ctx = #startArray cb ctx
                in
                  case skipWS ctx
                   of (#"]", ctx) => #endArray cb ctx
                    | _ => lp (ctx, [])
                  (* end case *)
                end
          (* parse a JSON object assuming that the '{' has been consumed *)
          and parseObject ctx = let
                (* loop to scan one or more key-value pairs *)
                fun lp (ctx, items) = (case skipWS ctx
                       of (#"\"", ctx) => let
                            val (key, ctx) = scanString ctx
                            in
                              case skipWS ctx
                               of (#":", ctx) => let
                                    val (v, ctx) = parseValue ctx
                                    val items = (key, v)::items
                                    in
                                      case skipWS ctx
                                       of (#",", ctx) => lp (ctx, items)
                                        | (#"}", ctx) => (JSON.OBJECT(reverse items), ctx)
                                        | _ => error'(InvalidObject, ctx)
                                      (* end case *)
                                    end
                                | _ => error'(ExpectedColon, ctx)
                              (* end case *)
                            end
                        | _ => error'(ExpectedKey, ctx)
                      (* end case *))
		val ctx = #startObject cb ctx
                in
                  case skipWS ctx
                   of (#"}", ctx) => #endObject cb ctx
                    | _ => lp (ctx, [])
                  (* end case *)
                end
          (* scan a string value assuming that the first quote has been consumed *)
          and scanString start = let
                fun c2w c = W.fromInt(ord c)
                fun w2c w = Char.chr(W.toInt w)
                fun scan (ctx, n, cs) = (case next ctx
                       of (#"\000", _) => error'(UnclosedString, start)
                        | (#"\"", ctx) => (mkString(n, cs), ctx)
                        | (#"\\", ctx) => scanEscape (ctx, n, cs)
                        | (c, ctx) => if (#" " <= c) andalso (c < #"\127")
                            (* printable ASCII character *)
                            then scan (ctx, inc n, c::cs)
                            (* either non-printable ASCII or UTF-8 byte sequence *)
                            else scanUTF8 (ctx, c, c2w c, n, cs)
                      (* end case *))
                and scanEscape (ctx, n, cs) = let
                      fun return (ctx, c) = scan (ctx, inc n, c::cs)
                      in
                        case next ctx
                         of (#"\"", ctx) => return (ctx, #"\"")
                          | (#"\\", ctx) => return (ctx, #"\\")
                          | (#"/", ctx) => return (ctx, #"/")
                          | (#"b", ctx) => return (ctx, #"\008") (* backspace *)
                          | (#"f", ctx) => return (ctx, #"\012") (* form feed *)
                          | (#"n", ctx) => return (ctx, #"\010") (* line feed *)
                          | (#"r", ctx) => return (ctx, #"\013") (* carriage return *)
                          | (#"t", ctx) => return (ctx, #"\009") (* tab *)
                          | (#"u", ctx) => scanUnicodeEscape (ctx, n, cs)
                          | _ => error'(InvalidEscape, ctx)
                        (* end case *)
                      end
                (* scan a Unicode escape sequence; we have already consumed the "\u"
                 * prefix, so we just need to parse the four hex digits followed by
                 * a possible second escape sequence for a surrogate pair.  The result
                 * is encoded as a UTF-8 byte sequence.
                 *)
                and scanUnicodeEscape (ctx, n, cs) = let
                      fun getDigit ctx = (case next ctx
                             of (#"0", ctx) => (0w0, ctx)
                              | (#"1", ctx) => (0w1, ctx)
                              | (#"2", ctx) => (0w2, ctx)
                              | (#"3", ctx) => (0w3, ctx)
                              | (#"4", ctx) => (0w4, ctx)
                              | (#"5", ctx) => (0w5, ctx)
                              | (#"6", ctx) => (0w6, ctx)
                              | (#"7", ctx) => (0w7, ctx)
                              | (#"8", ctx) => (0w8, ctx)
                              | (#"9", ctx) => (0w9, ctx)
                              | (#"a", ctx) => (0w10, ctx)
                              | (#"A", ctx) => (0w10, ctx)
                              | (#"b", ctx) => (0w11, ctx)
                              | (#"B", ctx) => (0w11, ctx)
                              | (#"c", ctx) => (0w12, ctx)
                              | (#"C", ctx) => (0w12, ctx)
                              | (#"d", ctx) => (0w13, ctx)
                              | (#"D", ctx) => (0w13, ctx)
                              | (#"e", ctx) => (0w14, ctx)
                              | (#"E", ctx) => (0w14, ctx)
                              | (#"f", ctx) => (0w15, ctx)
                              | (#"F", ctx) => (0w15, ctx)
                              | _ => error'(InvalidUnicodeEscape, ctx)
                            (* end case *))
                      fun getDigits ctx = let
                            (* get four digits *)
                            val (d0, ctx) = getDigit ctx
                            val (d1, ctx) = getDigit ctx
                            val (d2, ctx) = getDigit ctx
                            val (d3, ctx) = getDigit ctx
                            val n = W.<<(d0, 0w24)
                                  + W.<<(d1, 0w16)
                                  + W.<<(d2, 0w8)
                                  + d3
                            in
                              (n, ctx)
                            end
                      val (u0, ctx) = getDigits ctx
                      (* get the second 16-bit code point of a surrogate pair *)
                      fun scanLowSurrogate ctx = (
                           (* match "\uxxxx" *)
                            case next ctx
                             of (#"\\", ctx) => (case next ctx
                                   of (#"u", ctx) => let
                                        val (u1, ctx) = getDigits ctx
                                        in
                                          if (u1 < 0wxDC00) orelse (0wxDFFF < u1)
                                            then error'(InvalidUnicodeSurrogatePair, ctx)
                                            (* convert pair to a Unicode code point
                                             * and then to UTF-8 bytes.
                                             *)
                                            else toUTF8 (ctx,
                                              0wx10000
                                                + W.<<(u0 - 0wxD800, 0w10)
                                                + (u1 - 0wxDC00))
                                        end
                                    | _ => error'(InvalidUnicodeSurrogatePair, ctx)
                                  (* end case *))
                              | _ => error'(InvalidUnicodeSurrogatePair, ctx)
                            (* end case *))
                      (* convert a word to a UTF-8 sequence *)
                      and toUTF8 (ctx, w) = if (w <= 0wx7f)
                              then scan (ctx, inc n, w2c w :: cs)
                            else if (w <= 0wx7ff)
                              then scan (ctx,
                                n+2,
                                w2c(W.orb(0wxc0, W.>>(w, 0w6)))
                                  :: w2c(W.orb(0wx80, W.andb(w, 0wx3f)))
                                  :: cs)
                            else if (w <= 0wxffff)
                              then scan (ctx,
                                n+3,
                                w2c(W.orb(0wxe0, W.>>(w, 0w12)))
                                  :: w2c(W.orb(0wx80, W.andb(W.>>(w, 0w6), 0wx3f)))
                                  :: w2c(W.orb(0wx80, W.andb(w, 0wx3f)))
                                  :: cs)
                            else if (w <= 0wx10ffff)
                              then scan (ctx,
                                n+4,
                                w2c(W.orb(0wxf0, W.>>(w, 0w18)))
                                  :: w2c(W.orb(0wx80, W.andb(W.>>(w, 0w12), 0wx3f)))
                                  :: w2c(W.orb(0wx80, W.andb(W.>>(w, 0w6), 0wx3f)))
                                  :: w2c(W.orb(0wx80, W.andb(w, 0wx3f)))
                                  :: cs)
                              else error'(InvalidUnicodeEscape, ctx)
                      in
                        if (u0 < 0wxD800)
                          then toUTF8 (ctx, u0)
                        else if (u0 <= 0wxDBFF)
                          then scanLowSurrogate ctx
                          else error'(InvalidUnicodeEscape, ctx)
                      end (* scanUnicodeEscape *)
                (* a simple state machine for getting a valid UTF-8 byte sequence.  See
                 * https://unicode.org/mail-arch/unicode-ml/y2003-m02/att-0467/01-The_Algorithm_to_Valide_an_UTF-8_String
                 * for a description of the state machine.
                 *)
                and scanUTF8 (ctx, chr0, byte0, n, cs) = let
                      fun getByte ctx = (case next ctx
                             of (#"\000", _) => error'(IncompleteUTF8, ctx)
                              | (c, ctx') => (c2w c, c, ctx')
                            (* end case *))
                      fun inRange (minB : word, b, maxB) = ((b - minB) <= maxB - minB)
                      (* handles last byte for all multi-byte sequences *)
                      fun stateA (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wx80, b, 0wxbf)
                                then scan (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      (* handles second/third byte for three/four-byte sequences *)
                      and stateB (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wx80, b, 0wxbf)
                                then stateA (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      (* byte0 = 0b1110_0000 (3-byte sequence) *)
                      and stateC (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wxa0, b, 0wxbf)
                                then stateA (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      (* byte0 = 0b1110_1101 (3-byte sequence) *)
                      and stateD (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wx80, b, 0wx9f)
                                then stateA (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      (* byte0 = 0b1111_0001 .. 0b1111_0011 (4-byte sequence) *)
                      and stateE (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wx80, b, 0wxbf)
                                then stateB (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      (* byte0 = 0b1111_0000 (4-byte sequence) *)
                      and stateF (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wx90, b, 0wxbf)
                                then stateB (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      (* byte0 = 0b1111_1000 (4-byte sequence) *)
                      and stateG (ctx, n, chrs) = let
                            val (b, c, ctx) = getByte ctx
                            in
                              if inRange(0wx80, b, 0wx8f)
                                then stateB (ctx, inc n, c::chrs)
                                else error'(InvalidUTF8, ctx)
                            end
                      in
                        if (byte0 <= 0wx7f)
                          (* this case only occurs for non-printing ASCII characters *)
                          then error'(NonPrintingASCII, ctx)
                        else if inRange(0wxc2, byte0, 0wxdf)
                          then stateA (ctx, n, cs)
                        else if inRange(0wxe1, byte0, 0wxec)
                        orelse inRange(0wxee, byte0, 0wxef)
                          then stateB (ctx, n, cs)
                        else if (byte0 = 0wxe0)
                          then stateC (ctx, n, cs)
                        else if (byte0 = 0wxed)
                          then stateD (ctx, n, cs)
                        else if inRange(0wxf1, byte0, 0wxf3)
                          then stateE (ctx, n, cs)
                        else if (byte0 = 0wxf0)
                          then stateF (ctx, n, cs)
                        else if (byte0 = 0wxf4)
                          then stateG (ctx, n, cs)
                          else error'(InvalidUTF8, ctx)
                      end (* scanUTF8 *)
                in
                  scan (start, 0, [])
                end (* scanString *)
          and scanStringValue ctx = let
                val (s, ctx) = scanString ctx
                in
                  (JSON.STRING s, ctx)
                end
          (* scan an integer or floating-point number.  If the number of digits
           * for an integer literal exceeds the `maxDigits` limit, then we return
           * an `INTLIT` value (instead of an `INT`).
           *)
          and scanNumber (ctx, isNeg, first) = let
                (* scan an integer or the whole part of a float *)
                fun scanWhole (ctx, n, digits) = (case next ctx
                       of (#"0", ctx) => scanWhole (ctx, inc n, #"0"::digits)
                        | (#"1", ctx) => scanWhole (ctx, inc n, #"1"::digits)
                        | (#"2", ctx) => scanWhole (ctx, inc n, #"2"::digits)
                        | (#"3", ctx) => scanWhole (ctx, inc n, #"3"::digits)
                        | (#"4", ctx) => scanWhole (ctx, inc n, #"4"::digits)
                        | (#"5", ctx) => scanWhole (ctx, inc n, #"5"::digits)
                        | (#"6", ctx) => scanWhole (ctx, inc n, #"6"::digits)
                        | (#"7", ctx) => scanWhole (ctx, inc n, #"7"::digits)
                        | (#"8", ctx) => scanWhole (ctx, inc n, #"8"::digits)
                        | (#"9", ctx) => scanWhole (ctx, inc n, #"9"::digits)
                        | (#".", ctx) => scanFrac (ctx, digits)
                        | (#"e", ctx) => scanExp (ctx, digits, [])
                        | (#"E", ctx) => scanExp (ctx, digits, [])
                        | _ => if (n < maxDigits)
                              then let
                                fun cvt ([], k) = (JSON.INT(if isNeg then ~k else k), ctx)
                                  | cvt (d::ds, k) =
                                      cvt (ds, 10*k + IntInf.fromInt(ord d - ord #"0"))
                                in
                                  cvt (reverse digits, 0)
                                end
                            else if isNeg
                              then let
                                val s = mkString(inc n, digits)
                                in
                                  (* add the negative sign *)
                                  Unsafe.CharVector.update(s, 0, #"-");
                                  (JSON.INTLIT s, ctx)
                                end
                              else (JSON.INTLIT(mkString(n, digits)), ctx)
                      (* end case *))
                (* scan the fractional part of a real; the '.' has already been
                 * consumed.
                 *)
                and scanFrac (ctx, whole) = raise Fail "scanFrac"
                (* scan the exponent part of a real; the "e"/"E" has already been
                 * consumed.
                 *)
                and scanExp (ctx, whole, frac) = raise Fail "scanExp"
                in
                  if (first = #"0")
                    then (case next ctx
                       of (#".", ctx) => scanFrac(ctx, [#"0"])
                        | (#"e", ctx) => scanExp(ctx, [#"0"], [])
                        | (#"E", ctx) => scanExp(ctx, [#"0"], [])
                        | _ => (JSON.INT 0, ctx)
                      (* end case *))
                    else scanWhole (ctx, 1, [first])
                end
          (* skip over a C-style comment; the initial '/' has been consumed *)
          and skipComment ctx = let
                fun skip ctx = (case getc ctx
                       of NONE => error'(UnclosedComment, ctx)
                        | SOME(#"*", ctx) => let
                            (* look for "/" (possibly preceded by stars) *)
                            fun lp ctx = (case getc ctx
                                  of SOME(#"/", ctx) => ctx
                                   | SOME(#"*", ctx) => lp ctx
                                   | SOME(_, ctx) => skip ctx
                                   | NONE => error'(UnclosedComment, ctx)
                                (* end case *))
                            in
                              lp ctx
                            end
                        | SOME(_, ctx) => skip ctx
                      (* end case *))
                in
                  case next ctx
                   of (#"*", ctx) => skip ctx
                    | _ => error'(InvalidCharacter, ctx)
                  (* end case *)
                end
          in
            parseValue
          end

    fun parse getc = parseWithOpts {
            comments=true,
            maxDigits=SOME 16,
            error = fn (ec, _) => raise SyntaxError(errorMessage ec)
          } getc

  end
