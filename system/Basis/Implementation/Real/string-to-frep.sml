(* string-to-frep.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure StringToFRep : sig

    (* scan a real number from a character stream.  Leading and trailing zeros
     * are removed from the representation, but identification of sub-normals
     * and infinities is left for later, since that depends on the target
     * precision.
     *)
    val scan : (char, 'strm) StringCvt.reader
          -> (FloatRep.float_rep, 'strm) StringCvt.reader

  end = struct

    structure W = InlineT.Word
    structure U = ScanUtil

    datatype float_rep = datatype FloatRep.float_rep

    (* the largest IEEE floating-point format is 256 bits (octuple-precision),
     * so we limit exponents to valid values for that precision.  Doing so means
     * that we do not have to worry about overflow in down-stream exponent adjustments.
     *)
    val minExp = ~262142
    val maxExp = 262143
    val infExp = maxExp+1       (* bigger than the largest 256-bit exponent *)

    (* codes copied from ScanUtil *)
    val plusCode : word = 0w129		(* code for #"+" *)
    val minusCode : word = 0w130	(* code for #"-" and #"~" *)
    val ptCode : word = 0w131		(* code for #"." *)
    val eCode : word = 0w14		(* code for #"e" and #"E" *)

    (* suffixes for special reals *)
    val nfChrs = [#"n",#"f"]                    (* [i]nf *)
    val inityChrs = [#"i",#"n",#"i",#"t",#"y"]  (* [inf]inity *)
    val anChrs = [#"a", #"n"]                   (* [n]an *)

    fun scan getc = let
          (* trim leading zeros from a list of digits *)
          fun trimLeadingZeros (n, 0::ds) = trimLeadingZeros (n+1, ds)
            | trimLeadingZeros arg = arg
          (* scan the optional sign *)
          fun scanSign cs = (case getc cs
                 of SOME(#"~", cs) => (true, cs)
                  | SOME(#"-", cs) => (true, cs)
                  | SOME(#"+", cs) => (false, cs)
                  | _ => (false, cs)
                (* end case *))
          fun scanWhole (cs, sign, n, digits) = (case getc cs
                 of SOME(c, cs') => let
                      val d = U.code c
                      in
                        if (d <= 0w9)
                          then scanWhole (cs', sign, n+1, W.toIntX d :: digits)
                        else if (d = ptCode)
                          then scanFrac (cs', sign, n, 0, digits)
                        else if (d = eCode)
                          then scanExp (cs, cs', sign, n, 0, digits)
                          else mkNoExp (cs, sign, n, 0, digits)
                      end
                  | NONE => mkNoExp (cs, sign, n, 0, digits)
                (* end case *))
          and scanFrac (cs, mSign, nWhole, nFrac, digits) = (case getc cs
                 of SOME(c, cs') => let
                      val d = U.code c
                      in
                        if (d <= 0w9)
                          then scanFrac (cs', mSign, nWhole, nFrac+1, W.toIntX d :: digits)
                        else if (d = eCode)
                          then scanExp (cs, cs', mSign, nWhole, nFrac, digits)
                          else mkNoExp (cs, mSign, nWhole, nFrac, digits)
                      end
                  | NONE => mkNoExp (cs, mSign, nWhole, nFrac, digits)
                (* end case *))
          and scanExp (eIx, cs, mSign, nWhole, nFrac, digits) = let
                val (eSign, cs) = scanSign cs
                fun scan (cs, eDigits) = (case getc cs
                       of SOME(c, cs') => let
                            val d = U.code c
                            in
                              if (d <= 0w9)
                                then scan (cs', W.toIntX d :: eDigits)
                                else withExponent (cs, eDigits)
                            end
                        | NONE => withExponent (cs, eDigits)
                      (* end case *))
                and withExponent (cs, eDigits) =
                      mkWithExp (cs, mSign, nWhole, nFrac, digits, eSign, eDigits)
                fun noExponent () = (* invalid exponent, so backtrack the stream *)
                      mkNoExp (eIx, mSign, nWhole, nFrac, digits)
                in
                  case getc cs
                   of SOME(c, cs') => let
                        val d = U.code c
                        in
                          if (d <= 0w9)
                            then scan (cs', [W.toIntX d])
                            else noExponent ()
                        end
                    | NONE => noExponent ()
                  (* end case *)
                end
          (* make a number w/o an exponent.  The arguments are:
           *   cs       -- the remaing stream
           *   mSign    -- sign of the mantissa
           *   nWhole   -- number of whole digits (i.e., left of decimal)
           *   nFrac    -- number of fractional digits (i.e., right of decimal)
           *   rDigits   -- the digits in reverse order
           *)
          and mkNoExp (cs, mSign, nWhole, nFrac, rDigits) =
                mk (cs, mSign, nWhole + nFrac, rDigits, ~nFrac)
          (* make a number with an exponent.  The first five arguments are the same
           * as `mkNoExp`; the additional arguments are:
           *   eSign    -- the sign of the exponent
           *   eDigits  -- the exponent digits in reverse order
           *)
          and mkWithExp (cs, mSign, nWhole, nFrac, rDigits, eSign, eDigits) = let
                val exp = (List.foldr (fn (d, e) => 10*e + d) 0 eDigits
                            handle _ => infExp)
                val exp = if eSign then ~exp else exp
                in
                  mk (cs, mSign, nWhole + nFrac, rDigits, exp-nFrac)
                end
          (* make the result.  The arguments are:
           *   cs       -- the remaing stream
           *   mSign    -- sign of the mantissa
           *   nDigits  -- the total number of digits (including excess zeros)
           *   rDigits  -- the digits in reverse order
           *   exp      -- the adjusted exponent
           *)
          and mk (cs, sign, nDigits, rDigits, exp) = let
                val (nz1, rDigits) = trimLeadingZeros (0, rDigits)
                val (nz2, digits) = trimLeadingZeros (0, List.rev rDigits)
                val nDigits = nDigits - nz1 - nz2
                val exp = exp + nz1
                val frep = if (nDigits = 0) orelse (exp < minExp)
                        then Zero sign
                      else if (exp > maxExp)
                        then Inf sign
                        else Normal{
                            sign = sign,
                            nDigits = nDigits,
                            digits = digits,
                            exp = exp
                          }
                in
                  SOME(frep, cs)
                end
          (* scan special reals: "inf"|"infinity"|"nan".
           * Note that the names are case insesitive.
           *)
          fun scanSpecial (sign, c, cs) = let
                fun match (cs, []) = SOME cs
                  | match (cs, c::cr) = (case getc cs
                       of SOME(c', cs') => if (c = Char.toLower c')
                            then match (cs', cr)
                            else NONE
                        | NONE => NONE
                      (* end case *))
                fun infinity cs = SOME(Inf sign, cs)
                fun nan cs = SOME(NaN sign, cs)
                in
                  case c
                   of (#"I" | #"i") => (case match (cs, nfChrs)
                         of SOME cs' => (case match (cs', inityChrs)
                               of SOME cs'' => infinity cs''
                                | NONE => infinity cs'
                              (* end case *))
                          | NONE => NONE
                        (* end case *))
                    | (#"N" | #"n") => (case match (cs, anChrs)
                         of SOME cs => nan cs
                          | NONE => NONE
                        (* end case *))
                    | _ => NONE
                  (* end case *)
                end
          in
            fn cs => let
                val cs = StringCvt.skipWS getc cs
                val (sign, cs) = scanSign cs
                in
                  case getc cs
                   of SOME(#".", cs') => (case getc cs'
                         of SOME(c, cs'') => let
                              val d = U.code c
                              in
                                if (d <= 0w9)
                                  then scanFrac (cs'', sign, 0, 1, [W.toIntX d])
                                  else NONE
                              end
                          | NONE => NONE
                        (* end case *))
                    | SOME(c, cs') => let
                        val d = U.code c
                        in
                          if (d <= 0w9)
                            then scanWhole (cs', sign, 1, [W.toIntX d])
                            else scanSpecial (sign, c, cs')
                        end
                    | NONE => NONE
                  (* end case *)
                end
          end

  end
