(* float-rep.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure FloatRep : sig

    (* deconstrcted representation of real numbers (precision independent)
     * We interpret the number as
     *
     *    (-1)^sign * d_1 ... d_n * 10^exp
     *)
    type decimal_rep = {
        sign : bool,        (* sign bit; true for negative numbers *)
        nDigits : int,      (* the length of the digit list (> 0) *)
        digits : int list,  (* the non-empty list of decimal digits; these are to
                             * the left of the decimal point (unlike for the
                             * IEEEReal.decimal_approx type, where they are to
                             * the right).
                             *)
        exp : int           (* the signed exponent *)
      }

    datatype float_rep
      = Inf of bool             (* +/- infinity *)
      | NaN of bool             (* +/- NaN *)
      | Zero of bool            (* +/- 0 *)
      | Normal of decimal_rep
      | Subnormal of decimal_rep

    (* Normalize a `float_rep` value for 32-bit IEEE floating-point. *)
    val normalize32 : float_rep -> float_rep

    (* Normalize a `float_rep` value for 64-bit IEEE floating-point. *)
    val normalize64 : float_rep -> float_rep

    (* `toDecimalApprox f` converts the normalized float `f` to its equivalent
     * `IEEEReal.decimal_approx` representation.  Note that we assume that
     * the argument is well formed, since it will be for all use cases in the
     * Basis implementation.
     *)
    val toDecimalApprox : float_rep -> IEEERealTypes.decimal_approx

    (* `fromDecimalApprox approx` converts `approx` to its equivalent
     * `float_rep` representation.  If the input is invalid (i.e., digits
     * that are out of range) then NONE is returned.
     *)
    val fromDecimalApprox : IEEERealTypes.decimal_approx -> float_rep option

  end = struct

    type decimal_rep = {
        sign : bool,        (* sign bit; true for negative numbers *)
        nDigits : int,      (* the length of the digit list *)
        digits : int list,  (* the list of decimal digits; these are two the left
                             * of the decimal point (unlike IEEEReal.decimal_approx,
                             * where they are to the right).
                             *)
        exp : int           (* the signed exponent *)
      }

    datatype float_rep
      = Inf of bool             (* +/- infinity *)
      | NaN of bool             (* +/- NaN *)
      | Zero of bool            (* +/- 0 *)
      | Normal of decimal_rep
      | Subnormal of decimal_rep

    fun mkDecimalRep (digits, exp) : decimal_rep =  {
            sign = false,
            nDigits = List.length digits,
            digits = digits,
            exp = exp
          }

    (* the decimal representation of the minimum 32-bit subnormal number *)
    val minSubnormal32 = (* 0000 0001 *)
          mkDecimalRep ([1,4,0,1,2,9,8,4,6,4,3], ~45)

    (* the decimal representation of the minimum 32-bit normal number *)
    val minNormal32 = (* 0080 0000 *)
          mkDecimalRep ([1,1,7,5,4,9,4,3,5,0,8], ~38)

    (* the decimal representation of the maximum 32-bit normal number *)
    val maxNormal32 = (* 7f7f ffff *)
          mkDecimalRep ([3,4,0,2,8,2,3,4,6,6,4], 38)

    (* the decimal representation of the minimum 64-bit subnormal number *)
    val minSubnormal64 = (* 0000 0000 0000 0001 *)
          mkDecimalRep ([4,9,4,0,6,5,6,4,5,8,4,1,2,4,6,5,4], ~324)

    (* the decimal representation of the minimum 64-bit normal number *)
    val minNormal64 = (* 0010 0000 0000 0000 *)
          mkDecimalRep ([2,2,2,5,0,7,3,8,5,8,5,0,7,2,0,1,4], ~324)

    (* the decimal representation of the maximum 64-bit normal number *)
    val maxNormal64 = (* 7fef ffff ffff ffff *)
          mkDecimalRep ([1,7,9,7,6,9,3,1,3,4,8,6,2,3,1,5,7], 292)

    (* unsigned "<" comparison *)
    fun uLT (dr1 : decimal_rep, dr2 : decimal_rep) = let
          (* compare digits for "<" order *)
          fun ltDigits (d1::dr1, d2::dr2) =
                (d1 < d2) orelse ((d1 = d2) andalso ltDigits(dr1, dr2))
            | ltDigits ([], _::_) = true
            | ltDigits (_, []) = false
          (* normalize exponents *)
          val e1 = #exp dr1 + #nDigits dr1
          val e2 = #exp dr2 + #nDigits dr2
          in
            (e1 < e2) orelse (
              (e1 = e2) andalso ltDigits (#digits dr1, #digits dr2))
          end

    fun lessThan (dr1 : decimal_rep, dr2 : decimal_rep) = (
          case (#sign dr1, #sign dr2)
           of (true, false) => true (* negative < positive *)
            | (false, true) => false (* not(positive < negative) *)
            | (true, true) => uLT (dr2, dr1) (* both negative, so swap order *)
            | (false, false) => uLT (dr1, dr2)
          (* end case *))

    (* normalize a `decimal_rep` value for a precision that is specified by
     * the minimum sub-normal, minimum normal, and maximum normal values.
     *)
    fun normalizeDecimalRep (minSubNormal, minNormal, maxNormal) (f : decimal_rep) = let
          fun trimLeadingZeros (n, 0::ds) = trimLeadingZeros (n+1, ds)
            | trimLeadingZeros arg = arg
          (* remove leading zeros from digits *)
          val (nz1, digits) = trimLeadingZeros (0, #digits f)
          (* remove trailing zeros from digits *)
          val (nz2, rDigits) = trimLeadingZeros (0, List.rev digits)
          val f' = {
                  sign = #sign f,
                  nDigits = #nDigits f - nz1 - nz2,
                  digits = List.rev rDigits,
                  exp = #exp f + nz2
                }
          in
            if (#nDigits f' = 0) then Zero(#sign f')
            else if uLT(f', minSubNormal) then Zero(#sign f')
            else if uLT(f', minNormal) then Subnormal f'
            else if uLT(maxNormal, f') then Inf(#sign f')
            else Normal f'
          end

    fun normalize (minSubNormal, minNormal, maxNormal) = let
          val normalizeDRep = normalizeDecimalRep (minSubNormal, minNormal, maxNormal)
          in
            fn (Normal f) => normalizeDRep f
             | (Subnormal f) => normalizeDRep f
             | f => f
          end

    (* Normalize a `float_rep` value for 32-bit IEEE floating-point. *)
    val normalize32 = normalize (minSubnormal32, minNormal32, maxNormal32)

    (* Normalize a `float_rep` value for 64-bit IEEE floating-point. *)
    val normalize64 = normalize (minSubnormal64, minNormal64, maxNormal64)

    fun toDecimalApprox arg = let
          fun toDA (cls, {sign, nDigits, digits, exp}) =
                { class = cls, sign = sign, digits = digits, exp = exp + nDigits }
          in
            case arg
             of Inf sgn => { class = IEEERealTypes.INF, sign = sgn, digits = [], exp = 0 }
              | NaN sgn => { class = IEEERealTypes.NAN, sign = sgn, digits = [], exp = 0 }
              | Zero sgn => { class = IEEERealTypes.ZERO, sign = sgn, digits = [], exp = 0 }
              | Normal dr => toDA (IEEERealTypes.NORMAL, dr)
              | Subnormal dr => toDA (IEEERealTypes.SUBNORMAL, dr)
            (* end case *)
          end

    fun fromDecimalApprox {class, sign, digits, exp} = let
          fun fromDA cons = let
                (* remove leading zeros from a list *)
                fun rmZ (0::ds, nlz) = (ds, nlz+1)
                  | rmZ (ds, nlz) = (ds, nlz)
                (* first we remove leading zeros *)
                val (digits, nlz) = rmZ (digits, 0)
                (* adjust exponent for removed leading zeros *)
                val exp = exp - nlz
                (* check for invalid digits while counting the digits and removing
                 * trailing zeros.
                 *)
                fun chkDigits ([], n, ds) = let
                      val (ds', nz) = rmZ (ds, 0)
                      in
                        SOME(n - nz, List.rev ds')
                      end
                  | chkDigits (d::dr, n, ds) = if (0 <= d) andalso (d <= 9)
                      then chkDigits (dr, n+1, d::ds)
                      else NONE
                in
                  case chkDigits (digits, 0, [])
                   of NONE => NONE
                    | SOME(0, _) => SOME(Zero sign)
                    | SOME(nDigits, digits) => SOME(cons{
                          sign = sign,
                          nDigits = nDigits,
                          digits = digits,
                          exp = exp - nDigits
                        })
                  (* end case *)
                end
          in
            case class
             of IEEERealTypes.INF => SOME(Inf sign)
              | IEEERealTypes.NAN => SOME(NaN sign)
              | IEEERealTypes.ZERO => SOME(Zero sign)
              | IEEERealTypes.NORMAL => fromDA Normal
              | IEEERealTypes.SUBNORMAL => fromDA Subnormal
            (* end case *)
          end

  end
