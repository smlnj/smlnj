(* Copyright (c) 1998 by Lucent Technologies *)

(* equality for ctype datatype (defined in ast.sml) *)
structure CTypeEq =
struct
  open Ast
  fun eqStorageClass(AUTO, AUTO) = true
    | eqStorageClass(EXTERN, EXTERN) = true
    | eqStorageClass(REGISTER, REGISTER) = true
    | eqStorageClass(STATIC, STATIC) = true
    | eqStorageClass(DEFAULT, DEFAULT) = true
    | eqStorageClass _ = false

  fun eqQualifier(CONST, CONST) = true
    | eqQualifier(VOLATILE, VOLATILE) = true
    | eqQualifier _ = false
      
  fun eqSignedness(SIGNED, SIGNED) = true
    | eqSignedness(UNSIGNED, UNSIGNED) = true
    | eqSignedness _ = false

  fun eqIntKind(CHAR, CHAR) = true
    | eqIntKind(SHORT, SHORT) = true
    | eqIntKind(INT, INT) = true
    | eqIntKind(LONG, LONG) = true
    | eqIntKind(LONGLONG, LONGLONG) = true
    | eqIntKind(FLOAT, FLOAT) = true
    | eqIntKind(DOUBLE, DOUBLE) = true
    | eqIntKind(LONGDOUBLE, LONGDOUBLE) = true
    | eqIntKind _ = false

  fun eqFractionality(FRACTIONAL, FRACTIONAL) = true
    | eqFractionality(WHOLENUM, WHOLENUM) = true
    | eqFractionality _ = false

  fun eqSaturatedness(SATURATE, SATURATE) = true
    | eqSaturatedness(NONSATURATE, NONSATURATE) = true
    | eqSaturatedness _ = false

  fun eqCType(Void, Void) = true
    | eqCType(Ellipses, Ellipses) = true
    | eqCType(Qual(q1, ct1), Qual(q2, ct2)) = eqQualifier(q1, q2) andalso eqCType(ct1, ct2)
    | eqCType(Numeric x1, Numeric x2) = (x1 = x2)
    | eqCType(Array(SOME(i1, _), ct1), Array(SOME(i2, _), ct2)) = (i1=i2) andalso eqCType(ct1, ct2)
    | eqCType(Pointer ct1, Pointer ct2) = eqCType(ct1, ct2)
    | eqCType(Function(ct1, ctl1), Function(ct2, ctl2)) = eqCType(ct1, ct2) andalso eqCTypeList(ctl1, ctl2)
    | eqCType(StructRef tid1, StructRef tid2) = (tid1 = tid2)
    | eqCType(UnionRef tid1, UnionRef tid2) = (tid1 = tid2)
    | eqCType(EnumRef tid1, EnumRef tid2) = (tid1 = tid2)
    | eqCType(TypeRef tid1, TypeRef tid2) = (tid1 = tid2)
    | eqCType(Error, Error) = true
    | eqCType _ = false
  
 and eqCTypeList((ct1, _) :: ctl1, (ct2, _) :: ctl2) =
     eqCType(ct1, ct2) andalso eqCTypeList(ctl1, ctl2)
   | eqCTypeList(nil, nil) = true
   | eqCTypeList _ = false

end