(* overloadvar.sml
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature OVERLOADVAR =
sig
    val symToScheme : Symbol.symbol -> Types.tyfun
    val defaultTy : Symbol.symbol -> Types.ty
    val resolveVar : Symbol.symbol * Types.ty * Variable.var list -> Variable.var option
end

structure OverloadVar : OVERLOADVAR =
struct

local
    structure S = Symbol
    structure T = Types
    structure BT = BasicTypes
    structure TU = TypesUtil
    structure V = Variable
    structure OLC = OverloadClasses
in

fun bug msg = ErrorMsg.impossible("OverloadVar: "^msg)

(* overloaded operator symbols *)

val negation = S.varSymbol "~"
val plus = S.varSymbol "+"
val minus = S.varSymbol "-"
val times = S.varSymbol "*"
val divsym = S.varSymbol "div"
val modsym = S.varSymbol "mod"
val less = S.varSymbol "<"
val lessEqual = S.varSymbol "<="
val greater = S.varSymbol ">"
val greaterEqual = S.varSymbol ">="
val abssym = S.varSymbol "abs"

(* type schemes *)

val alpha = T.IBOUND 0
val unary = BT.--> (alpha, alpha)
val binary = BT.--> (BT.tupleTy [alpha,alpha], alpha)
val relation = BT.--> (BT.tupleTy [alpha,alpha], BT.boolTy)

val unaryScheme = T.TYFUN{arity = 1, body = unary}
val binaryScheme = T.TYFUN{arity = 1, body = binary}
val relationScheme = T.TYFUN{arity = 1, body = relation}

(* The order of variants declared in the overload declarations (and hence the variant
 * lists) are consistent with the order of primitive types in the overloading class
 * associtated with that operator symbol, because the orderings of classes are derived
 * from the order of variants (which are consistent among related groups of overloaded
 * operators, like { ~, +, -, * }). *)

type entry = S.symbol * T.tyfun * OLC.class

val overloadTable : entry list =
    [(negation,     unaryScheme,      OLC.numClass),
     (plus,         binaryScheme,     OLC.numClass),
     (minus,        binaryScheme,     OLC.numClass),
     (times,        binaryScheme,     OLC.numClass),
     (divsym,       binaryScheme,     OLC.int_wordClass),
     (modsym,       binaryScheme,     OLC.int_wordClass),
     (less,         relationScheme,   OLC.num_textClass),
     (lessEqual,    relationScheme,   OLC.num_textClass),
     (greater,      relationScheme,   OLC.num_textClass),
     (greaterEqual, relationScheme,   OLC.num_textClass),
     (abssym,       unaryScheme,      OLC.int_realClass)]

fun lookup (s : S.symbol) : entry option =
    let fun look ((entry as (s',_,_)) :: rest) =
	    if Symbol.eq(s,s') then SOME entry
	    else look rest
          | look nil = NONE
    in look overloadTable
    end

fun symToScheme (s: S.symbol) : T.tyfun =
    case Option.map #2 (lookup s)
     of SOME scheme => scheme
      | NONE => bug "symToScheme"

fun symToClass (s: S.symbol) : OLC.class =
    case Option.map #3 (lookup s)
     of SOME class => class
      | NONE => bug "symToClass"

fun defaultTy (s: S.symbol) : T.ty =
    case symToClass s
     of ty :: _ => ty
      | nil => bug "defaultTy"

fun resolveVar (name: S.symbol, indicator: T.ty, variants) : V.var option =
    let fun getVariant (indicator: T.ty, class, variants) : V.var option =
            (* ASSERT: length class = length variants *)
	    let fun get (ty1::restTy, v1::restVariants) =
		    if TU.equalType(indicator, ty1) then SOME v1
		    else get(restTy,restVariants)
		  | get (nil,nil) = NONE
		  | get _ = bug "getVariant"
	    in get (class, variants)
	    end
    in getVariant (indicator, symToClass name, variants)
    end

end (* local *)
end (* structure OverloadClasses *)
