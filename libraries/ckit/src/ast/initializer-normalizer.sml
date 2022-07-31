(* Copyright (c) 1999 by Lucent Technologies *)

(* initializer-normalizer-fn.sml
 *
 * AUTHORS: Dino Oliva (oliva@research.bell-labs.com)
 *
 *)

structure InitializerNormalizer : INITIALIZER_NORMALIZER =
struct

  structure Ast = Ast
  structure B = Bindings
  open Ast

  exception NormalizeExn

  fun fail msg = (print msg; raise NormalizeExn)
  (* does this signal an internal "compiler bug"?
   * only acts as a warning, since normalize acts as an identity
   * on the expression if this is called. *)

  fun warn msg = (print msg; ())

  val intCt = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.INT,Ast.SIGNASSUMED)

  val charCt = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.CHAR,Ast.SIGNASSUMED)

(* DBM: the bindAid function introduces new aid mappings in the atab state
 *  component *)

(* this takes the type of a declaration and the initializer and
 * massages the initializer so that it exactly matches the type of
 * declaration.  It is called in BuildAst. *)
fun normalize {lookTid: Tid.uid -> Bindings.tidBinding option,
	       bindAid: Ast.ctype -> Aid.uid,
	       initType: Ast.ctype,
	       initExpr: Ast.initExpression}
     : Ast.initExpression =
let
  fun coreExp2exp (ctype, coreExp) = 
      let val aid = bindAid ctype
       in EXPR (coreExp,aid,SourceMap.UNKNOWN)
      end

  fun mkChrInit c =
      Simple(coreExp2exp (charCt, (IntConst (LargeInt.fromInt (ord c)))))

  fun mkIntInit i = 
      Simple(coreExp2exp (intCt, (IntConst (i:LargeInt.int))))
		  
  fun mkChrs (NONE, []) = []
    | mkChrs (SOME c, []) = [mkChrInit c]
    | mkChrs (cOpt, c::cs) = mkChrInit c :: mkChrs (cOpt, cs)

  (* padding out with zero (via scalarNorm) when too few initializers.
   * as per [ISO-C, p.72-73] *)
  fun arrNorm (arrType, Ast.Qual (_,ctype), maxOp) origInits = (* strip qual *)
            arrNorm (arrType, ctype, maxOp) origInits 
    | arrNorm (arrType, Ast.TypeRef tid, maxOp) origInits = (* dereference type ref *)
	    (case lookTid tid
	       of SOME{ntype = SOME(B.Typedef (tid,ctype)),...} => 
		   arrNorm (arrType, ctype, maxOp) origInits
		| _ => fail "Inconsistent table for type ref")
    | arrNorm (arrType, Ast.Numeric(_,_,_,Ast.CHAR,_), maxOp)
              (Simple(EXPR(StringConst s,aid,loc))::rest) =
       (* special case for character arrays initialized w/strings *)
       let val len = (String.size s) + 1 (* size of c string *)
	   val max = case maxOp of SOME l => LargeInt.toInt l | _ => len
	   val nullOpt = if len = max + 1 then NONE else SOME #"\000"
	   val charInits = mkChrs (nullOpt, explode s)
	in norm(arrType, (Aggregate charInits)::rest)
       end
    | arrNorm (arrType, baseType, maxOp) origInits =
       let val max = case maxOp of
			 SOME l => LargeInt.toInt l
		       | _ => length origInits
	   fun loop(i, inits) = 
	       if (i=max) then ([], inits)
	       else let val (elemInit,remainder) = norm(baseType, inits)
			val (elemInits,remainder') = loop (i+1,remainder)
		     in (elemInit::elemInits, remainder')
		    end
	   val (arrayInits,remainder) = loop(0,origInits)
	in (Aggregate arrayInits, remainder)
       end

  and structNorm (structType, fields) origInits =
      let fun loop [] inits = ([],inits)
	    | loop ((fieldType,NONE,liOpt)::fields) inits =
	       (* according to the standard, unnamed fields don't
		* have initializers.
		*)
	       loop fields inits
	    | loop ((fieldType,pidOpt,liOpt)::fields) inits =
	       let val (fieldInit,remainder) = norm(fieldType, inits)
		   val (fieldInits,remainder') = loop fields remainder
	       in (fieldInit::fieldInits, remainder')
	       end
	  val (structInits,remainder) = loop fields origInits
       in (Aggregate structInits, remainder)
      end

  and unionNorm (unionType, fields) origInits = 
      case fields
	of [] => (warn "Empty union type, initializing to {}";
		  (Aggregate [], origInits))
         | (fieldCtype,member)::_ =>
	     let val (fieldInit,remainder) = norm(fieldCtype, origInits)
	      in (Aggregate [fieldInit], remainder)
	     end
			   
  (* fill in with zeros if you run out of initializers *)
  and scalarNorm ctype origInits =
      case origInits
	of (scalarInit::remainder) => (scalarInit, remainder)
         | [] => let val scalarInit = mkIntInit 0
		 in (scalarInit, [])
		 end
			   
  (* feed supplies its argument initfn with the inits from the first aggregate,
   * if there is one.  The initfn should consume all the inits from the aggregate. *)
  and feed (initfn, (Aggregate elemInits)::inits) =
	    let val (newinit,remainder) = initfn elemInits
	     in case remainder
		  of [] => (newinit, inits)
		   | _ =>
		     (warn "Too many initializers for expression, ignoring extras";
		      (newinit, inits))
	    end
    | feed (initfn, inits) = initfn inits

  and norm (ctype, inits) = 
      case ctype
	of Ast.Qual (_,ctype) => norm(ctype, inits)  (* strip qual *)
         | Ast.TypeRef tid => (* dereference type ref *)
	    (case lookTid tid
	       of SOME{ntype = SOME(B.Typedef (tid,ctype)),...} => 
		   norm(ctype, inits)
		| _ => fail "Inconsistent table for type ref")
	 | Ast.Array (opt,baseType) =>
	       let
		 val lenOp = case opt of SOME(i, _) => SOME i | NONE => NONE
	       in
		 feed (arrNorm(ctype, baseType, lenOp), inits)
	       end
	 | Ast.StructRef tid =>
	    (case lookTid tid
	       of SOME {ntype = SOME(B.Struct(tid,fields)),...} =>
		   feed (structNorm (ctype, fields), inits)
		| SOME _ => fail "Incomplete type for struct ref"
		| NONE => fail "Inconsistent table for struct ref")
	 | Ast.UnionRef tid =>
	    (case lookTid tid
	       of SOME {ntype = SOME(B.Union(tid,fields)),...} =>
		   feed (unionNorm (ctype, fields), inits)
		| SOME _ => fail "Incomplete type for union ref"
		| NONE => fail "Inconsistent table for union ref")
	 | (Ast.Numeric _ | Ast.Pointer _ | Ast.Function _ | Ast.EnumRef _) =>
	    feed (scalarNorm ctype, inits)
	 | Ast.Void => fail "Incomplete type: void"
	 | Ast.Ellipses => fail "Cannot initialize ellipses"
	 | Ast.Error => fail "Cannot initialize error type"

in

  let val (newinit,remainder) = norm(initType, [initExpr])
   in case remainder
	of [] => newinit  (* used them all *)
	 | _ =>
	   (warn "Too many initializers for expression, ignoring extras";
	    newinit)
  end
  handle NormalizeExn => initExpr

end (* END normalize *)

end (* structure InitializerNormalizer *)
