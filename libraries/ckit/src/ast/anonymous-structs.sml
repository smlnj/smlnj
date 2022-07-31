(* anonymous-structs.sml *)
(* implements structure equality for unions, structs, enums, at the level of ParseTree *)

structure TyEq =
struct
  
  local open ParseTree in
  
  fun eqList eq (x :: xl, y :: yl) = eq(x, y) andalso eqList eq (xl, yl)
    | eqList eq (nil, nil) = true
    | eqList _ _ = false
  
  fun eqPair (eq1, eq2) ((x1, x2), (y1, y2)) = eq1(x1, y1) andalso eq2(x2, y2)
  
  fun eqString (x:string, y) = (x = y)

  fun eqDeclarator(EmptyDecr,EmptyDecr) = true
    | eqDeclarator(EllipsesDecr,EllipsesDecr) = true
    | eqDeclarator(VarDecr s1,VarDecr s2) = (s1 = s2)
    | eqDeclarator(ArrayDecr(d1,e1), ArrayDecr(d2,e2)) =
        eqDeclarator(d1,d2) andalso eqExpr(e1,e2)
    | eqDeclarator(PointerDecr d1,PointerDecr d2) = eqDeclarator(d1,d2)
    | eqDeclarator(QualDecr(q1,d1), QualDecr(q2,d2)) =
	q1 = q2 andalso eqDeclarator(d1,d2)
    | eqDeclarator(FuncDecr arg1, FuncDecr arg2) =
	eqPair(eqDeclarator, eqList(eqPair(eqDecltype, eqDeclarator)))
        (arg1,arg2)
    | eqDeclarator _ = false  (* fix this *)

  and eqDecltype (x: decltype, y) =  (* not an equality type.  Why? *)
      raise Fail "eqDecltype not implemented"

  and eqCtype (x:ctype, y) = 
      raise Fail "eqCtype not implemented"
      (* (x = y) not an equality type?  Why? *)
  
  and eqExpOp (x: operator, y) =
      raise Fail "eqExpOp not implemented"
      (* (x = y) not an equality type?  Why? *)

  and eqExpr(EmptyExpr, EmptyExpr) = true
    | eqExpr(IntConst i, IntConst j) = (i=j)
    | eqExpr(RealConst i, RealConst j) =
       (case Real.compare(i,j) of EQUAL => true | _ => false)
    | eqExpr(String i, String j) = (i=j)
    | eqExpr(Id i, Id j) = (i = j)
    | eqExpr(Unop(expOp, expr), Unop(expOp', expr')) =
       eqExpOp(expOp,expOp') andalso eqExpr(expr, expr')
    | eqExpr(Binop(expOp, expr1, expr2), Binop(expOp', expr1', expr2')) =
       eqExpOp(expOp,expOp') andalso eqExpr(expr1, expr1') andalso eqExpr(expr2, expr2')
    | eqExpr(QuestionColon(expr1, expr2, expr3),
	     QuestionColon(expr1', expr2', expr3')) =
       eqExpr(expr1, expr1') andalso eqExpr(expr2, expr2')
       andalso eqExpr(expr3, expr3')  
    | eqExpr(Call(expr1, exprl), Call(expr1', exprl')) =
       eqExpr(expr1, expr1') andalso (eqList eqExpr (exprl, exprl'))
    | eqExpr(Cast(ctype, expr), Cast(ctype', expr')) = eqExpr(expr, expr')
    | eqExpr(InitList exprl, InitList exprl') = eqList eqExpr (exprl, exprl')
    | eqExpr(ExprExt _, ExprExt _) = false
    | eqExpr(_, _) = false

  (* dpo: some small changes to get eqType type correct but is the equality correct? *)
  fun eqTy({qualifiers=[], specifiers=[Enum{tagOpt=sOpt, enumerators=sel, ...}]}
	  ,{qualifiers=[], specifiers=[Enum{tagOpt=sOpt',enumerators=sel', ...}]}) =
       sOpt = sOpt' andalso eqList (eqPair (eqString, eqExpr)) (sel, sel')
    | eqTy({qualifiers=[], specifiers=[Struct{isStruct=b, tagOpt=sOpt, members=cdell}]}
          ,{qualifiers=[], specifiers=[Struct{isStruct=b',tagOpt=sOpt',members=cdell'}]}) =
      (b = b') andalso sOpt = sOpt' andalso
      eqList
        (eqPair (eqCtype, eqList(eqPair(eqDeclarator, eqExpr))))
        (cdell, cdell')
    | eqTy(_, _) = false

  end (* local *)
    
end (* structure TyEq *)


structure AnonymousStructs =
struct
  
  (* ------------------------------------------------------------
     Resolving Anonymous Structs (for inter-file analysis)
     The problem: need to resolve structurally equiv anonymous structs in
                  different files to same tid.
     ------------------------------------------------------------
   *)

  val anonymousStructsEnumsList = ref (nil : (ParseTree.ctype * Tid.uid) list)
  fun resetAnonymousStructsEnumsList () = (anonymousStructsEnumsList := nil)

  fun findAnonStructEnum ty =
      let fun finder((ty', tid) :: l) = 
	       if TyEq.eqTy(ty, ty')
	       then (SOME tid)
		       (* debugging code:
		       print ("recovered anon struct with tid " ^ (Tid.toString tid)
			      ^ "\n");
		       (case ty of 
			  ParseTree.Enum _ => print "Enum\n"
			| ParseTree.Struct(_, _, (_, (dec, e) :: _) :: _) => 
			    (case dec of
			       ParseTree.Name name => print("Struct " ^ name ^ ".. \n")
			     | _ => print("Struct ? .. \n"))
			| _ => print "Something else ..\n"); *)
	       else finder l
	    | finder nil = NONE
       in finder (!anonymousStructsEnumsList)
      end

  fun addAnonTid (ty, tid) = 
      let val l = (ty, tid) :: (!anonymousStructsEnumsList)
       in anonymousStructsEnumsList := l
      end

end (* structure AnonymousStructs *)
