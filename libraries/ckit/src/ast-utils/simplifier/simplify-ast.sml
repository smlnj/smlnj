(* Copyright (c) 1998 by Lucent Technologies *)

(* The simplify transformation normalizes the C code by:
 *  o getting rid of pre and post-increments
 *  o getting rid of op='s
 *  o getting rid of nested assignments w/in expressions
 *  o getting rid of comma expressions
 *  o getting rid of questioncolon expressions
 *  o getting rid of arrows
 *  o translating all breaks and continues into jumps
 *  o translating all do, whiles, and fors into conditional jumps
 *  o translating all Label (id,stmt) into sequences Label (id,emptyStmt); stmt
 *    which allows for a more unified expression of control flow.
 *  o translating all global/static variables without (or with only partial) 
 *    initializations into into explicit initializations to 0.
 *  o translating all local variable initializations into explicit assignments.
 * 
 * The transformation introduces new identifiers, expressions, and statements.
 * The pidtab and the aidtab are kept consistent but the opaidtab is not since it
 * shouldn't be needed after this transformation.  The tidtab is not affected by 
 * the transformation.
 *
 * The main transformation in the system is the simplicication of expressions into 
 * non-sideffecting forms.  All side effects (op==,pre/pos-increment,assignment)
 * expressions are lifted to statements.  The grammar of expressions after this
 * transformation is:
 *
 * exp ::= id
 *      |  constant
 *      |  primapp exp*  - where the primapp does not have a side-effect
 *      |  exp.exp
 *      |  exp->exp
 *      |  exp[exp]
 *      |  *exp
 *      |  &exp
 *      |  sizeof exp    - sizeof should have already been eliminated
 *      |  {exp*}
 *
 * within the context of statements, where assignments and function calls must
 * occur, top level expression have the following syntax:
 *
 * topExp ::= exp                     - ie the new, restricted form of expressions
 *         |  exp (exp* )             - function calls
 *         |  exp :=  exp             - simple assignments
 *         |  exp :=  exp (exp* )     - assignments of function call values
 *
 *
 * Issues:
 * Consider the following code:
 *
 *    x->y->m += 12;
 *
 * This gets translated into:
 *
 *    x->y->m = x->y->m + 12;
 *
 * by copying the arbitraily complex expression (x->y->m).
 * The alternative is to introduce a temporary variable:
 *
 *   temp = x->y
 *   temp->m = temp->m + 12;
 *
 * but this transformation is subtle as you can't write:
 *
 *   temp = x->y->m
 *   temp = temp + 12;
 * 
 *)

structure SimplifyAst : SIMPLIFYAST =
struct 

  structure Ast = Ast
  structure Copy = CopyAst
  open Ast 

  exception simplifyExn

  fun warn msg = print msg

  fun fail msg = (print msg; raise simplifyExn)

  val strictlyPrintable = ref true (* try to make it acceptable C code: see handling of casts *)

  val sizeOf = Sizeof.byteSizeOf {warn=warn,err=fail, bug=fail} 

  fun lookup looker id =
      case looker id
	  of NONE => fail "trying to lookup id"
	   | SOME v => v

  fun simplifyAst (edecls,tidtab,aidtab,opaidtab) =
    let 
      val esctab = Pidtab.uidtab () : unit Pidtab.uidtab
      val getTid = lookup (fn tid => Tidtab.find (tidtab,tid))
      val getOpAid = lookup (fn aid => Aidtab.find (opaidtab,aid))

      fun copyExp exp =  Copy.copyExpr aidtab exp

      fun newLabel name = 
	  { name=Symbol.label name
	  , uid = Pid.new ()
	  , location=SourceMap.UNKNOWN
	  }

      fun newId name ctype = 
	  { name=Symbol.object name
          , uid=Pid.new()
	  , location=SourceMap.UNKNOWN
	  , ctype=ctype
	  , stClass=Ast.DEFAULT,
	   global=false,
	   status=DECLARED
	  , kind= if TypeUtil.isFunction tidtab ctype then Ast.FUNCTION{hasFunctionDef=false}
	          else Ast.NONFUN
	  }

      fun addEscape pid = 
	  Pidtab.insert(esctab,pid,())

      (* fix: this code is incomplete ... *)
      (* lval ::= id | id.field | expr[expr'] | expr->field | *expr *)
      fun escapes (EXPR (Id {uid=pid,...},_,_)) = addEscape pid
	| escapes (EXPR (Member (expr, _))) = escapes expr
        | escapes _ = ()

      (* Generate a new aid, bind it to ty in aidtab, and return it *)
      fun bindAid ctype = 
	  let val aid = Aid.new ()
	  in Aidtab.insert(aidtab,aid,ctype);
	     aid
	  end

      fun id2ctype (id: Ast.id) = #ctype id

      fun isStaticOrGlobal ({stClass=Ast.STATIC,...}: Ast.id) = true
	| isStaticOrGlobal {global=true,...} = true
        | isStaticOrGlobal _ = false

      fun aid2ctype aid = 
	  case Aidtab.find (aidtab,aid)
	    of NONE => ( print "unknown type for aid "
		       ; print (Aid.toString aid)
		       ; print ",assuming its void\n"
		       ;  Ast.Void
		       )
	     | SOME ctype => ctype

      fun exp2ctype (EXPR (_,aid,_)) = aid2ctype aid

      fun coreExp2exp ctype coreExp = EXPR (coreExp,bindAid ctype,SourceMap.UNKNOWN)

      fun coreStmt2stmt coreStmt = STMT (coreStmt,Aid.new (),SourceMap.UNKNOWN)

      fun exp2stmt exp = coreStmt2stmt (Expr (SOME exp))
      
      fun coreExp2stmt ctype coreExp = exp2stmt (coreExp2exp ctype coreExp)

      fun mkId id = 
	  coreExp2exp (id2ctype id) (Id id)

      fun label id =
	  coreStmt2stmt (Labeled (id,coreStmt2stmt (Expr NONE)))

      fun assign id exp = 
	  let val ctype = id2ctype id
	  in coreExp2stmt ctype (Assign (mkId id,exp)) end

      (* dpo: eqCtype this is potentially expensive, should we do this? *)
      fun cast ctype exp =  
	  if CTypeEq.eqCType(ctype, (exp2ctype exp)) then exp
	  else coreExp2exp ctype (Cast (ctype,exp))

      fun decl id = 
	  let val ctype = id2ctype id
	  in VarDecl (id,NONE) end

      fun compound decls stmts = 
	  let fun filter [] = ([],[])
		| filter (stmt::stmts) =
		  let val (decls,stmts) = filter stmts
		  in case stmt
		       of STMT (Compound (cmpDecls,cmpStmts),_,_) => (cmpDecls@decls,cmpStmts@stmts)
		        | STMT (Expr NONE,_,_) => (decls,stmts)
		        | _ => (decls,stmt::stmts)
		  end
	      val (decls',stmts') = filter stmts
	  in Compound (decls@decls',stmts') end

      fun noEffect (EXPR (coreExpr,_,_)) = 
	  case coreExpr
	    of IntConst _ => true
	     | RealConst _ => true
	     | StringConst _ => true
	     | Id _ => true
	     | _ => false

      fun stmts2stmt [] [stmt] = stmt
	| stmts2stmt decls stmts = STMT (compound decls stmts,Aid.new (),
					 SourceMap.UNKNOWN)

      val intCt = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.SIGNED,Ast.INT, Ast.SIGNASSUMED)

      val charCt = Ast.Numeric (Ast.NONSATURATE,Ast.WHOLENUM,Ast.UNSIGNED,Ast.CHAR, Ast.SIGNASSUMED)

      fun mkInt i = coreExp2exp intCt (IntConst (i:LargeInt.int))

      fun mkChr c = coreExp2exp charCt (IntConst (Int32.fromInt (ord c)))

      fun simplifyExtDecls edecls =
	      map simplifyExtDecl edecls

      and simplifyExtDecl (DECL (coreExtDecl,aid,loc)) =
	  DECL (simplifyCoreExtDecl coreExtDecl,aid,loc)

      and simplifyCoreExtDecl coreExtDecl =
	  case coreExtDecl
	    of ExternalDecl decl => ExternalDecl decl
	     | FunctionDef (id,ids,stmt) =>
		 (case simplifyStmt (NONE,NONE) stmt
	            of {decs=[],stmts=[stmt]} => FunctionDef (id,ids,stmt)
	             | {decs,stmts} =>
			  FunctionDef (id,ids,coreStmt2stmt (compound decs stmts)))
	     | ExternalDeclExt ext => 
		 SimplifyAstExt.simplifyExtDeclExt
		   (tidtab,aidtab,opaidtab)
		   (simplifyNestedExp, simplifyStmt (NONE,NONE))
		   ext

      and simplifyDecls [] = {decs=[],stmts=[]}
	| simplifyDecls (decl::decls) =
	    let val {decs=decs0,stmts=stmts0} = simplifyDecl decl
		val {decs=decs1,stmts=stmts1} = simplifyDecls decls
	    in {decs=decs0@decs1,stmts=stmts0@stmts1} end

      and simplifyDecl decl =
	  case decl
            of TypeDecl tid => {decs=[decl],stmts=[]}
	     | VarDecl (id,NONE) => {decs=[decl],stmts=[]}
	     | VarDecl (id,SOME initExpr) =>
	       if isStaticOrGlobal id
		   then {decs=[VarDecl (id,SOME initExpr)],stmts=[]}
	       else let val ctype = id2ctype id
			val dec = VarDecl (id, NONE)
			val {decs,stmts} = simplifyAutoInit (mkId id) ctype initExpr
		    in {decs=decs@[dec],stmts=stmts} end

    and simplifyStmts pair [] = {decs=[],stmts=[]}
      | simplifyStmts pair (stmt::stmts) =
 	  let val {decs=decs0,stmts=stmts0} = simplifyStmt pair stmt
 	      val {decs=decs1,stmts=stmts1} = simplifyStmts pair stmts
 	  in {decs=decs0@decs1,stmts=stmts0@stmts1} end


      and simplifyStmt (pair as (contOpt,brkOpt))
	               (stmt as STMT (coreStmt,aid,loc)) =
	let fun mkStmt coreStmt = STMT (coreStmt,aid,loc)
	    fun cs2stmt coreStmt = STMT (coreStmt,Aid.new (),loc)
	in case coreStmt
	    of Expr expOpt => 
		 let val {decs,pre,expOpt} = simplifyTopExpOpt expOpt
		     val stmt = mkStmt (Expr expOpt)
		 in {decs=decs,stmts=pre@[stmt]} end
 	     | Compound (decls,stmts) => 
		 let val {decs=decs0,stmts=stmts0}= simplifyDecls decls
		     val {decs=decs1,stmts=stmts1} = simplifyStmts pair stmts
		 in {decs=decs0@decs1,stmts=[mkStmt (compound [] (stmts0@stmts1))]} end
	     (* The translation of while minimizes the number of jumps
              * in the body of the loop.
	      *
	      *  while (exp,stmt) => 
              *                    goto startLabel
	      *          topLabel: stmt
	      *   start&contLabel: preExp
	      *                    if exp then goto topLabel
	      *          brkLabel:
	      *
              * NOTE: the brk label is added only if is used.
	      *)
	     | While (exp,stmt) =>
		 let val topLab = newLabel "whileTop"
		     val contLab = newLabel "whileCont"
		     val brkLab = newLabel "whileBrk"
		     val contUsed = ref true
		     val brkUsed = ref false
		     val pair = (SOME (contUsed,contLab),SOME (brkUsed,brkLab))
		     val {decs=expDecs,pre=preExp,exp=exp} = simplifyTopExp exp
		     val {decs=bodyDecs,stmts} = simplifyStmt pair stmt
		     val stmts =   [cs2stmt (Goto contLab),
				    label topLab
				   ]
				 @ stmts
				 @ [label contLab]
				 @ preExp
				 @ [mkStmt (IfThen (exp,cs2stmt (Goto topLab)))]
				 @ (if !brkUsed then [label brkLab] else [])
		 in {decs=expDecs@bodyDecs,stmts=stmts} end
	     (* The translation of do minimizes the number of jumps
              * in the body of the loop.
	      *
	      *  do (exp,stmt) => 
	      *    topLabel:  stmt
	      *   contLabel:  preExp
	      *               if exp then goto topLabel
	      *    brkLabel:
	      *
              * NOTE: cont and brk labels are added only if they are used.
	      *)
	     | Do (exp,stmt) => 
		 let val topLab = newLabel "doTop"
		     val contLab = newLabel "doCont"
		     val brkLab = newLabel "doBrk"
		     val contUsed = ref false
		     val brkUsed = ref false
		     val pair = (SOME (contUsed,contLab),SOME (brkUsed,brkLab))
		     val {decs=expDecs,pre=preExp,exp} = simplifyTopExp exp
		     val {decs=bodyDecs,stmts} = simplifyStmt pair stmt
		     val stmts =   [label topLab]
				 @ stmts
				 @ (if !contUsed then [label contLab] else [])
				 @ preExp
				 @ [mkStmt (IfThen (exp,mkStmt (Goto topLab)))]
				 @ (if !brkUsed then [label brkLab] else [])
		 in {decs=expDecs@bodyDecs,stmts=stmts} end
	     (* The translation of for minimizes the number of jumps
              * in the body of the loop.
	      *
	      *  for (e0,e1,e2,stmt) => 
	      *                    preE0
              *                    e0
              *                    goto startLabel
	      *          topLabel: stmt
              *         contLabel: preE2
	      *                    e2
	      *        startLabel: preE1
	      *                    if e1 then goto topLabel
	      *          brkLabel:
	      *
              * NOTE: cont and brk labels are added only if they are used.
	      *)
	     | For (eOpt0,eOpt1,eOpt2,stmt) => 
		 let val topLab = newLabel "forTop"
		     val startLab = newLabel "forStart"
		     val contLab = newLabel "forCont"
		     val brkLab = newLabel "forBrk"
		     val contUsed = ref false
		     val brkUsed = ref false
		     val pair = (SOME (contUsed,contLab),SOME (brkUsed,brkLab))
		     val {decs=e0Decs,pre=preE0,expOpt=eOpt0} = simplifyTopExpOpt eOpt0
		     val {decs=e1Decs,pre=preE1,expOpt=eOpt1} = simplifyTopExpOpt eOpt1
		     val {decs=e2Decs,pre=preE2,expOpt=eOpt2} = simplifyTopExpOpt eOpt2
		     val {decs=bodyDecs,stmts} = simplifyStmt pair stmt
		     fun expOpt2stmt NONE = []
		       | expOpt2stmt (SOME exp) = 
			 if noEffect exp then [] else [exp2stmt exp]

		     val stmts =   preE0
				 @ expOpt2stmt eOpt0
				 @ [cs2stmt (Goto startLab)
				   ,label topLab
				   ]
				 @ stmts
				 @ (if !contUsed then [label contLab] else [])
				 @ preE2
				 @ expOpt2stmt eOpt2
				 @ [label startLab]
				 @ preE1
				 @ (case eOpt1 
				      of SOME e1 => [mkStmt (IfThen (e1,cs2stmt (Goto topLab)))]
				       | NONE => [])
				 @ (if !brkUsed then [label brkLab] else [])
		 in {decs=e0Decs@e1Decs@e2Decs@bodyDecs,stmts=stmts} end
	     | Labeled (label,stmt) =>
		 let val {decs,stmts} = simplifyStmt pair stmt
		     val stmt = mkStmt (Labeled (label,cs2stmt (Expr NONE)))
		 in {decs=decs,stmts=stmt::stmts} end
	     | CaseLabel (li,stmt) =>
		 let val {decs,stmts} = simplifyStmt pair stmt
		 in {decs=decs,stmts=[mkStmt (CaseLabel (li,stmts2stmt [] stmts))]}
		 end
	     | DefaultLabel stmt => 
		 let val {decs,stmts} = simplifyStmt pair stmt
		 in {decs=decs,stmts=[mkStmt (DefaultLabel (stmts2stmt [] stmts))]}
		 end
	     | Goto label => {decs=[],stmts=[mkStmt (Goto label)]}
	     | Break =>
		 (case brkOpt
		    of NONE => fail "invalid context for break"
		     | SOME (brkUsed,label) =>
			 ( brkUsed := true
			 ; {decs=[],stmts=[mkStmt (Goto label)]}
			 ))
	     | Continue =>
		 (case contOpt
		    of NONE => fail "invalid context for continue"
		     | SOME (contUsed,label) =>
			(contUsed := true;
			 {decs=[],stmts=[mkStmt (Goto label)]}))
	     | Return expOpt =>
		 let val {decs,pre,expOpt} = simplifyTopExpOpt expOpt
		 in {decs=decs,stmts=pre@[mkStmt (Return expOpt)]}
		 end
	     | IfThen (exp,stmt) => 
		 let val {decs=decs0,pre,exp} = simplifyTopExp exp
		     val {decs=decs1,stmts} = simplifyStmt pair stmt
		     val stmts = pre@[mkStmt (IfThen (exp,stmts2stmt [] stmts))]
		  in {decs=decs0@decs1,stmts=stmts}
		 end
	     | IfThenElse (exp,stmt0,stmt1) =>
		 let val {decs,pre,exp} = simplifyTopExp exp
		     val {decs=decs0,stmts=stmts0} = simplifyStmt pair stmt0
		     val {decs=decs1,stmts=stmts1} = simplifyStmt pair stmt1
		     val stmts =
			 pre@[mkStmt(IfThenElse(exp,stmts2stmt [] stmts0,
						stmts2stmt [] stmts1))]
		 in {decs=decs@decs0@decs1,stmts=stmts} end
	     | Switch (exp,stmt) =>
		 let val {decs=decs0,pre,exp} = simplifyTopExp exp
		     val brkLab = newLabel "switchBrk"
		     val brkUsed = ref false
		     val {decs=decs1,stmts} =
			 simplifyStmt (contOpt,SOME (brkUsed,brkLab)) stmt
		     val stmts =  pre
				@ [mkStmt (Switch (exp,stmts2stmt [] stmts))]
				@ (if !brkUsed then [label brkLab] else [])
		  in {decs=decs0@decs1,stmts=stmts}
		 end
	     | ErrorStmt => {decs=nil, stmts=[mkStmt ErrorStmt]}
	     | StatExt ext => 
		 let val {decs,coreStmt} =
		         SimplifyAstExt.simplifyStmtExt 
			   (tidtab,aidtab,opaidtab) 
			   (simplifyNestedExp,simplifyStmt (NONE,NONE))
			   ext
		  in {decs=decs,stmts=[mkStmt coreStmt]}
		 end
	end 


      and simplifyAutoInit lhs ctype initExp =
	  case initExp
	    of (Aggregate initExps) => 
		let val {stmts} = autoInit lhs ctype initExp
		in {decs=[],stmts=stmts} end
	     | (Simple exp) => 
		  let val {decs,pre,exp} = simplifyTopExp exp
		      val stmt = coreExp2stmt ctype (Assign (lhs,exp))
		  in {decs=decs,stmts=pre@[stmt]} end

      and autoInit lhs ctype initExp =
	  let fun feed initer (Aggregate initExps) = initer initExps
		| feed initer _ = fail "bad form for initializer"

	      fun arrInit lhs ctype i [] = {stmts=[]}
		| arrInit lhs ctype i (initExp::initExps) =
		  let val intConst = mkInt i
		      val arrLhs = coreExp2exp ctype (Sub (lhs,intConst))
		      val {stmts} = autoInit arrLhs ctype initExp
		      val {stmts=stmts'} = arrInit lhs ctype (i+1) initExps
		  in {stmts=stmts@stmts'} end

	      fun structInit lhs [] [] = {stmts=[]}
		| structInit lhs [] initExps = fail "initializer too big"
		| structInit lhs fields [] = fail "initializer too small"
		| structInit lhs ((ctype,NONE,liOpt)::fields) initExps = 
		  (* according to the standard, unnamed fields don't
		   * get initialized.
		   *)
		  structInit lhs fields initExps
		| structInit lhs ((ctype,SOME mem,liOpt)::fields) (initExp::initExps) = 
		  let val memLhs = coreExp2exp ctype (Member (lhs,mem))
		      val {stmts} = autoInit memLhs ctype initExp
		      val {stmts=stmts'} = structInit lhs fields initExps
		  in {stmts=stmts@stmts'} end

	      fun unionInit lhs [] initExps = {stmts=[]}
		| unionInit lhs ((ctype,mem)::_) ([initExp]) =
		  let val lhs = coreExp2exp ctype (Member (lhs,mem))
		  in autoInit lhs ctype initExp end
		| unionInit lhs fields exp = fail "bad form for union"

	      fun scalarInit lhs ctype (Simple exp) = 
		  {stmts=[coreExp2stmt ctype (Assign (lhs,exp))]}
		| scalarInit lhs ctype _ = 
		  fail "bad form for initializer"
		  
	  in case ctype 
	       of Ast.Qual (_,ctype) => autoInit lhs ctype initExp
	        | Ast.TypeRef tid => 
		    (case getTid tid 
		       of {ntype=SOME (Bindings.Typedef (tid,ctype)),...} => 
			   autoInit lhs ctype initExp
		        | _ => fail "bad type for initializer")
		| Ast.Array (_,ctype) => feed (arrInit lhs ctype 0) initExp
		| Ast.StructRef tid => 
		    (case getTid tid 
		       of {ntype=SOME (Bindings.Struct (tid,fields)),...} =>
			   feed (structInit lhs fields) initExp
			| _ => fail "bad type for initializer")
		| Ast.UnionRef tid => 
		    (case getTid tid 
		       of {ntype=SOME (Bindings.Union (tid,fields)),...} =>
			   feed (unionInit lhs fields) initExp
			| _ => fail "bad type for initializer")
		| Ast.Numeric _ => scalarInit lhs ctype initExp
		| Ast.Pointer _ => scalarInit lhs ctype initExp
		| Ast.Function _ => scalarInit lhs ctype initExp
		| Ast.EnumRef _ => scalarInit lhs ctype initExp
		| _ => fail "bad type for initializer"
	  end

      and simplifyExps [] = {decs=[],pre=[],exps=[]}
	| simplifyExps (exp::exps) =
	  let val {decs,pre,exp} = simplifyExp {nested=true} exp
	      val {decs=decs',pre=pre',exps} = simplifyExps exps
	  in {decs=decs@decs',pre=pre@pre',exps=exp::exps} end

      and simplifyNestedExp exp = simplifyExp {nested=true} exp

      and simplifyTopExp exp = simplifyExp {nested=false} exp

      and simplifyTopExpOpt NONE = {decs=[],pre=[],expOpt=NONE}
	| simplifyTopExpOpt (SOME exp) =
	  let val {decs,pre,exp} = simplifyTopExp exp
	  in {decs=decs,pre=pre,expOpt=SOME exp} end


      and simplifyExp {nested} (exp as EXPR (coreExp,aid,loc)) =
	let fun mkExp coreExp = EXPR (coreExp,aid,loc)
	    val ctype = aid2ctype aid
	in case coreExp
	     of IntConst _ => {decs=[],pre=[],exp=exp}
	      | RealConst _ => {decs=[],pre=[],exp=exp}
	      | StringConst _ => {decs=[],pre=[],exp=exp}
	      | Call (exp,exps) =>
		  let val {decs=decs0,pre=pre0,exp} = simplifyNestedExp exp
		      val {decs=decs1,pre=pre1,exps} = simplifyExps exps
		      val callExp = mkExp (Call (exp,exps))
		  in if nested
			 then let val id = newId "call" ctype
				  val dec = decl id
				  val stmt = assign id callExp
				  val exp = mkId id
			      in {decs=dec::decs0@decs1,pre=pre0@pre1@[stmt],exp=exp} end
		     else {decs=decs0@decs1,pre=pre0@pre1,exp=callExp}
		  end 
	      | QuestionColon (exp0,exp1,exp2) =>
		  let val id = newId "quesCol" ctype
		      val dec = decl id
		      val {decs=decs0,pre=pre0,exp=exp0} = simplifyNestedExp exp0
		      val {decs=decs1,pre=pre1,exp=exp1} = simplifyTopExp exp1
		      val {decs=decs2,pre=pre2,exp=exp2} = simplifyTopExp exp2
		      val stmt = coreStmt2stmt
			             (IfThenElse
			               ( exp0
				       , stmts2stmt decs1 (pre1@[assign id exp1])
				       , stmts2stmt decs2 (pre2@[assign id exp2])
				       )
				     )
		      val exp = mkId id
		  in {decs=dec::decs0,pre=pre0@[stmt],exp=exp} end
	      | Assign (exp0,exp1) =>
		  let val {decs=decs0,pre=pre0,exp=exp0} = simplifyNestedExp exp0
		      val {decs=decs1,pre=pre1,exp=exp1} = simplifyNestedExp exp1
		      val exp = mkExp (Assign (exp0,exp1))
		  in if nested
			 then {decs=decs0@decs1,pre=pre0@pre1@[exp2stmt exp],exp=exp0}
		     else{decs=decs0@decs1,pre=pre0@pre1,exp=exp}
		  end
	      | Comma (exp0,exp1) =>
		  let val {decs=decs0,pre=pre0,exp=exp0} = simplifyNestedExp exp0
		      val {decs=decs1,pre=pre1,exp=exp1} = simplifyNestedExp exp1
		      val pre = if noEffect exp0 then pre0@pre1 else pre0@[exp2stmt exp0]@pre1
		  in {decs=decs0@decs1,pre=pre,exp=exp1} end
	      | Sub (exp0,exp1) =>
		  let val {decs=decs0,pre=pre0,exp=exp0} = simplifyNestedExp exp0
		      val {decs=decs1,pre=pre1,exp=exp1} = simplifyNestedExp exp1
		      val exp = mkExp (Sub (exp0,exp1))
		  in {decs=decs0@decs1,pre=pre0@pre1,exp=exp} end
	      | Member (exp,mem) =>
		  let val {decs,pre,exp} = simplifyNestedExp exp
		      val exp = mkExp (Member (exp,mem))
		  in {decs=decs,pre=pre,exp=exp} end
	      | Arrow (exp,mem) =>
		  (case exp2ctype exp
		     of Ast.Pointer ctype =>
			  let val {decs,pre,exp} = simplifyNestedExp exp
			      val exp = coreExp2exp ctype (Deref exp)
			      val exp = mkExp (Member (exp,mem))
			  in {decs=decs,pre=pre,exp=exp} end
		      | _ => fail "Arrow: type error")
	      | Deref exp =>
		  let val {decs,pre,exp} = simplifyNestedExp exp
		      val exp = mkExp (Deref exp)
		  in {decs=decs,pre=pre,exp=exp} end
(***
	      | AddrOf exp => 
		    let val {decs,pre,exp} = simplifyNestedExp exp
			val exp = mkExp (AddrOf exp)
		    in (escapes exp; {decs=decs,pre=pre,exp=exp}) end
***)

              (* notes on addrOf:
	            s: effect of simplifyNestedExp
                    a: effect of adrf
                 1. x =s=> x =a=> &x
                 2. x->field =s=> *(x+k) =a=> x+k  where k is offset of field
                 3. x.field =s=> *((&x)+k) =a=> &x+k where k is offset of field
                 4. e[i] =s=> *(e+i*k) =a=> e+i*k  where k is scaling for ( *e)
                 5. x[i] =s=> *(&x+i*k)
               **)
	      | AddrOf exp => 
		    let
		      val {decs, pre, exp} = simplifyNestedExp exp
		      fun adrf(expr as EXPR(coreExpr, aid, loc)) =
			(case coreExpr of
			   Id{uid=pid, ...} =>
			     (addEscape pid;
			      exp=EXPR(AddrOf expr, aid, loc))
			 | Member(memExpr, field) =>
			     let val expr = adrf(memExpr)
			       val ctype = exp2ctype(memExpr)
			       fun nullErr _ = ()
			       val errs = {err=nullErr, warn=nullErr, bug=nullErr} 
			       val fieldOffs = Sizeof.fieldOffsets errs tidtab ctype
			       val byteOffset = #bitOffset(Sizeof.getField errs (field, fieldOffs))
			     in
				EXPR(Binop(Plus(exp, byteOffset)), aid, loc)
			     end
			 | Deref expr => expr
			 | (Sub _ | Arrow _) => fail "simplifyNestedExp returned Sub or Arrow")
(* x[4] -> *(x+16) -> x+16 *)
		    in
		      {decs=decs, pre=pre, exp=adrf exp}
		    end

		    let val {decs,pre,exp} = simplifyNestedExp exp
			val exp = mkExp (AddrOf exp)
		    in (escapes exp; {decs=decs,pre=pre,exp=exp}) end

	      | Binop trip =>
		  simplifyBinop mkExp {nested=nested} ctype trip
	      | Unop pair => 
		  let val {decs,pre,coreExp} = simplifyUnop {nested=nested} ctype pair
		  in {decs=decs,pre=pre,exp=mkExp coreExp} end
	      | Cast (ctype,exp) =>
		    let val {decs,pre,exp} = simplifyNestedExp exp
		    in
		      {decs=decs, pre=pre, exp= mkExp(Cast(ctype, exp))}
		    end
	      | Id _ => {decs=[],pre=[],exp=exp}
	      | EnumId _ => {decs=[],pre=[],exp=exp}
              | SizeOf _ => {decs=[],pre=[],exp=exp}  (* should not appear in compiler mode *)
	      | ExprExt ext => 
		  let val {decs,pre,coreExp} =
		             SimplifyAstExt.simplifyExpExt 
			         (tidtab,aidtab,opaidtab) 
				 (simplifyNestedExp, simplifyStmt(NONE,NONE))
				 ext
		  in {decs=decs,pre=pre,exp=mkExp coreExp}
		  end
	      | ErrorExpr => {decs=[],pre=[],exp=exp}
	end

      and scale ctype i = 
	  case ctype
	    of Ast.Qual (_,ctype) => scale ctype i
	     | Ast.Pointer ctype => let val {bytes,...} = sizeOf tidtab ctype
			        in LargeInt.fromInt (bytes * i) end 
	     | _ => LargeInt.fromInt i

      and simplifyUnop {nested} ctype (unop,exp as EXPR (_,aid,_)) = 
	  let val {decs,pre,exp} = simplifyNestedExp exp
	      fun mkUnop unop = {decs=decs,pre=pre,coreExp=Unop (unop,exp)}
	      fun mkAssign {prefixOp} binop =
		  (* opArgTy is type to which arg is converted
		     e.g. e++ where e has type ctype and opArgTy newTy
		     becomes e = (ctype)( (newTy)e + (newTy)1 )
                     and if ctype is a pointer, then 1 gets scaled by sizeof( *ctype )
		   *)
		  let val id = newId (if prefixOp then "pref" else "post") ctype
		      val dec = decl id
		      val newTy = getOpAid aid
		      val argExp = cast newTy exp
		      val one = cast newTy
			          (coreExp2exp intCt (IntConst (scale ctype 1)))
		      val binExp = coreExp2exp newTy (Binop (binop,argExp,one))
		      val incrStmt = coreExp2stmt ctype
			               (Assign(copyExp exp, cast ctype binExp))
		      val assignStmt = 
			  coreExp2stmt ctype (Assign (mkId id,copyExp exp))
		      val pre = if prefixOp then pre@[incrStmt,assignStmt] 
				else pre@[assignStmt,incrStmt]
		  in {decs=dec::decs, pre=pre, coreExp=Id id}
		  end
	  in case unop
	    (* the ++, --, cases are no longer dealt with in here;
               there is now code in build-ast (which is
               enabled when insert_explicit_coersions is set)
               that simplifies ++ and -- *)
	       of PreInc  => mkAssign {prefixOp=true} Plus
	        | PreDec  => mkAssign {prefixOp=true} Minus
		| PostInc => mkAssign {prefixOp=false} Plus
		| PostDec => mkAssign {prefixOp=false} Minus
		| _ => mkUnop unop
	  end
	  
      and simplifyBinop mkExp {nested} ctype (binop,exp0 as EXPR (_,aid,_),exp1) = 
	  let val {decs=decs0,pre=pre0,exp=exp0} = simplifyNestedExp exp0
	      val {decs=decs1,pre=pre1,exp=exp1} = simplifyNestedExp exp1
	      val decs = decs0@decs1
	      val pre = pre0@pre1
	      fun mkBinop binop =
		  {decs=decs,pre=pre,exp=mkExp (Binop (binop,exp0,exp1))}
	      fun mkAssign binop =
		  (* for e0 += e1, e0 -= e1 and their friends, opArgTy specifies
		     the type that e0 must be converted to
		     e.g. e0 += e1 becomes e0 = (ctype(e0)) ( (opArgTy e0) + e1 )
		   *)
		  let val opArgTy = getOpAid aid
		      val binExp = coreExp2exp opArgTy
			             (Binop (binop,cast opArgTy exp0,exp1))
		      val assign = mkExp (Assign (copyExp exp0,cast ctype binExp))
		  in if nested
		     then {decs=decs,pre=pre@[exp2stmt assign],exp=copyExp exp0}
		     else {decs=decs,pre=pre,exp=assign}
		  end 
	  in case binop
	    (* the +=, -=, etc., cases are no longer dealt with in here;
               there is now code in build-ast (which is
               enabled when insert_explicit_coersions is set)
               that simplifies +=, -=, ... *)
	       of PlusAssign   => mkAssign Plus
	        | MinusAssign  => mkAssign Minus
		| TimesAssign  => mkAssign Times
		| DivAssign    => mkAssign Divide
		| ModAssign    => mkAssign Mod
		| XorAssign    => mkAssign BitXor
		| OrAssign     => mkAssign Or
		| AndAssign    => mkAssign And
		| LshiftAssign => mkAssign Lshift
		| RshiftAssign => mkAssign Rshift
		| _ => mkBinop binop
	  end
    in {ast=simplifyExtDecls edecls, escapetab = esctab}
    end
end
