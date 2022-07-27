(* cxx.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A tree representation of C++ programs.
 *)

structure Cxx =
  struct

    type var = string
    type attr = string          (* e.g., "static", "kernel", etc ... *)

  (* fixed-precision numeric types from <stdint> *)
    datatype num_ty
      = NT_Int8 | NT_UInt8
      | NT_Int16 | NT_UInt16
      | NT_Int32 | NT_UInt32
      | NT_Int64 | NT_UInt64
      | NT_Float | NT_Double

    datatype ty
      = T_Num of num_ty
      | T_Const of ty           (* const annotation; note that T_Const(T_Ptr ty) means
                                 * that the pointer is constant, whereas T_Ptr(T_Const ty)
                                 * means that what is pointed to is constant.
                                 *)
      | T_Ptr of ty
      | T_Ref of ty		(* reference type *)
      | T_RestrictPtr of ty     (* pointer type with "restrict" annotation *)
      | T_Array of ty * int option
      | T_Named of string
      | T_Template of string * ty list
      | T_Qual of attr * ty     (* qualified type *)
      | T_Member of ty * string (* select type member of named/template type *)

    datatype typed_var = V of ty * var

    val voidTy = T_Named "void"
    val voidPtr = T_Ptr voidTy
    val constVoidPtr = T_Ptr(T_Const voidTy)
    val charTy = T_Named "char"
    val boolTy = T_Named "bool"
    val charPtr = T_Ptr charTy
    val charArrayPtr = T_Ptr charPtr
    val intTy = T_Named "int"
    val uintTy = T_Named "unsigned int"
    val int8 = T_Num(NT_Int8)
    val uint8 = T_Num(NT_UInt8)
    val int32 = T_Num(NT_Int32)
    val uint32 = T_Num(NT_UInt32)
    val int64 = T_Num(NT_Int64)
    val float = T_Num(NT_Float)
    val double = T_Num(NT_Double)
    val autoTy = T_Named "auto"

  (* make a "const ty *" type for some ty *)
    fun constPtrTy ty = T_Ptr(T_Const ty)

  (* make a "ty const &" type for some ty *)
    fun constRefTy ty = T_Ref(T_Const ty)

    datatype decl
      = D_Pragma of string list
      | D_Comment of string list
    (* verbatim text (e.g., preprocessor directives) *)
      | D_Verbatim of string list
    (* global variable declaration *)
      | D_Var of attr list * ty * scope list * var * initializer option
    (* function prototype/definition *)
      | D_Func of attr list * ty * scope list * string * param list * stm option
(* TODO: add support for "const" methods *)
    (* method prototype/definition *)
      | D_Meth of attr list * ty * scope list * string * param list * bool * meth_body option
    (* class constructor definition or prototype:
     *     D_Constr(attrs, namespace, class, params, initsAndBody)
     * the inits should be of the form "id(exp)"
     *)
      | D_Constr of attr list * scope list * string * param list * (exp list * meth_body) option
    (* class destructor definition or prototype *)
      | D_Destr of attr list * scope list * string * stm option
    (* struct type declaration; if the third argument is SOME name, then a
     * typedef is generated.
     *)
      | D_StructDef of string option * (ty * string) list * string option
    (* C++ struct/class definition *)
      | D_ClassDef of {
            name : string,              (* class/struct name *)
            args : ty list option,      (* optional type arguments to class name for specialization *)
            from : string option,       (* optional base class *)
            public : decl list,
            protected : decl list,
            private : decl list
          }
    (* enum / enum-class definition *)
      | D_EnumDef of {
	    isClass : bool,		(* true for "enum class" definition *)
	    name : string,		(* name of enumeration type *)
	    repTy : ty option,		(* optional underlying representation type *)
	    cons : (string * exp option) list	(* enumeration constants *)
	  }
    (* typedef/type alias *)
      | D_Typedef of string * ty
    (* template declaration *)
      | D_Template of template_param list * decl
    (* C++ namespace *)
      | D_Namespace of string * decl list

    and scope = SC_Namespace of string | SC_Type of ty

  (* C++ function bodies *)
    and meth_body
      = MP_Delete		(* '=' 'delete' ';' for constructors *)
      | MP_0			(* '=' '0' ';' for abstract virtual methods *)
      | MP_Body of stm		(* '{' stms '}' *)

    and initializer
      = I_Exp of exp
      | I_Exps of initializer list
      | I_Struct of (string * initializer) list (* C99 labeled struct initializer *)
      | I_Array of (int * initializer) list     (* C99 labeled array initializer *)
      | I_Cons of ty * exp list

    and param = PARAM of attr list * ty * var

    and template_param
      = TypeParam of string
      | ConstParam of ty * string

    and stm
      = S_Block of stm list             (* "{" stms "}" *)
      | S_Comment of string list
      | S_Verbatim of string list
      | S_Decl of attr list * ty * var * initializer option
                                        (* ty var [ '=' exp ]';' *)
      | S_Exp of exp                    (* exp ';' *)
      | S_If of exp * stm * stm         (* 'if' exp stm 'else' stm *)
      | S_Switch of exp * (string list * stm list) list
					(* 'switch' exp '{' ... '}' *)
      | S_While of exp * stm            (* 'while' exp stm *)
      | S_DoWhile of stm * exp          (* 'do' stm 'while' exp *)
      | S_For of ty * (var * exp) list * exp * exp list * stm
                                        (* 'for' '(' decl ';' exp ';' incrs ')' stm *)
      | S_Return of exp option          (* 'return' [ exp ] ';' *)
      | S_Break                         (* 'break' ';' *)
      | S_Continue                      (* 'continue' ';' *)
      | S_Delete of bool * exp          (* 'delete' exp ';' or 'delete[]' exp ';' *)

    and exp
      = E_Grp of exp                    (* "(" e ")" *)
      | E_AssignOp of exp * assignop * exp (* lvalue op= e *)
      | E_Cond of exp * exp * exp       (* e "?" e ":" e *)
      | E_BinOp of exp * binop * exp    (* e op e *)
      | E_UnOp of unop * exp            (* op e *)
      | E_PostOp of exp * postfix       (* e op *)
      | E_Apply of exp * exp list       (* e "(" ... ")" *)
      | E_TApply of scope list * string * ty list * exp list
                                        (* f "<" ... ">" "(" ... ")" *)
      | E_QId of scope list * string    (* qualified ID *)
      | E_Cons of ty * exp list         (* ty "(" ... ")" [C++,CUDA]*)
      | E_New of ty * exp list          (* "new" ty "(" ... ")" [C++,CUDA]*)
      | E_NewAt of exp * ty * exp list	(* "new" "(" exp ")" ty "(" ... ")" [C++]*)
      | E_Subscript of exp * exp        (* e "[" e "]" *)
      | E_Select of exp * string        (* e "." f *)
      | E_Indirect of exp * string      (* e "->" f *)
      | E_Cast of ty * exp              (* "(" ty ")" e *)
      | E_XCast of string * ty * exp    (* "xxx_cast<" ty ">(" e ")"  [C++] *)
      | E_Vec of ty * exp list          (* vector-expression; syntax depends on target [C,OpenCL] *)
      | E_Array of exp list             (* "{" e "," ... "}" [C++] *)
      | E_Var of var
      | E_Int of IntInf.int * ty
      | E_Flt of string
      | E_Bool of bool
      | E_Str of string
      | E_Char of char
      | E_Sizeof of ty                  (* "sizeof(" ty ")" *)

  (* assignment operators *)
    and assignop
      = $= | += | *= | /= | %= | <<= | >>= | &= | ^= | |=

  (* binary operators in increasing order of precedence *)
    and binop
      = #||
      | #&&
      | #|
      | #^
      | #&
      | #== | #!=
      | #< | #<= | #>= | #>
      | #<< | #>>
      | #+ | #-
      | #* | #/ | #%

    and unop = %- | %! | %& | %* | %~ | %++ | %--

    and postfix = ^++ | ^--

  (* smart constructors that add E_Grp wrappers based on operator precedence *)
    local
      val commaP        = 0
      val assignP       = 1
      val condP         = 2
      val lorP          = 3
      val landP         = 4
      val borP          = 5
      val bxorP         = 6
      val bandP         = 7
      val eqP           = 8
      val relP          = 9
      val shiftP        = 10
      val addP          = 11
      val mulP          = 12
      val castP         = 13
      val unaryP        = 14
      val preP          = 15
      val compundP      = 16    (* compound literal *)
      val postP         = 17
      val callP         = 18
      val subP          = 19
      val atomP         = 20
      fun precOfBinop rator = (case rator
             of #|| => lorP
              | #&& => landP
              | #| => borP
              | #^ => bxorP
              | #& => bandP
              | #== => eqP | #!= => eqP
              | #< => relP | #<= => relP | #>= => relP | #> => relP
              | #<< => shiftP | #>> => shiftP
              | #+ => addP | #- => addP
              | #* => mulP | #/ => mulP | #% => mulP
            (* end case *))
      fun prec (E_Grp _) = atomP
        | prec (E_AssignOp _) = assignP
        | prec (E_Cond _) = condP
        | prec (E_BinOp(_, rator, _)) = precOfBinop rator
        | prec (E_UnOp _) = preP
        | prec (E_PostOp _) = postP
        | prec (E_Apply _) = callP
        | prec (E_TApply _) = callP
        | prec (E_QId _) = atomP
        | prec (E_Cons _) = callP (* check this *)
        | prec (E_New _) = callP (* check this *)
        | prec (E_NewAt _) = callP (* check this *)
        | prec (E_Subscript _) = postP
        | prec (E_Select _) = postP
        | prec (E_Indirect _) = postP
        | prec (E_Cast _) = castP
        | prec (E_XCast _) = atomP
        | prec (E_Vec _) = castP
        | prec (E_Array _) = atomP
        | prec (E_Var _) = atomP
        | prec (E_Int _) = atomP
        | prec (E_Flt _) = atomP
        | prec (E_Bool _) = atomP
        | prec (E_Str _) = atomP
        | prec (E_Char _) = atomP
        | prec (E_Sizeof _) = callP
    in
    fun mkGrp e = if (prec e < atomP) then E_Grp e else e
    fun mkAssignOp (e1, rator, e2) = let
          val e1' = if prec e1 < unaryP then E_Grp e1 else e1
          val e2' = if prec e2 < assignP then E_Grp e2 else e2
          in
            E_AssignOp(e1', rator, e2')
          end
  (* note that we over-parenthesize here, but it makes nested conditionals easeier to read *)
    fun mkCond (e1, e2, e3) = E_Cond(
          if prec e1 <= condP then E_Grp e1 else e1,
          if prec e2 <= condP then E_Grp e2 else e2,
          if prec e3 < condP then E_Grp e3 else e3)
  (* Note that all C binary operators are left associative. *)
    fun mkBinOp (e1, #-, e2 as E_UnOp(%-, _)) = let
          val e1' = if prec e1 < addP then E_Grp e1 else e1
          val e2' = E_Grp e2
          in
            E_BinOp(e1', #-, e2')
          end
      | mkBinOp (e1, rator, e2) = let
          val p = precOfBinop rator
          val e1' = if prec e1 < p then E_Grp e1 else e1
          val e2' = if prec e2 <= p then E_Grp e2 else e2
          in
            E_BinOp(e1', rator, e2')
          end
    fun mkUnOp (%-, e as E_UnOp(%-, _)) = E_UnOp(%-, E_Grp e)
      | mkUnOp (%-, e as E_UnOp(%--, _)) = E_UnOp(%-, E_Grp e)
      | mkUnOp (%--, e as E_UnOp(%-, _)) = E_UnOp(%--, E_Grp e)
      | mkUnOp (%--, e as E_UnOp(%--, _)) = E_UnOp(%--, E_Grp e)
      | mkUnOp (%&, E_UnOp(%*, e)) = e
      | mkUnOp (%*, E_UnOp(%&, e)) = e
      | mkUnOp (rator, e) = if prec e < unaryP
          then E_UnOp(rator, E_Grp e)
          else E_UnOp(rator, e)
    fun mkPostOp (e, rator) = if prec e < postP
          then E_PostOp(E_Grp e, rator)
          else E_PostOp(e, rator)
    fun mkApply (f, args) = E_Apply(E_Var f, args)
    fun mkApplyExp (e, args) = if (prec e <= callP)
          then E_Apply(e, args)
          else E_Apply(E_Grp e, args)
    fun mkTemplateApply (f, tys, args) = E_TApply([], f, tys, args)
    fun mkQTemplateApply (prefix, f, tys, args) = E_TApply(prefix, f, tys, args)
    fun mkQId (prefix, id) = E_QId(prefix, id)
    fun mkQApply (prefix, f, args) = E_Apply(E_QId(prefix, f), args)
    val mkCons = E_Cons
    val mkNew = E_New
    val mkNewAt = E_NewAt
    fun mkSubscript(e1, e2) = if prec e1 < postP
          then E_Subscript(E_Grp e1, e2)
          else E_Subscript(e1, e2)
    fun mkSelect (e, f) = if prec e < postP
          then E_Select(E_Grp e, f)
          else E_Select(e, f)
    fun mkIndirect (e, f) = if prec e < postP
          then E_Indirect(E_Grp e, f)
          else E_Indirect(e, f)
    fun mkDispatch (e, meth, args) = mkApplyExp(mkSelect(e, meth), args)
    fun mkIndirectDispatch (e, meth, args) = mkApplyExp(mkIndirect(e, meth), args)
    fun mkCast (ty, e) = if prec e < castP
          then E_Cast(ty, E_Grp e)
          else E_Cast(ty, e)
    fun mkConstCast (ty, e) = E_XCast("const_cast", ty, e)
    fun mkDynamicCast (ty, e) = E_XCast("dynamic_cast", ty, e)
    fun mkReinterpretCast (ty, e) = E_XCast("reinterpret_cast", ty, e)
    fun mkStaticCast (ty, e) = E_XCast("static_cast", ty, e)
    val mkVec = E_Vec
    val mkArray = E_Array
    val mkVar = E_Var
    fun mkIntTy (n, ty) = if n < 0 then E_UnOp(%-, E_Int(~n, ty)) else E_Int(n, ty)
    fun mkInt n = mkIntTy(n, intTy)
    val mkFlt = E_Flt
    val mkBool = E_Bool
    val mkStr = E_Str
    val mkChar = E_Char
    val mkSizeof = E_Sizeof
    fun mkAddrOf x = mkUnOp(%&, x)
    end (* local *)

    val skip = S_Block[]

    local
      fun paren (e as E_Grp _) = e
        | paren e = E_Grp e
    in
    val mkComment = S_Comment
    fun mkBlock [stm] = stm
      | mkBlock stms = S_Block stms
    fun unBlock (S_Block stms) = stms
      | unBlock stm = [stm]
    fun prependStm (stm, blk) = mkBlock(stm :: unBlock blk)
    fun appendStm (blk, stm) = mkBlock(unBlock blk @ [stm])
    fun concatBlocks blocks = mkBlock(List.concat(List.map unBlock blocks))
    fun mkDecl (ty, x, init) = S_Decl([], ty, x, init)
    fun mkDeclInit (ty, x, init) = S_Decl([], ty, x, SOME(I_Exp init))
    val mkAttrDecl = S_Decl
    val mkExpStm = S_Exp
    fun mkAssign (e1, e2) = S_Exp(mkAssignOp(e1, $=, e2))
    fun mkAssign' (e1, rator, e2) = S_Exp(mkAssignOp(e1, rator, e2))
    fun mkIfThenElse (e, b1, b2) = S_If(paren e, b1, b2)
    fun mkIfThen (e, b) = mkIfThenElse (e, b, skip)
    fun mkSwitch (e, cases) = S_Switch(paren e, cases)
    fun mkCase (label, stms) = ([label], stms)
    fun mkDefault stms = ([], stms)
    val mkFor = S_For
    fun mkWhile (e, b) = S_While(paren e, b)
    fun mkDoWhile (b, e) = S_DoWhile(b, paren e)
    fun mkCall (f, args) = S_Exp(mkApply(f, args))
    fun mkCallExp (f, args) = S_Exp(mkApplyExp(f, args))
    fun mkTemplateCall (f, tys, args) = S_Exp(mkTemplateApply(f, tys, args))
    val voidReturn = S_Return NONE
    fun mkReturn exp = S_Return(SOME exp)
    val mkBreak = S_Break
    val mkContinue = S_Continue
    fun mkDelete e = S_Delete(false, e)
    fun mkDeleteArray e = S_Delete(true, e)
    end (* local *)

  (* smart constructors for declaration forms *)
    fun mkVarDcl (ty, x) = D_Var([], ty, [], x, NONE)
    fun mkVarInitDcl (ty, x, init) = D_Var([], ty, [], x, SOME init)
    fun mkProto (ty, f, params) = D_Func([], ty, [], f, params, NONE)
    fun mkFuncDcl (ty, f, params, body) = D_Func([], ty, [], f, params, SOME body)
  (* constructor prototype member declaration *)
    fun mkConstrProto (cls, params) = D_Constr([], [], cls, params, NONE)
  (* constructor function definition outside class body *)
    fun mkConstrDcl (cls, params, inits, body) =
          D_Constr([], [SC_Type(T_Named cls)], cls, params, SOME(inits, MP_Body body))
  (* destructor prototype member declaration *)
    fun mkDestrProto cls = D_Destr([], [], cls, NONE)
  (* destructor function definition outside class body *)
    fun mkDestrDcl (cls, body) = D_Destr([], [SC_Type(T_Named cls)], cls, SOME body)
  (* method prototype *)
    fun mkMethProto (ty, f, params) =
	  D_Meth([], ty, [], f, params, false, NONE)
    fun mkConstMethProto (ty, f, params) =
	  D_Meth([], ty, [], f, params, true, NONE)
  (* virtual method prototype *)
    fun mkVirtualProto (ty, f, params, isAbstract) =
	  D_Meth(["virtual"], ty, [], f, params, false, if isAbstract then SOME MP_0 else NONE)
    fun mkVirtualConstProto (ty, f, params, isAbstract) =
	  D_Meth(["virtual"], ty, [], f, params, true, if isAbstract then SOME MP_0 else NONE)
  (* static method prototype *)
    fun mkStaticMethProto (ty, f, params) =
	  D_Meth(["static"], ty, [], f, params, false, NONE)
  (* inline method definition inside class body *)
    fun mkInlineMethDcl (ty, f, params, body) =
	  D_Meth([], ty, [], f, params, false, SOME(MP_Body body))
    fun mkInlineConstMethDcl (ty, f, params, body) =
	  D_Meth([], ty, [], f, params, true, SOME(MP_Body body))
    fun mkInlineStaticMethDcl (ty, f, params, body) =
	  D_Meth(["static"], ty, [], f, params, false, SOME(MP_Body body))
  (* method definition outside class body *)
    fun mkMethDcl (cls, ty, f, params, body) =
	  D_Meth([], ty, [SC_Type(T_Named cls)], f, params, false, SOME(MP_Body body))
    fun mkConstMethDcl (cls, ty, f, params, body) =
	  D_Meth([], ty, [SC_Type(T_Named cls)], f, params, true, SOME(MP_Body body))
  (* simple function parameter *)
    fun param (ty, name) = PARAM([], ty, name)

  (* utility functions *)

    fun varToString x = x

    fun assignopToString rator = (case rator
           of $= => "="
            | += => "+="
            | *= => "*="
            | /= => "/="
            | %= => "%="
            | <<= => "<<="
            | >>= => ">>="
            | &= => "&="
            | ^= => "^="
            | |= => "|="
          (* end case *))

    fun binopToString rator = (case rator
           of #|| => "||"
            | #&& => "&&"
            | #== => "=="
            | #| => "|"
            | #^ => "^"
            | #& => "&"
            | #!= => "!="
            | #< => "<"
            | #<= => "<="
            | #>= => ">="
            | #> => ">"
            | #<< => "<<"
            | #>> => ">>"
            | #+ => "+"
            | #- => "-"
            | #* => "*"
            | #/ => "/"
            | #% => "%"
          (* end case *))

    fun unopToString rator = (case rator
           of %- => "-"
            | %! => "!"
            | %& => "&"
            | %* => "*"
            | %~ => "~"
            | %++ => "++"
            | %-- => "--"
          (* end case *))

    fun postopToString rator = (case rator
           of ^++ => "++"
            | ^-- => "--"
          (* end case *))

  end
