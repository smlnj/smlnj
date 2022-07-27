(* print-cxx.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Print C++ code.
 *)

structure PrintCxx : sig

    val output : TextIOPP.stream * Cxx.decl -> unit

  end = struct

    structure CL = Cxx
    structure PP = TextIOPP

    val indent0 = (PP.Abs 0)
    val indent2 = (PP.Abs 2)
    val indent = (PP.Abs 4)     (* standard indentation amount *)

    fun numTyName rty = (case rty
           of CL.NT_Int8 => "int8_t"
            | CL.NT_UInt8 => "uint8_t"
            | CL.NT_Int16 => "int16_t"
            | CL.NT_UInt16 => "uint16_t"
            | CL.NT_Int32 => "int32_t"
            | CL.NT_UInt32 => "uint32_t"
            | CL.NT_Int64 => "int64_t"
            | CL.NT_UInt64 => "uint64_t"
            | CL.NT_Float => "float"
            | CL.NT_Double => "double"
          (* end case *))

  (* convert a qualified name to a string *)
    fun qNameToS (scopes, name) = let
          fun ppQ [] = [name]
            | ppQ (CL.SC_Namespace ns :: scopes) = ns :: "::" :: ppQ scopes
            | ppQ (CL.SC_Type ty :: scopes) = tyToS' ty :: "::" :: ppQ scopes
          in
            String.concat(ppQ scopes)
          end

  (* convert a type plus optional varaible to a string *)
    and tyToS (ty, optVar)= let
          fun prefix (s, "") = s
            | prefix (s1, s2) = concat[s1, " ", s2]
          fun wrapArray (tyOp, CL.T_Array(ty', sz), acc) = let
                val suffix = (case sz
                       of NONE => "[]"
                        | SOME n => concat["[", Int.toString n, "]"]
                      (* end case *))
                in
                  toS (ty', concat[tyOp, "(", acc, suffix, ")"])
                end
            | wrapArray (tyOp, ty, acc) = toS (ty, prefix(tyOp, acc))
          and toS (ty, acc) = (case ty
                 of CL.T_Num nty => prefix(numTyName nty, acc)
                  | CL.T_Const ty => toS (ty, prefix("const", acc))
                  | CL.T_Ptr ty => wrapArray ("*", ty, acc)
                  | CL.T_Ref ty => wrapArray ("&", ty, acc)
                  | CL.T_RestrictPtr _ => wrapArray ("* __restrict__", ty, acc)
                  | CL.T_Array(ty, NONE) => toS (ty, acc ^ "[]")
                  | CL.T_Array(ty, SOME n) =>
                      toS (ty, concat[acc, "[", Int.toString n, "]"])
                  | CL.T_Named ty => prefix(ty, acc)
                  | CL.T_Template(ty, tyArgs) => let
                      val args = String.concatWithMap "," tyToS' tyArgs
                      val suffix = if acc = "" then [] else [" ", acc]
                      in
                        concat(
                          ty :: "<" :: args ::
                          (if String.isSuffix ">" args then " >" else ">") ::
                          suffix)
                      end
                  | CL.T_Qual(attr, ty) => concat[attr, " ", toS(ty, acc)]
                  | CL.T_Member(ty as CL.T_Template _, ty') => let
                      val suffix = if acc = "" then [] else [" ", acc]
                      in
                        concat(tyToS' ty :: "::" :: ty' :: suffix)
                      end
                  | CL.T_Member _ => raise Fail "ill-formed type member"
                (* end case *))
          in
            case optVar
             of SOME x => toS (ty, qNameToS x)
              | NONE => toS (ty, "")
            (* end case *)
          end

  (* convert a type to a string *)
    and tyToS' ty = tyToS (ty, NONE)

    fun output (_, CL.D_Verbatim[]) = ()
      | output (strm, decl) = let
          val str = PP.string strm
          fun sp () = PP.space strm 1
          fun nl () = PP.newline strm
          fun inHBox f = (PP.openHBox strm; f(); PP.closeBox strm)
          fun ppCom s = inHBox (fn () => (str "// "; str s))
          fun ppList {pp, sep, l} = let
                fun ppList' [] = ()
                  | ppList' [x] = pp x
                  | ppList' (x::xs) = (pp x; sep(); ppList' xs)
                in
                  ppList' l
                end
          fun ppCommaList {pp, l} = ppList {pp = pp, sep = fn () => (str ","; sp()), l = l}
          fun ppQName name = str(qNameToS name)
          fun ppTyAndVar (ty, x) = str (tyToS (ty, SOME x))
          fun ppTy ty = str (tyToS (ty, NONE))
          fun ppAttrs [] = ()
            | ppAttrs attrs = (
                ppList {pp=str, sep=sp, l = attrs};
                sp())
          fun ppDecl (inClass, dcl) = (case dcl
                 of CL.D_Pragma l => (
                      if inClass then nl() else ();
                      inHBox (fn () => (
                        str "#pragma";
                        List.app (fn s => (sp(); str s)) l)))
                  | CL.D_Comment[] => ()
                  | CL.D_Comment(com::coms) => (
                      if inClass then nl() else ();
                      ppCom com;
                      List.app (fn com => (nl(); ppCom com)) coms)
                  | CL.D_Verbatim [] => ()
                  | CL.D_Verbatim l => (if inClass then nl() else (); List.app str l)
                  | CL.D_Var(attrs, ty, scopes, x, optInit) => (
                      if inClass then nl() else ();
                      inHBox (fn () => (
                        ppAttrs attrs;
                        ppTyAndVar (ty, (scopes, x));
                        case optInit
                         of SOME init => (sp(); str "="; sp(); ppInit init)
                          | NONE => ()
                        (* end case *);
                        str ";")))
                  | CL.D_Func(attrs, ty, scopes, f, params, optBody) => (
                      if inClass then nl() else ();
                      PP.openVBox strm indent0;
                        inHBox (fn () => (
                          ppAttrs attrs;
                          ppTyAndVar(ty, (scopes, f)); sp(); str "(";
                          ppCommaList {pp=ppParam, l=params};
                          str ")"));
                        case optBody
                         of NONE => str ";"
                          | SOME body => (nl(); ppBody body)
                        (* end case *);
                      PP.closeBox strm)
                  | CL.D_Meth(attrs, ty, scopes, f, params, isConst, optBody) => (
                      if inClass then nl() else ();
                      PP.openVBox strm indent0;
                        inHBox (fn () => (
                          ppAttrs attrs;
                          ppTyAndVar(ty, (scopes, f)); sp(); str "(";
                          ppCommaList {pp=ppParam, l=params};
                          str ")";
			  if isConst then (sp(); str "const") else ()));
                        case optBody
                         of NONE => str ";"
                          | SOME CL.MP_Delete => raise Fail "unexpected 'delete'"
                          | SOME CL.MP_0 =>
                              inHBox (fn () => (sp(); str "="; sp(); str "0;"))
                          | SOME(CL.MP_Body body) => (nl(); ppBody body)
                        (* end case *);
                      PP.closeBox strm)
                  | CL.D_Constr(attrs, scopes, cls, params, initsAndBody) => let
		      fun ppHeader () = (
			    ppAttrs attrs;
			    if inClass
			      then str cls
			      else ppQName (scopes, cls);
			    sp(); str "(";
			    ppCommaList {pp=ppParam, l=params};
			    str ")")
		      in
			if inClass then nl() else ();
                        PP.openVBox strm indent0;
			case initsAndBody
			 of SOME([], CL.MP_Delete) => (
			      PP.openHBox strm;
				ppHeader ();
			        sp(); str "="; sp(); str "delete;";
			      PP.closeBox strm)
			  | SOME(inits, CL.MP_Body(CL.S_Block[])) =>
			      if List.null inits
				then (
				  PP.openHBox strm;
				    ppHeader ();
				    sp(); str "{ }";
				  PP.closeBox strm)
				else (
				  PP.openVBox strm indent2;
				    PP.openHBox strm;
				      ppHeader ();
				    PP.closeBox strm;
				    nl();
				    PP.openBox strm indent;
				      str ":"; sp();
				      ppCommaList {pp = ppExp, l = inits};
				    PP.closeBox strm;
				  PP.closeBox strm;
				  nl();
				  str "{ }")
			  | SOME(inits, CL.MP_Body body) => (
			      if List.null inits
				then (
				  PP.openHBox strm;
				    ppHeader ();
				  PP.closeBox strm)
				else (
				  PP.openVBox strm indent2;
				    PP.openHBox strm;
				      ppHeader ();
				    PP.closeBox strm;
				    nl();
				    PP.openBox strm indent;
				      str ":"; sp();
				      ppCommaList {pp = ppExp, l = inits};
				    PP.closeBox strm;
				  PP.closeBox strm);
			      nl();
			      ppBody body)
			  | SOME _ => raise Fail "ill-formed constructor"
			  | NONE => (str ";"; PP.closeBox strm) (* closes HBox *)
			(* end case *);
                        PP.closeBox strm
		      end
                  | CL.D_Destr(attrs, scopes, cls, body) => (
                      if inClass then nl() else ();
                      PP.openVBox strm indent0;
                        PP.openHBox strm;
                          ppAttrs attrs;
                          if inClass
                            then str("~" ^ cls)
                            else ppQName (scopes, "~" ^ cls);
                          sp();
                          case body
                           of NONE => (str "();"; PP.closeBox strm)
                            | SOME(CL.S_Block[]) => (str "() { }"; PP.closeBox strm)
                            | SOME body => (
                                str "()"; PP.closeBox strm; nl(); ppBody body)
                          (* end case *);
                        (* NOTE: HBox has been closed *)
                      PP.closeBox strm)
                  | CL.D_StructDef(SOME name, fields, NONE) => (
                      if inClass then nl() else ();
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "struct"; sp(); str name; sp(); str "{"));
                        PP.openVBox strm indent;
                          List.app (fn (ty, x) => (
                              nl();
                              inHBox (fn () => (ppTyAndVar(ty, ([], x)); str ";"))))
                            fields;
                        PP.closeBox strm;
                        nl();
                        str "};";
                      PP.closeBox strm)
                  | CL.D_StructDef(optStruct, fields, SOME tyName) => (
                      if inClass then nl() else ();
                      PP.openVBox strm indent0;
                        str "typedef struct {";
                        PP.openVBox strm indent;
                          List.app (fn (ty, x) => (
                              nl();
                              inHBox (fn () => (ppTyAndVar(ty, ([], x)); str ";"))))
                            fields;
                        PP.closeBox strm;
                        nl();
                        inHBox (fn () => (str "}"; sp(); str tyName; str ";"));
                      PP.closeBox strm)
                  | CL.D_StructDef(NONE, _, NONE) => raise Fail "unamed struct"
                  | CL.D_ClassDef{name, args, from, public, protected, private} => let
                      fun ppStart kind = inHBox (fn () => (
                            str kind; sp(); str name;
                            Option.map
                              (fn tys => (str "<"; ppCommaList {pp=ppTy, l=tys}; str ">"))
                                args;
                            Option.map (fn base => (sp(); str ":"; sp(); str base)) from;
                            sp(); str "{"))
                      in
                        if inClass then nl() else ();
                        PP.openVBox strm indent0;
                          case (protected, private)
                           of ([], []) => (
                                ppStart "struct";
                                PP.openVBox strm indent;
                                  List.app (fn dcl => ppDecl (true, dcl)) public;
                                PP.closeBox strm)
                            | _ => let
                                fun ppDecls (vis, []) = ()
                                  | ppDecls (vis, dcls) = (
                                      PP.openVBox strm indent2;
                                        nl(); str vis; str ":";
                                        PP.openVBox strm indent2;
                                          List.app (fn dcl => ppDecl (true, dcl)) dcls;
                                        PP.closeBox strm;
                                      PP.closeBox strm)
                                in
                                  ppStart "class";
                                  ppDecls ("public", public);
                                  ppDecls ("protected", protected);
                                  ppDecls ("private", private)
                                end
                          (* end case *);
                          nl();
                          str "};";
                        PP.closeBox strm
                      end
                  | CL.D_EnumDef{isClass, name, repTy, cons=[]} => (
                      if inClass then nl() else ();
                      inHBox (fn () => (
                        str "enum"; sp();
                        if isClass then (str "class"; sp()) else ();
                        str name; sp();
                        case repTy
                         of NONE => ()
                          | SOME ty => (str ":"; sp(); ppTy ty)
                        (* end case *);
                        str ";")))
                  | CL.D_EnumDef{isClass, name, repTy, cons=con::conr} => let
                      fun ppCon (name, NONE) = str name
                        | ppCon (name, SOME e) = inHBox (fn () => (
                            str name; sp(); str "="; sp(); ppExp e))
                      in
                        if inClass then nl() else ();
                        PP.openHVBox strm indent0;
                          inHBox (fn () => (
                            str "enum"; sp();
                            if isClass then (str "class"; sp()) else ();
                            str name; sp();
                            case repTy
                             of NONE => ()
                              | SOME ty => (str ":"; sp(); ppTy ty)
                            (* end case *);
                            str "{"));
                          PP.openHVBox strm indent;
                            PP.cut strm;
                            ppCon con;
                            List.app (fn c => (str ","; sp(); ppCon c)) conr;
                          PP.closeBox strm;
                          PP.cut strm;
                          str "};";
                        PP.closeBox strm
                      end
                  | CL.D_Template(params, dcl) => let
                      fun ppParam (CL.TypeParam name) = (str "typename"; sp(); str name)
                        | ppParam (CL.ConstParam(ty, name)) = (
                            str "const"; sp(); ppTy ty; sp(); str name)
                      in
                        if inClass then nl() else ();
                        PP.openVBox strm indent0;
                          inHBox (fn () => (
                            str "template"; sp(); str "<";
                            ppCommaList {pp = ppParam, l = params};
                            str ">"));
                          nl();
                          ppDecl (inClass, dcl);
                        PP.closeBox strm
                      end
                  | CL.D_Typedef(name, ty) => (
                      if inClass then nl() else ();
                      inHBox (fn () => (
                        str "using"; sp(); str name; sp(); str"="; sp(); ppTy ty; str ";")))
                  | CL.D_Namespace(name, dcls) => (
                      if inClass then raise Fail "unexpected namespace inside class decl" else ();
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "namespace"; sp(); str name; sp(); str "{"));
                        PP.openVBox strm indent;
                          List.app
                            (fn CL.D_Verbatim[] => ()
                              | dcl => (nl(); ppDecl(false, dcl))
                            ) dcls;
                        PP.closeBox strm;
                        nl();
                        inHBox (fn () => (str "}"; sp(); str("// namespace " ^ name)));
                      PP.closeBox strm)
                (* end case *))
          and ppBody (CL.S_Block stms) = ppBlock stms
            | ppBody stm = ppBlock [stm]
          and ppParam (CL.PARAM(attrs, ty, x)) = (
                ppAttrs attrs;
                ppTyAndVar(ty, ([], CL.varToString x)))
          and ppInit init = (case init
                 of CL.I_Exp e => ppExp e
                  | CL.I_Exps fields => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn init => (
                            PP.break strm;
                            inHBox (fn () => (ppInit init; str ","))))
                          fields;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Struct fields => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn (lab, init) => (
                            PP.break strm;
                            inHBox (fn () => (
                              str("." ^ lab); sp(); str "="; sp(); ppInit init; str ","))))
                          fields;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Array elems => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn (i, init) => (
                            PP.break strm;
                            inHBox (fn () => (
                              str(concat["[", Int.toString i, "]"]); sp(); str "="; sp();
                              ppInit init; str ","))))
                          elems;
                      PP.closeBox strm;
                      str "}")
                  | CL.I_Cons(ty, args) => (
                      PP.openHVBox strm indent;
                        ppTy ty; ppArgs args; str ";";
                      PP.closeBox strm)
                (* end case *))
          and ppBlock stms = (
                str "{";
                PP.openVBox strm indent;
                  List.app (fn stm => (nl(); ppStm stm)) stms;
                PP.closeBox strm;
                nl();
                str "}")
          and ppStm stm = (case stm
                 of CL.S_Block stms => ppBlock stms
                  | CL.S_Comment l => List.app ppCom l
                  | CL.S_Verbatim [] => ()
                  | CL.S_Verbatim (stm::stms) => (
                      str stm;
                      List.app (fn stm => (nl(); str stm)) stms)
                  | CL.S_Decl(attrs, ty, x, NONE) => inHBox (fn () => (
                      ppAttrs attrs;
                      ppTyAndVar(ty, ([], x)); str ";"))
                  | CL.S_Decl(attrs, ty, x, SOME e) => inHBox (fn () => (
                      ppAttrs attrs;
                      ppTyAndVar(ty, ([], x)); sp(); str "="; sp(); ppInit e; str ";"))
                  | CL.S_Exp e => inHBox (fn () => (ppExp e; str ";"))
                  | CL.S_If(e, blk, CL.S_Block[]) =>
                      inHBox (fn () => (str "if"; sp(); ppExp e; ppStmAsBlock blk))
                  | CL.S_If(e, blk1, stm as CL.S_If _) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "if"; sp(); ppExp e; ppStmAsBlock blk1));
                        nl();
                      PP.closeBox strm;
                      inHBox (fn () => (str "else"; sp(); ppStm stm)))
                  | CL.S_If(e, blk1, blk2) => (
                      PP.openVBox strm indent0;
                        inHBox (fn () => (str "if"; sp(); ppExp e; ppStmAsBlock blk1));
                        nl();
                        inHBox (fn () => (str "else"; ppStmAsBlock blk2));
                      PP.closeBox strm)
                  | CL.S_Switch(e, cases) => let
                      fun ppCase (labels, stms) = (
                            if List.null labels
                              then (nl(); str "default:")
                              else List.app
                                (fn lab => inHBox(fn () => (
                                    nl(); str "case"; sp(); str lab; str ":")))
                                  labels;
                            PP.openVBox strm indent2;
                              List.app (fn stm => (nl(); ppStm stm)) stms;
                            PP.closeBox strm)
                      in
                        PP.openVBox strm indent0;
                          inHBox (fn () => (str "switch"; sp(); ppExp e; sp(); str "{"));
                          PP.openVBox strm indent2;
                            List.app ppCase cases;
                          PP.closeBox strm;
                          nl (); str "}";
                        PP.closeBox strm
                      end
                  | CL.S_While(e, blk) =>
                      inHBox (fn () => (str "while"; sp(); ppExp e; ppStmAsBlock blk))
                  | CL.S_DoWhile(blk, e) =>
                      inHBox (fn () => (
                        str "do"; ppStmAsBlock blk; sp(); str "while"; sp(); ppExp e))
                  | CL.S_For(ty, dcls, cond, incrs, blk) => inHBox (fn () => (
                      str "for"; sp(); str "(";
                      case dcls
                       of (x, e)::rest => (
                            ppTyAndVar(ty, ([], x)); sp(); str "="; sp(); ppExp e;
                            List.app
                              (fn (x, e) => (str ","; sp(); str x; sp(); str "="; sp(); ppExp e))
                                rest)
                        | [] => ()
                      (* end case *);
                      str ";"; sp();
                      ppExp cond; str ";"; sp();
                      ppList {pp = ppExp, sep = fn () => str ",", l = incrs};
                      str ")";
                      ppStmAsBlock blk))
                  | CL.S_Return(SOME e) => inHBox (fn () => (str "return"; sp(); ppExp e; str ";"))
                  | CL.S_Return _ => str "return;"
                  | CL.S_Break => str "break;"
                  | CL.S_Continue => str "continue;"
                  | CL.S_Delete(isArr, e) => inHBox (fn () => (
                      if isArr then str "delete[]" else str "delete";
                      sp(); ppExp e; str ";"))
                (* end case *))
        (* force printing "{" "}" around a statement *)
          and ppStmAsBlock (CL.S_Block stms) = (sp(); ppBlock stms)
            | ppStmAsBlock stm = (sp(); ppBlock [stm])
          and ppExp e = (case e
                 of CL.E_Grp e => (str "("; ppExp e; str ")")
                  | CL.E_AssignOp(lhs, rator, rhs) => (
                      ppExp lhs; sp(); str(CL.assignopToString rator); sp(); ppExp rhs)
                  | CL.E_Cond(e1, e2, e3) => (
                      ppExp e1; sp(); str "?"; sp(); ppExp e2; sp(); str ":"; sp(); ppExp e3)
                  | CL.E_BinOp(e1, rator, e2) => (
                      ppExp e1; sp(); str(CL.binopToString rator); sp(); ppExp e2)
                  | CL.E_UnOp(rator, e) => (str(CL.unopToString rator); ppExp e)
                  | CL.E_PostOp(e, rator) => (ppExp e; str(CL.postopToString rator))
                  | CL.E_Apply(e, args) => (ppExp e; ppArgs args)
                  | CL.E_TApply(prefix, f, tys, args) => (
                      ppQName(prefix, f); str "<";
                      ppCommaList {
                          pp = fn ty => (PP.openHBox strm; ppTy ty; PP.closeBox strm),
                          l = tys
                        };
                      str ">"; ppArgs args)
                  | CL.E_QId(prefix, id) => ppQName(prefix, id)
                  | CL.E_Cons(ty, args) => (ppTy ty; ppArgs args)
                  | CL.E_New(ty, args) => (
                      str "new"; sp(); ppTy ty;
                      if null args then () else ppArgs args)
                  | CL.E_NewAt(addr, ty, args) => (
                      str "new"; sp(); str "("; ppExp addr; str ")"; sp(); ppTy ty;
                      if null args then () else ppArgs args)
                  | CL.E_Subscript(e1, e2) => (ppExp e1; str "["; ppExp e2; str "]")
                  | CL.E_Select(e, f) => (ppExp e; str "."; str f)
                  | CL.E_Indirect(e, f) => (ppExp e; str "->"; str f)
                  | CL.E_Cast(ty, e) => (str "("; ppTy ty; str ")"; ppExp e)
                  | CL.E_XCast(c, ty, e) => (
                      str c; str "<"; ppTy ty; str ">("; ppExp e; str ")")
                  | CL.E_Vec(ty, args) => (
                    (* GCC vector syntax: "__extension__ (ty){a, b, ...}" *)
                      str "__extension__"; sp(); str "("; ppTy ty; str ")";
                      str "{";
                      PP.openHOVBox strm indent;
                        PP.cut strm;
                        ppCommaList {
                            pp = fn e => (PP.openHBox strm; ppExp e; PP.closeBox strm),
                            l = args
                          };
                        str "}";
                      PP.closeBox strm)
                  | CL.E_Array exps => (
                      str "{";
                      PP.openHVBox strm indent;
                        List.app (fn e => (
                            PP.break strm;
                            inHBox (fn () => (ppExp e; str ","))))
                          exps;
                      PP.closeBox strm;
                      str "}")
                  | CL.E_Var x => str(CL.varToString x)
                  | CL.E_Int(n, ty) => let
                      val (prefix, n) = if (n < 0) then ("-", ~n) else ("", n)
                      val n = IntInf.toString n
                      in
                        case ty
                         of CL.T_Num(CL.NT_Int64) =>
                              str(concat["INT64_C(", prefix, n, ")"])
                          | CL.T_Num(CL.NT_UInt64) =>
                              str(concat["UINT64_C(", n, ")"])
                          | _ => str(prefix ^ n)
                      end
                  | CL.E_Flt f => str f
                  | CL.E_Bool b => str(Bool.toString b)
                  | CL.E_Str s => str(concat["\"", String.toCString s, "\""])
                  | CL.E_Char c => str(concat["'", Char.toCString c, "'"])
                  | CL.E_Sizeof ty => (str "sizeof("; ppTy ty; str ")")
                (* end case *))
          and ppArgs args = (
                str "(";
                PP.openHOVBox strm indent;
                  PP.cut strm;
                  ppCommaList {
                      pp = fn e => (PP.openHBox strm; ppExp e; PP.closeBox strm),
                      l = args
                    };
                  str ")";
                PP.closeBox strm)
          in
            PP.openVBox strm indent0;
              ppDecl (false, decl);
              nl();
            PP.closeBox strm
          end

  end
