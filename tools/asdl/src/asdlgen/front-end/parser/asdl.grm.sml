structure ASDLTokens =
  struct
    datatype token
      = KW_alias
      | KW_attributes
      | KW_import
      | KW_include
      | KW_module
      | KW_primitive
      | KW_view
      | LPAREN
      | RPAREN
      | LBRACK
      | RBRACK
      | LBRACE
      | RBRACE
      | LEQ
      | COMMA
      | DOT
      | SEQUENCE
      | OPTIONAL
      | SHARED
      | EQ
      | PIPE
      | FILE
      | CODE of string
      | LID of Atom.atom
      | UID of Atom.atom
      | EOF
    val allToks = [
            KW_alias, KW_attributes, KW_import, KW_include, KW_module, KW_primitive, KW_view, LPAREN, RPAREN, LBRACK, RBRACK, LBRACE, RBRACE, LEQ, COMMA, DOT, SEQUENCE, OPTIONAL, SHARED, EQ, PIPE, FILE, EOF
           ]
    fun toString tok =
(case (tok)
 of (KW_alias) => "alias"
  | (KW_attributes) => "attributes"
  | (KW_import) => "import"
  | (KW_include) => "include"
  | (KW_module) => "module"
  | (KW_primitive) => "primitive"
  | (KW_view) => "view"
  | (LPAREN) => "("
  | (RPAREN) => ")"
  | (LBRACK) => "["
  | (RBRACK) => "]"
  | (LBRACE) => "{"
  | (RBRACE) => "}"
  | (LEQ) => "<="
  | (COMMA) => ","
  | (DOT) => "."
  | (SEQUENCE) => "*"
  | (OPTIONAL) => "?"
  | (SHARED) => "!"
  | (EQ) => "="
  | (PIPE) => "|"
  | (FILE) => "<file>"
  | (CODE(_)) => "CODE"
  | (LID(_)) => "LID"
  | (UID(_)) => "UID"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (KW_alias) => true
  | (KW_attributes) => true
  | (KW_import) => true
  | (KW_include) => true
  | (KW_module) => true
  | (KW_primitive) => true
  | (KW_view) => true
  | (LPAREN) => false
  | (RPAREN) => false
  | (LBRACK) => false
  | (RBRACK) => false
  | (LBRACE) => false
  | (RBRACE) => false
  | (LEQ) => false
  | (COMMA) => false
  | (DOT) => false
  | (SEQUENCE) => false
  | (OPTIONAL) => false
  | (SHARED) => false
  | (EQ) => false
  | (PIPE) => false
  | (FILE) => false
  | (CODE(_)) => false
  | (LID(_)) => false
  | (UID(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* ASDLTokens *)

functor ASDLParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
ASDLTokens
    structure UserCode =
      struct

  structure PT = ParseTree

  val aliasId = Atom.atom "alias"
  val attributesId = Atom.atom "attributes"
  val importId = Atom.atom "import"
  val includeId = Atom.atom "include"
  val moduleId = Atom.atom "module"
  val primitiveId = Atom.atom "primitive"
  val viewId = Atom.atom "view"


  fun mark cons (span : AntlrStreamPos.span, tr) = cons{span = span, tree = tr}

  fun markId (span : AntlrStreamPos.span, id) = {span = span, tree = id}

fun Root_PROD_1_ACT (MarkDecl, Include, MarkDecl_SPAN : (Lex.pos * Lex.pos), Include_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.File{includes = Include, decls = MarkDecl})
fun Include_PROD_1_ACT (CODE, KW_include, CODE_SPAN : (Lex.pos * Lex.pos), KW_include_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ({span = FULL_SPAN, tree = CODE})
fun MarkDecl_PROD_1_ACT (Decl, Decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.D_Mark (FULL_SPAN, Decl))
fun Decl_PROD_1_ACT (Id, LBRACE, KW_module, RBRACE, TypeDecl, ModuleImport, Id_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), KW_module_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), TypeDecl_SPAN : (Lex.pos * Lex.pos), ModuleImport_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.D_Module{name = Id, imports = ModuleImport, decls = TypeDecl})
fun Decl_PROD_2_ACT (KW_primitive, LBRACE, KW_module, RBRACE, Id1, Id2, KW_primitive_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), KW_module_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), Id1_SPAN : (Lex.pos * Lex.pos), Id2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.D_Primitive{name = Id1, exports = Id2})
fun Decl_PROD_3_ACT (Id, LBRACE, MarkViewEntry, RBRACE, KW_view, Id_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), MarkViewEntry_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), KW_view_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.D_View{name = Id, entries = MarkViewEntry})
fun ModuleImport_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ([])
fun ModuleImport_PROD_2_ACT (LPAREN, Import, RPAREN, LPAREN_SPAN : (Lex.pos * Lex.pos), Import_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Import)
fun Import_PROD_1_ACT (Id, SR, KW_import, Id_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), KW_import_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.Import_Mark (FULL_SPAN, PT.Import{module = Id, alias=SR}))
fun TypeDecl_PROD_1_ACT (EQ, TyId, TypeDef, EQ_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), TypeDef_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.TD_Mark (FULL_SPAN, TypeDef TyId))
fun TypeDef_PROD_1_ACT (TyCon, QualId, TyCon_SPAN : (Lex.pos * Lex.pos), QualId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn id => PT.TD_Alias{name = id, def = (QualId, TyCon)})
fun TypeDef_PROD_2_ACT (SR, Constructors, SR_SPAN : (Lex.pos * Lex.pos), Constructors_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn id => PT.TD_Sum{
			name = id,
			attribs = case SR of NONE => [] | SOME flds => flds,
			cons = Constructors
		      })
fun TypeDef_PROD_3_ACT (LPAREN, Fields, RPAREN, LPAREN_SPAN : (Lex.pos * Lex.pos), Fields_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (fn id => PT.TD_Product{name = id, fields = Fields})
fun QualId_PROD_1_ACT (TyId, TyId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NONE, TyId)
fun QualId_PROD_2_ACT (Id, TyId, DOT, Id_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (SOME Id, TyId)
fun Constructors_PROD_1_ACT (SR, Constructor, SR_SPAN : (Lex.pos * Lex.pos), Constructor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Constructor :: SR)
fun Constructor_PROD_1_ACT (ConId, optFields, ConId_SPAN : (Lex.pos * Lex.pos), optFields_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.Cons_Mark (FULL_SPAN, PT.Cons(ConId, optFields)))
fun optFields_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ([])
fun optFields_PROD_2_ACT (LPAREN, Fields, RPAREN, LPAREN_SPAN : (Lex.pos * Lex.pos), Fields_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Fields)
fun Fields_PROD_1_ACT (SR, Field, SR_SPAN : (Lex.pos * Lex.pos), Field_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (Field :: SR)
fun Field_PROD_1_ACT (Id, TyCon, QualId, Id_SPAN : (Lex.pos * Lex.pos), TyCon_SPAN : (Lex.pos * Lex.pos), QualId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (let
		    val fld = PT.Field{typ=QualId, tycon=TyCon, label=Id}
		    in
		      mark PT.Field_Mark (FULL_SPAN, fld)
		    end)
fun TyCon_PROD_1_ACT (OPTIONAL, OPTIONAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.Optional)
fun TyCon_PROD_2_ACT (SEQUENCE, SEQUENCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.Sequence)
fun TyCon_PROD_3_ACT (SHARED, SHARED_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.Shared)
fun Id_PROD_1_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markId (FULL_SPAN, LID))
fun Id_PROD_2_ACT (UID, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markId (FULL_SPAN, UID))
fun Id_PROD_3_ACT (Keyword, Keyword_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markId (FULL_SPAN, Keyword))
fun TyId_PROD_1_ACT (LID, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markId (FULL_SPAN, LID))
fun TyId_PROD_2_ACT (Keyword, Keyword_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markId (FULL_SPAN, Keyword))
fun ConId_PROD_1_ACT (UID, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (markId (FULL_SPAN, UID))
fun Keyword_PROD_1_ACT (KW_alias, KW_alias_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (aliasId)
fun Keyword_PROD_2_ACT (KW_attributes, KW_attributes_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (attributesId)
fun Keyword_PROD_3_ACT (KW_import, KW_import_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (importId)
fun Keyword_PROD_4_ACT (KW_include, KW_include_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (includeId)
fun Keyword_PROD_5_ACT (KW_module, KW_module_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (moduleId)
fun Keyword_PROD_6_ACT (KW_primitive, KW_primitive_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (primitiveId)
fun Keyword_PROD_7_ACT (KW_view, KW_view_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (viewId)
fun MarkViewEntry_PROD_1_ACT (ViewEntry, ViewEntry_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.VEntry_Mark (FULL_SPAN, ViewEntry))
fun ViewEntry_PROD_1_ACT (ViewProps, LEQ, ViewEntities, ViewProps_SPAN : (Lex.pos * Lex.pos), LEQ_SPAN : (Lex.pos * Lex.pos), ViewEntities_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntry(ViewEntities, ViewProps))
fun ViewEntry_PROD_2_ACT (Id, LBRACE, LEQ, RBRACE, MarkViewEntityCode, Id_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), LEQ_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), MarkViewEntityCode_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntry_Multiple(Id, MarkViewEntityCode))
fun MarkViewEntityCode_PROD_1_ACT (CODE, ViewEntity, CODE_SPAN : (Lex.pos * Lex.pos), ViewEntity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ViewEntity, CODE)
fun ViewEntities_PROD_1_ACT (MarkViewEntity, MarkViewEntity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ([MarkViewEntity])
fun ViewEntities_PROD_2_ACT (LBRACE, MarkViewEntity, RBRACE, LBRACE_SPAN : (Lex.pos * Lex.pos), MarkViewEntity_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (MarkViewEntity)
fun MarkViewEntity_PROD_1_ACT (ViewEntity, ViewEntity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.VEntity_Mark (FULL_SPAN, ViewEntity))
fun ViewEntity_PROD_1_ACT (FILE, FILE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntity_File)
fun ViewEntity_PROD_2_ACT (Id, KW_module, Id_SPAN : (Lex.pos * Lex.pos), KW_module_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntity_Module Id)
fun ViewEntity_PROD_3_ACT (Id, TyId, DOT, Id_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntity_Type(Id, TyId))
fun ViewEntity_PROD_4_ACT (Id, SEQUENCE, TyId, DOT1, DOT2, Id_SPAN : (Lex.pos * Lex.pos), SEQUENCE_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT1_SPAN : (Lex.pos * Lex.pos), DOT2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntity_AllCons(Id, TyId))
fun ViewEntity_PROD_5_ACT (Id, ConId, TyId, DOT1, DOT2, Id_SPAN : (Lex.pos * Lex.pos), ConId_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT1_SPAN : (Lex.pos * Lex.pos), DOT2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (PT.VEntity_Cons(Id, TyId, ConId))
fun ViewProps_PROD_1_ACT (ViewProp, ViewProp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ([ViewProp])
fun ViewProps_PROD_2_ACT (LBRACE, RBRACE, ViewProp, LBRACE_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), ViewProp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (ViewProp)
fun ViewProp_PROD_1_ACT (Id, CODE, Id_SPAN : (Lex.pos * Lex.pos), CODE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (mark PT.VProp_Mark (FULL_SPAN, (PT.VProp(Id, CODE))))
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)

(* replace functor with inline structure for better optimization
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)
*)
    structure EBNF =
      struct
	fun optional (pred, parse, strm) =
	      if pred strm
		then let
		  val (y, span, strm') = parse strm
		  in
		    (SOME y, span, strm')
		  end
		else (NONE, Err.getSpan strm, strm)

	fun closure (pred, parse, strm) = let
	      fun iter (strm, (left, right), ys) =
		    if pred strm
		      then let
			val (y, (_, right'), strm') = parse strm
			in iter (strm', (left, right'), y::ys)
			end
		      else (List.rev ys, (left, right), strm)
	      in
		iter (strm, Err.getSpan strm, [])
	      end

	fun posclos (pred, parse, strm) = let
	      val (y, (left, _), strm') = parse strm
	      val (ys, (_, right), strm'') = closure (pred, parse, strm')
	      in
		(y::ys, (left, right), strm'')
	      end
      end

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)
        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) =
	        (Err.whileDisabled eh (fn() => prod strm))
		handle Err.ParseError => try (prods)
          in try prods end
fun matchKW_alias strm = (case (lex(strm))
 of (Tok.KW_alias, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_attributes strm = (case (lex(strm))
 of (Tok.KW_attributes, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_import strm = (case (lex(strm))
 of (Tok.KW_import, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_include strm = (case (lex(strm))
 of (Tok.KW_include, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_module strm = (case (lex(strm))
 of (Tok.KW_module, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_primitive strm = (case (lex(strm))
 of (Tok.KW_primitive, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_view strm = (case (lex(strm))
 of (Tok.KW_view, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLPAREN strm = (case (lex(strm))
 of (Tok.LPAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRPAREN strm = (case (lex(strm))
 of (Tok.RPAREN, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLBRACK strm = (case (lex(strm))
 of (Tok.LBRACK, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRBRACK strm = (case (lex(strm))
 of (Tok.RBRACK, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLBRACE strm = (case (lex(strm))
 of (Tok.LBRACE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRBRACE strm = (case (lex(strm))
 of (Tok.RBRACE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLEQ strm = (case (lex(strm))
 of (Tok.LEQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDOT strm = (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEQUENCE strm = (case (lex(strm))
 of (Tok.SEQUENCE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchOPTIONAL strm = (case (lex(strm))
 of (Tok.OPTIONAL, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSHARED strm = (case (lex(strm))
 of (Tok.SHARED, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPIPE strm = (case (lex(strm))
 of (Tok.PIPE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchFILE strm = (case (lex(strm))
 of (Tok.FILE, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCODE strm = (case (lex(strm))
 of (Tok.CODE(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchLID strm = (case (lex(strm))
 of (Tok.LID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchUID strm = (case (lex(strm))
 of (Tok.UID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (Root_NT) = 
let
fun ConId_NT (strm) = let
      val (UID_RES, UID_SPAN, strm') = matchUID(strm)
      val FULL_SPAN = (#1(UID_SPAN), #2(UID_SPAN))
      in
        (UserCode.ConId_PROD_1_ACT (UID_RES, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Keyword_NT (strm) = let
      fun Keyword_PROD_1 (strm) = let
            val (KW_alias_RES, KW_alias_SPAN, strm') = matchKW_alias(strm)
            val FULL_SPAN = (#1(KW_alias_SPAN), #2(KW_alias_SPAN))
            in
              (UserCode.Keyword_PROD_1_ACT (KW_alias_RES, KW_alias_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Keyword_PROD_2 (strm) = let
            val (KW_attributes_RES, KW_attributes_SPAN, strm') = matchKW_attributes(strm)
            val FULL_SPAN = (#1(KW_attributes_SPAN), #2(KW_attributes_SPAN))
            in
              (UserCode.Keyword_PROD_2_ACT (KW_attributes_RES, KW_attributes_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Keyword_PROD_3 (strm) = let
            val (KW_import_RES, KW_import_SPAN, strm') = matchKW_import(strm)
            val FULL_SPAN = (#1(KW_import_SPAN), #2(KW_import_SPAN))
            in
              (UserCode.Keyword_PROD_3_ACT (KW_import_RES, KW_import_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Keyword_PROD_4 (strm) = let
            val (KW_include_RES, KW_include_SPAN, strm') = matchKW_include(strm)
            val FULL_SPAN = (#1(KW_include_SPAN), #2(KW_include_SPAN))
            in
              (UserCode.Keyword_PROD_4_ACT (KW_include_RES, KW_include_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Keyword_PROD_5 (strm) = let
            val (KW_module_RES, KW_module_SPAN, strm') = matchKW_module(strm)
            val FULL_SPAN = (#1(KW_module_SPAN), #2(KW_module_SPAN))
            in
              (UserCode.Keyword_PROD_5_ACT (KW_module_RES, KW_module_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Keyword_PROD_6 (strm) = let
            val (KW_primitive_RES, KW_primitive_SPAN, strm') = matchKW_primitive(strm)
            val FULL_SPAN = (#1(KW_primitive_SPAN), #2(KW_primitive_SPAN))
            in
              (UserCode.Keyword_PROD_6_ACT (KW_primitive_RES, KW_primitive_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Keyword_PROD_7 (strm) = let
            val (KW_view_RES, KW_view_SPAN, strm') = matchKW_view(strm)
            val FULL_SPAN = (#1(KW_view_SPAN), #2(KW_view_SPAN))
            in
              (UserCode.Keyword_PROD_7_ACT (KW_view_RES, KW_view_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_view, _, strm') => Keyword_PROD_7(strm)
          | (Tok.KW_module, _, strm') => Keyword_PROD_5(strm)
          | (Tok.KW_import, _, strm') => Keyword_PROD_3(strm)
          | (Tok.KW_alias, _, strm') => Keyword_PROD_1(strm)
          | (Tok.KW_attributes, _, strm') => Keyword_PROD_2(strm)
          | (Tok.KW_include, _, strm') => Keyword_PROD_4(strm)
          | (Tok.KW_primitive, _, strm') => Keyword_PROD_6(strm)
          | _ => fail()
        (* end case *))
      end
fun TyId_NT (strm) = let
      fun TyId_PROD_1 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.TyId_PROD_1_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyId_PROD_2 (strm) = let
            val (Keyword_RES, Keyword_SPAN, strm') = Keyword_NT(strm)
            val FULL_SPAN = (#1(Keyword_SPAN), #2(Keyword_SPAN))
            in
              (UserCode.TyId_PROD_2_ACT (Keyword_RES, Keyword_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_alias, _, strm') => TyId_PROD_2(strm)
          | (Tok.KW_attributes, _, strm') => TyId_PROD_2(strm)
          | (Tok.KW_import, _, strm') => TyId_PROD_2(strm)
          | (Tok.KW_include, _, strm') => TyId_PROD_2(strm)
          | (Tok.KW_module, _, strm') => TyId_PROD_2(strm)
          | (Tok.KW_primitive, _, strm') => TyId_PROD_2(strm)
          | (Tok.KW_view, _, strm') => TyId_PROD_2(strm)
          | (Tok.LID(_), _, strm') => TyId_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Id_NT (strm) = let
      fun Id_PROD_1 (strm) = let
            val (LID_RES, LID_SPAN, strm') = matchLID(strm)
            val FULL_SPAN = (#1(LID_SPAN), #2(LID_SPAN))
            in
              (UserCode.Id_PROD_1_ACT (LID_RES, LID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Id_PROD_2 (strm) = let
            val (UID_RES, UID_SPAN, strm') = matchUID(strm)
            val FULL_SPAN = (#1(UID_SPAN), #2(UID_SPAN))
            in
              (UserCode.Id_PROD_2_ACT (UID_RES, UID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Id_PROD_3 (strm) = let
            val (Keyword_RES, Keyword_SPAN, strm') = Keyword_NT(strm)
            val FULL_SPAN = (#1(Keyword_SPAN), #2(Keyword_SPAN))
            in
              (UserCode.Id_PROD_3_ACT (Keyword_RES, Keyword_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_alias, _, strm') => Id_PROD_3(strm)
          | (Tok.KW_attributes, _, strm') => Id_PROD_3(strm)
          | (Tok.KW_import, _, strm') => Id_PROD_3(strm)
          | (Tok.KW_include, _, strm') => Id_PROD_3(strm)
          | (Tok.KW_module, _, strm') => Id_PROD_3(strm)
          | (Tok.KW_primitive, _, strm') => Id_PROD_3(strm)
          | (Tok.KW_view, _, strm') => Id_PROD_3(strm)
          | (Tok.LID(_), _, strm') => Id_PROD_1(strm)
          | (Tok.UID(_), _, strm') => Id_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun ViewEntity_NT (strm) = let
      fun ViewEntity_PROD_1 (strm) = let
            val (FILE_RES, FILE_SPAN, strm') = matchFILE(strm)
            val FULL_SPAN = (#1(FILE_SPAN), #2(FILE_SPAN))
            in
              (UserCode.ViewEntity_PROD_1_ACT (FILE_RES, FILE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewEntity_PROD_2 (strm) = let
            val (KW_module_RES, KW_module_SPAN, strm') = matchKW_module(strm)
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm')
            val FULL_SPAN = (#1(KW_module_SPAN), #2(Id_SPAN))
            in
              (UserCode.ViewEntity_PROD_2_ACT (Id_RES, KW_module_RES, Id_SPAN : (Lex.pos * Lex.pos), KW_module_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewEntity_PROD_3 (strm) = let
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
            val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm')
            val (TyId_RES, TyId_SPAN, strm') = TyId_NT(strm')
            val FULL_SPAN = (#1(Id_SPAN), #2(TyId_SPAN))
            in
              (UserCode.ViewEntity_PROD_3_ACT (Id_RES, TyId_RES, DOT_RES, Id_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewEntity_PROD_4 (strm) = let
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
            val (DOT1_RES, DOT1_SPAN, strm') = matchDOT(strm')
            val (TyId_RES, TyId_SPAN, strm') = TyId_NT(strm')
            val (DOT2_RES, DOT2_SPAN, strm') = matchDOT(strm')
            val (SEQUENCE_RES, SEQUENCE_SPAN, strm') = matchSEQUENCE(strm')
            val FULL_SPAN = (#1(Id_SPAN), #2(SEQUENCE_SPAN))
            in
              (UserCode.ViewEntity_PROD_4_ACT (Id_RES, SEQUENCE_RES, TyId_RES, DOT1_RES, DOT2_RES, Id_SPAN : (Lex.pos * Lex.pos), SEQUENCE_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT1_SPAN : (Lex.pos * Lex.pos), DOT2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewEntity_PROD_5 (strm) = let
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
            val (DOT1_RES, DOT1_SPAN, strm') = matchDOT(strm')
            val (TyId_RES, TyId_SPAN, strm') = TyId_NT(strm')
            val (DOT2_RES, DOT2_SPAN, strm') = matchDOT(strm')
            val (ConId_RES, ConId_SPAN, strm') = ConId_NT(strm')
            val FULL_SPAN = (#1(Id_SPAN), #2(ConId_SPAN))
            in
              (UserCode.ViewEntity_PROD_5_ACT (Id_RES, ConId_RES, TyId_RES, DOT1_RES, DOT2_RES, Id_SPAN : (Lex.pos * Lex.pos), ConId_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT1_SPAN : (Lex.pos * Lex.pos), DOT2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_alias, _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_attributes, _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_import, _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_include, _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_primitive, _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.KW_view, _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.LID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.UID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | (Tok.FILE, _, strm') => ViewEntity_PROD_1(strm)
          | (Tok.KW_module, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.KW_attributes, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.KW_import, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.KW_include, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.KW_module, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.KW_primitive, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.KW_view, _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.LID(_), _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.UID(_), _, strm') => ViewEntity_PROD_2(strm)
                | (Tok.DOT, _, strm') =>
                    (case (lex(strm'))
                     of (Tok.KW_alias, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_attributes, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_import, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_include, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_module, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_primitive, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.KW_view, _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | (Tok.LID(_), _, strm') =>
                          (case (lex(strm'))
                           of (Tok.KW_alias, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_attributes, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_import, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_include, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_module, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_primitive, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.KW_view, _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.RBRACE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.LEQ, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.FILE, _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.CODE(_), _, strm') =>
                                ViewEntity_PROD_3(strm)
                            | (Tok.LID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.UID(_), _, strm') => ViewEntity_PROD_3(strm)
                            | (Tok.DOT, _, strm') =>
                                (case (lex(strm'))
                                 of (Tok.UID(_), _, strm') =>
                                      ViewEntity_PROD_5(strm)
                                  | (Tok.SEQUENCE, _, strm') =>
                                      ViewEntity_PROD_4(strm)
                                  | _ => fail()
                                (* end case *))
                            | _ => fail()
                          (* end case *))
                      | _ => fail()
                    (* end case *))
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun MarkViewEntityCode_NT (strm) = let
      val (ViewEntity_RES, ViewEntity_SPAN, strm') = ViewEntity_NT(strm)
      val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
      val FULL_SPAN = (#1(ViewEntity_SPAN), #2(CODE_SPAN))
      in
        (UserCode.MarkViewEntityCode_PROD_1_ACT (CODE_RES, ViewEntity_RES, CODE_SPAN : (Lex.pos * Lex.pos), ViewEntity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ViewProp_NT (strm) = let
      val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
      val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
      val FULL_SPAN = (#1(Id_SPAN), #2(CODE_SPAN))
      in
        (UserCode.ViewProp_PROD_1_ACT (Id_RES, CODE_RES, Id_SPAN : (Lex.pos * Lex.pos), CODE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ViewProps_NT (strm) = let
      fun ViewProps_PROD_1 (strm) = let
            val (ViewProp_RES, ViewProp_SPAN, strm') = ViewProp_NT(strm)
            val FULL_SPAN = (#1(ViewProp_SPAN), #2(ViewProp_SPAN))
            in
              (UserCode.ViewProps_PROD_1_ACT (ViewProp_RES, ViewProp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewProps_PROD_2 (strm) = let
            val (LBRACE_RES, LBRACE_SPAN, strm') = matchLBRACE(strm)
            fun ViewProps_PROD_2_SUBRULE_1_NT (strm) = let
                  val (ViewProp_RES, ViewProp_SPAN, strm') = ViewProp_NT(strm)
                  val FULL_SPAN = (#1(ViewProp_SPAN), #2(ViewProp_SPAN))
                  in
                    ((ViewProp_RES), FULL_SPAN, strm')
                  end
            fun ViewProps_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_alias, _, strm') => true
                    | (Tok.KW_attributes, _, strm') => true
                    | (Tok.KW_import, _, strm') => true
                    | (Tok.KW_include, _, strm') => true
                    | (Tok.KW_module, _, strm') => true
                    | (Tok.KW_primitive, _, strm') => true
                    | (Tok.KW_view, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (ViewProp_RES, ViewProp_SPAN, strm') = EBNF.closure(ViewProps_PROD_2_SUBRULE_1_PRED, ViewProps_PROD_2_SUBRULE_1_NT, strm')
            val (RBRACE_RES, RBRACE_SPAN, strm') = matchRBRACE(strm')
            val FULL_SPAN = (#1(LBRACE_SPAN), #2(RBRACE_SPAN))
            in
              (UserCode.ViewProps_PROD_2_ACT (LBRACE_RES, RBRACE_RES, ViewProp_RES, LBRACE_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), ViewProp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LBRACE, _, strm') => ViewProps_PROD_2(strm)
          | (Tok.KW_alias, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.KW_attributes, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.KW_import, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.KW_include, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.KW_module, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.KW_primitive, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.KW_view, _, strm') => ViewProps_PROD_1(strm)
          | (Tok.LID(_), _, strm') => ViewProps_PROD_1(strm)
          | (Tok.UID(_), _, strm') => ViewProps_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun MarkViewEntity_NT (strm) = let
      val (ViewEntity_RES, ViewEntity_SPAN, strm') = ViewEntity_NT(strm)
      val FULL_SPAN = (#1(ViewEntity_SPAN), #2(ViewEntity_SPAN))
      in
        (UserCode.MarkViewEntity_PROD_1_ACT (ViewEntity_RES, ViewEntity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ViewEntities_NT (strm) = let
      fun ViewEntities_PROD_1 (strm) = let
            val (MarkViewEntity_RES, MarkViewEntity_SPAN, strm') = MarkViewEntity_NT(strm)
            val FULL_SPAN = (#1(MarkViewEntity_SPAN), #2(MarkViewEntity_SPAN))
            in
              (UserCode.ViewEntities_PROD_1_ACT (MarkViewEntity_RES, MarkViewEntity_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewEntities_PROD_2 (strm) = let
            val (LBRACE_RES, LBRACE_SPAN, strm') = matchLBRACE(strm)
            fun ViewEntities_PROD_2_SUBRULE_1_NT (strm) = let
                  val (MarkViewEntity_RES, MarkViewEntity_SPAN, strm') = MarkViewEntity_NT(strm)
                  val FULL_SPAN = (#1(MarkViewEntity_SPAN),
                    #2(MarkViewEntity_SPAN))
                  in
                    ((MarkViewEntity_RES), FULL_SPAN, strm')
                  end
            fun ViewEntities_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_alias, _, strm') => true
                    | (Tok.KW_attributes, _, strm') => true
                    | (Tok.KW_import, _, strm') => true
                    | (Tok.KW_include, _, strm') => true
                    | (Tok.KW_module, _, strm') => true
                    | (Tok.KW_primitive, _, strm') => true
                    | (Tok.KW_view, _, strm') => true
                    | (Tok.FILE, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (MarkViewEntity_RES, MarkViewEntity_SPAN, strm') = EBNF.closure(ViewEntities_PROD_2_SUBRULE_1_PRED, ViewEntities_PROD_2_SUBRULE_1_NT, strm')
            val (RBRACE_RES, RBRACE_SPAN, strm') = matchRBRACE(strm')
            val FULL_SPAN = (#1(LBRACE_SPAN), #2(RBRACE_SPAN))
            in
              (UserCode.ViewEntities_PROD_2_ACT (LBRACE_RES, MarkViewEntity_RES, RBRACE_RES, LBRACE_SPAN : (Lex.pos * Lex.pos), MarkViewEntity_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LBRACE, _, strm') => ViewEntities_PROD_2(strm)
          | (Tok.KW_alias, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.KW_attributes, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.KW_import, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.KW_include, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.KW_module, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.KW_primitive, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.KW_view, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.FILE, _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.LID(_), _, strm') => ViewEntities_PROD_1(strm)
          | (Tok.UID(_), _, strm') => ViewEntities_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun ViewEntry_NT (strm) = let
      fun ViewEntry_PROD_1 (strm) = let
            val (ViewEntities_RES, ViewEntities_SPAN, strm') = ViewEntities_NT(strm)
            val (LEQ_RES, LEQ_SPAN, strm') = matchLEQ(strm')
            val (ViewProps_RES, ViewProps_SPAN, strm') = ViewProps_NT(strm')
            val FULL_SPAN = (#1(ViewEntities_SPAN), #2(ViewProps_SPAN))
            in
              (UserCode.ViewEntry_PROD_1_ACT (ViewProps_RES, LEQ_RES, ViewEntities_RES, ViewProps_SPAN : (Lex.pos * Lex.pos), LEQ_SPAN : (Lex.pos * Lex.pos), ViewEntities_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun ViewEntry_PROD_2 (strm) = let
            val (LEQ_RES, LEQ_SPAN, strm') = matchLEQ(strm)
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm')
            val (LBRACE_RES, LBRACE_SPAN, strm') = matchLBRACE(strm')
            fun ViewEntry_PROD_2_SUBRULE_1_NT (strm) = let
                  val (MarkViewEntityCode_RES, MarkViewEntityCode_SPAN, strm') = MarkViewEntityCode_NT(strm)
                  val FULL_SPAN = (#1(MarkViewEntityCode_SPAN),
                    #2(MarkViewEntityCode_SPAN))
                  in
                    ((MarkViewEntityCode_RES), FULL_SPAN, strm')
                  end
            fun ViewEntry_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_alias, _, strm') => true
                    | (Tok.KW_attributes, _, strm') => true
                    | (Tok.KW_import, _, strm') => true
                    | (Tok.KW_include, _, strm') => true
                    | (Tok.KW_module, _, strm') => true
                    | (Tok.KW_primitive, _, strm') => true
                    | (Tok.KW_view, _, strm') => true
                    | (Tok.FILE, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (MarkViewEntityCode_RES, MarkViewEntityCode_SPAN, strm') = EBNF.closure(ViewEntry_PROD_2_SUBRULE_1_PRED, ViewEntry_PROD_2_SUBRULE_1_NT, strm')
            val (RBRACE_RES, RBRACE_SPAN, strm') = matchRBRACE(strm')
            val FULL_SPAN = (#1(LEQ_SPAN), #2(RBRACE_SPAN))
            in
              (UserCode.ViewEntry_PROD_2_ACT (Id_RES, LBRACE_RES, LEQ_RES, RBRACE_RES, MarkViewEntityCode_RES, Id_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), LEQ_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), MarkViewEntityCode_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LEQ, _, strm') => ViewEntry_PROD_2(strm)
          | (Tok.KW_alias, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.KW_attributes, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.KW_import, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.KW_include, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.KW_module, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.KW_primitive, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.KW_view, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.LBRACE, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.FILE, _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.LID(_), _, strm') => ViewEntry_PROD_1(strm)
          | (Tok.UID(_), _, strm') => ViewEntry_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun MarkViewEntry_NT (strm) = let
      val (ViewEntry_RES, ViewEntry_SPAN, strm') = ViewEntry_NT(strm)
      val FULL_SPAN = (#1(ViewEntry_SPAN), #2(ViewEntry_SPAN))
      in
        (UserCode.MarkViewEntry_PROD_1_ACT (ViewEntry_RES, ViewEntry_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TyCon_NT (strm) = let
      fun TyCon_PROD_1 (strm) = let
            val (OPTIONAL_RES, OPTIONAL_SPAN, strm') = matchOPTIONAL(strm)
            val FULL_SPAN = (#1(OPTIONAL_SPAN), #2(OPTIONAL_SPAN))
            in
              (UserCode.TyCon_PROD_1_ACT (OPTIONAL_RES, OPTIONAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyCon_PROD_2 (strm) = let
            val (SEQUENCE_RES, SEQUENCE_SPAN, strm') = matchSEQUENCE(strm)
            val FULL_SPAN = (#1(SEQUENCE_SPAN), #2(SEQUENCE_SPAN))
            in
              (UserCode.TyCon_PROD_2_ACT (SEQUENCE_RES, SEQUENCE_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TyCon_PROD_3 (strm) = let
            val (SHARED_RES, SHARED_SPAN, strm') = matchSHARED(strm)
            val FULL_SPAN = (#1(SHARED_SPAN), #2(SHARED_SPAN))
            in
              (UserCode.TyCon_PROD_3_ACT (SHARED_RES, SHARED_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.SHARED, _, strm') => TyCon_PROD_3(strm)
          | (Tok.OPTIONAL, _, strm') => TyCon_PROD_1(strm)
          | (Tok.SEQUENCE, _, strm') => TyCon_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun QualId_NT (strm) = let
      fun QualId_PROD_1 (strm) = let
            val (TyId_RES, TyId_SPAN, strm') = TyId_NT(strm)
            val FULL_SPAN = (#1(TyId_SPAN), #2(TyId_SPAN))
            in
              (UserCode.QualId_PROD_1_ACT (TyId_RES, TyId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun QualId_PROD_2 (strm) = let
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
            val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm')
            val (TyId_RES, TyId_SPAN, strm') = TyId_NT(strm')
            val FULL_SPAN = (#1(Id_SPAN), #2(TyId_SPAN))
            in
              (UserCode.QualId_PROD_2_ACT (Id_RES, TyId_RES, DOT_RES, Id_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.UID(_), _, strm') => QualId_PROD_2(strm)
          | (Tok.KW_alias, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_attributes, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_import, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_include, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_module, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_primitive, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_view, _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.LID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_alias, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_attributes, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_import, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_include, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_module, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_primitive, _, strm') => QualId_PROD_1(strm)
                | (Tok.KW_view, _, strm') => QualId_PROD_1(strm)
                | (Tok.RPAREN, _, strm') => QualId_PROD_1(strm)
                | (Tok.RBRACE, _, strm') => QualId_PROD_1(strm)
                | (Tok.COMMA, _, strm') => QualId_PROD_1(strm)
                | (Tok.SEQUENCE, _, strm') => QualId_PROD_1(strm)
                | (Tok.OPTIONAL, _, strm') => QualId_PROD_1(strm)
                | (Tok.SHARED, _, strm') => QualId_PROD_1(strm)
                | (Tok.LID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.UID(_), _, strm') => QualId_PROD_1(strm)
                | (Tok.DOT, _, strm') => QualId_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun Field_NT (strm) = let
      val (QualId_RES, QualId_SPAN, strm') = QualId_NT(strm)
      fun Field_PROD_1_SUBRULE_1_NT (strm) = let
            val (TyCon_RES, TyCon_SPAN, strm') = TyCon_NT(strm)
            val FULL_SPAN = (#1(TyCon_SPAN), #2(TyCon_SPAN))
            in
              ((TyCon_RES), FULL_SPAN, strm')
            end
      fun Field_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEQUENCE, _, strm') => true
              | (Tok.OPTIONAL, _, strm') => true
              | (Tok.SHARED, _, strm') => true
              | _ => false
            (* end case *))
      val (TyCon_RES, TyCon_SPAN, strm') = EBNF.optional(Field_PROD_1_SUBRULE_1_PRED, Field_PROD_1_SUBRULE_1_NT, strm')
      fun Field_PROD_1_SUBRULE_2_NT (strm) = let
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
            val FULL_SPAN = (#1(Id_SPAN), #2(Id_SPAN))
            in
              ((Id_RES), FULL_SPAN, strm')
            end
      fun Field_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_alias, _, strm') => true
              | (Tok.KW_attributes, _, strm') => true
              | (Tok.KW_import, _, strm') => true
              | (Tok.KW_include, _, strm') => true
              | (Tok.KW_module, _, strm') => true
              | (Tok.KW_primitive, _, strm') => true
              | (Tok.KW_view, _, strm') => true
              | (Tok.LID(_), _, strm') => true
              | (Tok.UID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (Id_RES, Id_SPAN, strm') = EBNF.optional(Field_PROD_1_SUBRULE_2_PRED, Field_PROD_1_SUBRULE_2_NT, strm')
      val FULL_SPAN = (#1(QualId_SPAN), #2(Id_SPAN))
      in
        (UserCode.Field_PROD_1_ACT (Id_RES, TyCon_RES, QualId_RES, Id_SPAN : (Lex.pos * Lex.pos), TyCon_SPAN : (Lex.pos * Lex.pos), QualId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Fields_NT (strm) = let
      val (Field_RES, Field_SPAN, strm') = Field_NT(strm)
      fun Fields_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (Field_RES, Field_SPAN, strm') = Field_NT(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(Field_SPAN))
            in
              ((Field_RES), FULL_SPAN, strm')
            end
      fun Fields_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Fields_PROD_1_SUBRULE_1_PRED, Fields_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(Field_SPAN), #2(SR_SPAN))
      in
        (UserCode.Fields_PROD_1_ACT (SR_RES, Field_RES, SR_SPAN : (Lex.pos * Lex.pos), Field_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun optFields_NT (strm) = let
      fun optFields_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.optFields_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun optFields_PROD_2 (strm) = let
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm)
            val (Fields_RES, Fields_SPAN, strm') = Fields_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val FULL_SPAN = (#1(LPAREN_SPAN), #2(RPAREN_SPAN))
            in
              (UserCode.optFields_PROD_2_ACT (LPAREN_RES, Fields_RES, RPAREN_RES, LPAREN_SPAN : (Lex.pos * Lex.pos), Fields_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LPAREN, _, strm') => optFields_PROD_2(strm)
          | (Tok.KW_alias, _, strm') => optFields_PROD_1(strm)
          | (Tok.KW_attributes, _, strm') => optFields_PROD_1(strm)
          | (Tok.KW_import, _, strm') => optFields_PROD_1(strm)
          | (Tok.KW_include, _, strm') => optFields_PROD_1(strm)
          | (Tok.KW_module, _, strm') => optFields_PROD_1(strm)
          | (Tok.KW_primitive, _, strm') => optFields_PROD_1(strm)
          | (Tok.KW_view, _, strm') => optFields_PROD_1(strm)
          | (Tok.RBRACE, _, strm') => optFields_PROD_1(strm)
          | (Tok.PIPE, _, strm') => optFields_PROD_1(strm)
          | (Tok.LID(_), _, strm') => optFields_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Constructor_NT (strm) = let
      val (ConId_RES, ConId_SPAN, strm') = ConId_NT(strm)
      val (optFields_RES, optFields_SPAN, strm') = optFields_NT(strm')
      val FULL_SPAN = (#1(ConId_SPAN), #2(optFields_SPAN))
      in
        (UserCode.Constructor_PROD_1_ACT (ConId_RES, optFields_RES, ConId_SPAN : (Lex.pos * Lex.pos), optFields_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Constructors_NT (strm) = let
      val (Constructor_RES, Constructor_SPAN, strm') = Constructor_NT(strm)
      fun Constructors_PROD_1_SUBRULE_1_NT (strm) = let
            val (PIPE_RES, PIPE_SPAN, strm') = matchPIPE(strm)
            val (Constructor_RES, Constructor_SPAN, strm') = Constructor_NT(strm')
            val FULL_SPAN = (#1(PIPE_SPAN), #2(Constructor_SPAN))
            in
              ((Constructor_RES), FULL_SPAN, strm')
            end
      fun Constructors_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.PIPE, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(Constructors_PROD_1_SUBRULE_1_PRED, Constructors_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(Constructor_SPAN), #2(SR_SPAN))
      in
        (UserCode.Constructors_PROD_1_ACT (SR_RES, Constructor_RES, SR_SPAN : (Lex.pos * Lex.pos), Constructor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun TypeDef_NT (strm) = let
      fun TypeDef_PROD_1 (strm) = let
            val (QualId_RES, QualId_SPAN, strm') = QualId_NT(strm)
            fun TypeDef_PROD_1_SUBRULE_1_NT (strm) = let
                  val (TyCon_RES, TyCon_SPAN, strm') = TyCon_NT(strm)
                  val FULL_SPAN = (#1(TyCon_SPAN), #2(TyCon_SPAN))
                  in
                    ((TyCon_RES), FULL_SPAN, strm')
                  end
            fun TypeDef_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEQUENCE, _, strm') => true
                    | (Tok.OPTIONAL, _, strm') => true
                    | (Tok.SHARED, _, strm') => true
                    | _ => false
                  (* end case *))
            val (TyCon_RES, TyCon_SPAN, strm') = EBNF.optional(TypeDef_PROD_1_SUBRULE_1_PRED, TypeDef_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(QualId_SPAN), #2(TyCon_SPAN))
            in
              (UserCode.TypeDef_PROD_1_ACT (TyCon_RES, QualId_RES, TyCon_SPAN : (Lex.pos * Lex.pos), QualId_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TypeDef_PROD_2 (strm) = let
            val (Constructors_RES, Constructors_SPAN, strm') = Constructors_NT(strm)
            fun TypeDef_PROD_2_SUBRULE_1_NT (strm) = let
                  val (KW_attributes_RES, KW_attributes_SPAN, strm') = matchKW_attributes(strm)
                  val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm')
                  val (Fields_RES, Fields_SPAN, strm') = Fields_NT(strm')
                  val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
                  val FULL_SPAN = (#1(KW_attributes_SPAN), #2(RPAREN_SPAN))
                  in
                    ((Fields_RES), FULL_SPAN, strm')
                  end
            fun TypeDef_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_attributes, _, strm') =>
                        (case (lex(strm'))
                         of (Tok.LPAREN, _, strm') => true
                          | _ => false
                        (* end case *))
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.optional(TypeDef_PROD_2_SUBRULE_1_PRED, TypeDef_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(Constructors_SPAN), #2(SR_SPAN))
            in
              (UserCode.TypeDef_PROD_2_ACT (SR_RES, Constructors_RES, SR_SPAN : (Lex.pos * Lex.pos), Constructors_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun TypeDef_PROD_3 (strm) = let
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm)
            val (Fields_RES, Fields_SPAN, strm') = Fields_NT(strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val FULL_SPAN = (#1(LPAREN_SPAN), #2(RPAREN_SPAN))
            in
              (UserCode.TypeDef_PROD_3_ACT (LPAREN_RES, Fields_RES, RPAREN_RES, LPAREN_SPAN : (Lex.pos * Lex.pos), Fields_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LPAREN, _, strm') => TypeDef_PROD_3(strm)
          | (Tok.KW_alias, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.KW_attributes, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.KW_import, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.KW_include, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.KW_module, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.KW_primitive, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.KW_view, _, strm') => TypeDef_PROD_1(strm)
          | (Tok.LID(_), _, strm') => TypeDef_PROD_1(strm)
          | (Tok.UID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.DOT, _, strm') => TypeDef_PROD_1(strm)
                | (Tok.KW_alias, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.KW_attributes, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.KW_import, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.KW_include, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.KW_module, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.KW_primitive, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.KW_view, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.LPAREN, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.RBRACE, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.PIPE, _, strm') => TypeDef_PROD_2(strm)
                | (Tok.LID(_), _, strm') => TypeDef_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun TypeDecl_NT (strm) = let
      val (TyId_RES, TyId_SPAN, strm') = TyId_NT(strm)
      val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
      val (TypeDef_RES, TypeDef_SPAN, strm') = TypeDef_NT(strm')
      val FULL_SPAN = (#1(TyId_SPAN), #2(TypeDef_SPAN))
      in
        (UserCode.TypeDecl_PROD_1_ACT (EQ_RES, TyId_RES, TypeDef_RES, EQ_SPAN : (Lex.pos * Lex.pos), TyId_SPAN : (Lex.pos * Lex.pos), TypeDef_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Import_NT (strm) = let
      val (KW_import_RES, KW_import_SPAN, strm') = matchKW_import(strm)
      val (Id_RES, Id_SPAN, strm') = Id_NT(strm')
      fun Import_PROD_1_SUBRULE_1_NT (strm) = let
            val (KW_alias_RES, KW_alias_SPAN, strm') = matchKW_alias(strm)
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm')
            val FULL_SPAN = (#1(KW_alias_SPAN), #2(Id_SPAN))
            in
              ((Id_RES), FULL_SPAN, strm')
            end
      fun Import_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_alias, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(Import_PROD_1_SUBRULE_1_PRED, Import_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(KW_import_SPAN), #2(SR_SPAN))
      in
        (UserCode.Import_PROD_1_ACT (Id_RES, SR_RES, KW_import_RES, Id_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), KW_import_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun ModuleImport_NT (strm) = let
      fun ModuleImport_PROD_1 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.ModuleImport_PROD_1_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      fun ModuleImport_PROD_2 (strm) = let
            val (LPAREN_RES, LPAREN_SPAN, strm') = matchLPAREN(strm)
            fun ModuleImport_PROD_2_SUBRULE_1_NT (strm) = let
                  val (Import_RES, Import_SPAN, strm') = Import_NT(strm)
                  val FULL_SPAN = (#1(Import_SPAN), #2(Import_SPAN))
                  in
                    ((Import_RES), FULL_SPAN, strm')
                  end
            fun ModuleImport_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_import, _, strm') => true
                    | _ => false
                  (* end case *))
            val (Import_RES, Import_SPAN, strm') = EBNF.posclos(ModuleImport_PROD_2_SUBRULE_1_PRED, ModuleImport_PROD_2_SUBRULE_1_NT, strm')
            val (RPAREN_RES, RPAREN_SPAN, strm') = matchRPAREN(strm')
            val FULL_SPAN = (#1(LPAREN_SPAN), #2(RPAREN_SPAN))
            in
              (UserCode.ModuleImport_PROD_2_ACT (LPAREN_RES, Import_RES, RPAREN_RES, LPAREN_SPAN : (Lex.pos * Lex.pos), Import_SPAN : (Lex.pos * Lex.pos), RPAREN_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.LPAREN, _, strm') => ModuleImport_PROD_2(strm)
          | (Tok.LBRACE, _, strm') => ModuleImport_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun Decl_NT (strm) = let
      fun Decl_PROD_1 (strm) = let
            val (KW_module_RES, KW_module_SPAN, strm') = matchKW_module(strm)
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm')
            val (ModuleImport_RES, ModuleImport_SPAN, strm') = ModuleImport_NT(strm')
            val (LBRACE_RES, LBRACE_SPAN, strm') = matchLBRACE(strm')
            fun Decl_PROD_1_SUBRULE_1_NT (strm) = let
                  val (TypeDecl_RES, TypeDecl_SPAN, strm') = TypeDecl_NT(strm)
                  val FULL_SPAN = (#1(TypeDecl_SPAN), #2(TypeDecl_SPAN))
                  in
                    ((TypeDecl_RES), FULL_SPAN, strm')
                  end
            fun Decl_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_alias, _, strm') => true
                    | (Tok.KW_attributes, _, strm') => true
                    | (Tok.KW_import, _, strm') => true
                    | (Tok.KW_include, _, strm') => true
                    | (Tok.KW_module, _, strm') => true
                    | (Tok.KW_primitive, _, strm') => true
                    | (Tok.KW_view, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (TypeDecl_RES, TypeDecl_SPAN, strm') = EBNF.closure(Decl_PROD_1_SUBRULE_1_PRED, Decl_PROD_1_SUBRULE_1_NT, strm')
            val (RBRACE_RES, RBRACE_SPAN, strm') = matchRBRACE(strm')
            val FULL_SPAN = (#1(KW_module_SPAN), #2(RBRACE_SPAN))
            in
              (UserCode.Decl_PROD_1_ACT (Id_RES, LBRACE_RES, KW_module_RES, RBRACE_RES, TypeDecl_RES, ModuleImport_RES, Id_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), KW_module_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), TypeDecl_SPAN : (Lex.pos * Lex.pos), ModuleImport_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_2 (strm) = let
            val (KW_primitive_RES, KW_primitive_SPAN, strm') = matchKW_primitive(strm)
            val (KW_module_RES, KW_module_SPAN, strm') = matchKW_module(strm')
            val (Id1_RES, Id1_SPAN, strm') = Id_NT(strm')
            val (LBRACE_RES, LBRACE_SPAN, strm') = matchLBRACE(strm')
            fun Decl_PROD_2_SUBRULE_1_NT (strm) = let
                  val (Id_RES, Id_SPAN, strm') = Id_NT(strm)
                  val FULL_SPAN = (#1(Id_SPAN), #2(Id_SPAN))
                  in
                    ((Id_RES), FULL_SPAN, strm')
                  end
            fun Decl_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_alias, _, strm') => true
                    | (Tok.KW_attributes, _, strm') => true
                    | (Tok.KW_import, _, strm') => true
                    | (Tok.KW_include, _, strm') => true
                    | (Tok.KW_module, _, strm') => true
                    | (Tok.KW_primitive, _, strm') => true
                    | (Tok.KW_view, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (Id2_RES, Id2_SPAN, strm') = EBNF.closure(Decl_PROD_2_SUBRULE_1_PRED, Decl_PROD_2_SUBRULE_1_NT, strm')
            val (RBRACE_RES, RBRACE_SPAN, strm') = matchRBRACE(strm')
            val FULL_SPAN = (#1(KW_primitive_SPAN), #2(RBRACE_SPAN))
            in
              (UserCode.Decl_PROD_2_ACT (KW_primitive_RES, LBRACE_RES, KW_module_RES, RBRACE_RES, Id1_RES, Id2_RES, KW_primitive_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), KW_module_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), Id1_SPAN : (Lex.pos * Lex.pos), Id2_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun Decl_PROD_3 (strm) = let
            val (KW_view_RES, KW_view_SPAN, strm') = matchKW_view(strm)
            val (Id_RES, Id_SPAN, strm') = Id_NT(strm')
            val (LBRACE_RES, LBRACE_SPAN, strm') = matchLBRACE(strm')
            fun Decl_PROD_3_SUBRULE_1_NT (strm) = let
                  val (MarkViewEntry_RES, MarkViewEntry_SPAN, strm') = MarkViewEntry_NT(strm)
                  val FULL_SPAN = (#1(MarkViewEntry_SPAN),
                    #2(MarkViewEntry_SPAN))
                  in
                    ((MarkViewEntry_RES), FULL_SPAN, strm')
                  end
            fun Decl_PROD_3_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.KW_alias, _, strm') => true
                    | (Tok.KW_attributes, _, strm') => true
                    | (Tok.KW_import, _, strm') => true
                    | (Tok.KW_include, _, strm') => true
                    | (Tok.KW_module, _, strm') => true
                    | (Tok.KW_primitive, _, strm') => true
                    | (Tok.KW_view, _, strm') => true
                    | (Tok.LBRACE, _, strm') => true
                    | (Tok.LEQ, _, strm') => true
                    | (Tok.FILE, _, strm') => true
                    | (Tok.LID(_), _, strm') => true
                    | (Tok.UID(_), _, strm') => true
                    | _ => false
                  (* end case *))
            val (MarkViewEntry_RES, MarkViewEntry_SPAN, strm') = EBNF.closure(Decl_PROD_3_SUBRULE_1_PRED, Decl_PROD_3_SUBRULE_1_NT, strm')
            val (RBRACE_RES, RBRACE_SPAN, strm') = matchRBRACE(strm')
            val FULL_SPAN = (#1(KW_view_SPAN), #2(RBRACE_SPAN))
            in
              (UserCode.Decl_PROD_3_ACT (Id_RES, LBRACE_RES, MarkViewEntry_RES, RBRACE_RES, KW_view_RES, Id_SPAN : (Lex.pos * Lex.pos), LBRACE_SPAN : (Lex.pos * Lex.pos), MarkViewEntry_SPAN : (Lex.pos * Lex.pos), RBRACE_SPAN : (Lex.pos * Lex.pos), KW_view_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_view, _, strm') => Decl_PROD_3(strm)
          | (Tok.KW_module, _, strm') => Decl_PROD_1(strm)
          | (Tok.KW_primitive, _, strm') => Decl_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun MarkDecl_NT (strm) = let
      val (Decl_RES, Decl_SPAN, strm') = Decl_NT(strm)
      val FULL_SPAN = (#1(Decl_SPAN), #2(Decl_SPAN))
      in
        (UserCode.MarkDecl_PROD_1_ACT (Decl_RES, Decl_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Include_NT (strm) = let
      val (KW_include_RES, KW_include_SPAN, strm') = matchKW_include(strm)
      val (CODE_RES, CODE_SPAN, strm') = matchCODE(strm')
      val FULL_SPAN = (#1(KW_include_SPAN), #2(CODE_SPAN))
      in
        (UserCode.Include_PROD_1_ACT (CODE_RES, KW_include_RES, CODE_SPAN : (Lex.pos * Lex.pos), KW_include_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun Root_NT (strm) = let
      fun Root_PROD_1_SUBRULE_1_NT (strm) = let
            val (Include_RES, Include_SPAN, strm') = Include_NT(strm)
            val FULL_SPAN = (#1(Include_SPAN), #2(Include_SPAN))
            in
              ((Include_RES), FULL_SPAN, strm')
            end
      fun Root_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_include, _, strm') => true
              | _ => false
            (* end case *))
      val (Include_RES, Include_SPAN, strm') = EBNF.closure(Root_PROD_1_SUBRULE_1_PRED, Root_PROD_1_SUBRULE_1_NT, strm)
      fun Root_PROD_1_SUBRULE_2_NT (strm) = let
            val (MarkDecl_RES, MarkDecl_SPAN, strm') = MarkDecl_NT(strm)
            val FULL_SPAN = (#1(MarkDecl_SPAN), #2(MarkDecl_SPAN))
            in
              ((MarkDecl_RES), FULL_SPAN, strm')
            end
      fun Root_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_module, _, strm') => true
              | (Tok.KW_primitive, _, strm') => true
              | (Tok.KW_view, _, strm') => true
              | _ => false
            (* end case *))
      val (MarkDecl_RES, MarkDecl_SPAN, strm') = EBNF.posclos(Root_PROD_1_SUBRULE_2_PRED, Root_PROD_1_SUBRULE_2_NT, strm')
      val FULL_SPAN = (#1(Include_SPAN), #2(MarkDecl_SPAN))
      in
        (UserCode.Root_PROD_1_ACT (MarkDecl_RES, Include_RES, MarkDecl_SPAN : (Lex.pos * Lex.pos), Include_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (Root_NT)
end
val Root_NT =  fn s => unwrap (Err.launch (eh, lexFn, Root_NT , true) s)

in (Root_NT) end
  in
fun parse lexFn  s = let val (Root_NT) = mk lexFn in Root_NT s end

  end

end
