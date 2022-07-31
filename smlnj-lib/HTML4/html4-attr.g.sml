structure HTML4AttrTokens =
  struct
    datatype token
      = NAME of Atom.atom
      | EQUALS
      | STRINGLIT of string
      | DOT
      | NUMBER of string
      | EOF
    val allToks = [
            EQUALS, DOT, EOF
           ]
    fun toString tok =
(case (tok)
 of (NAME(_)) => "NAME"
  | (EQUALS) => "="
  | (STRINGLIT(_)) => "STRINGLIT"
  | (DOT) => "."
  | (NUMBER(_)) => "NUMBER"
  | (EOF) => "EOF"
(* end case *))
    fun isKW tok =
(case (tok)
 of (NAME(_)) => false
  | (EQUALS) => false
  | (STRINGLIT(_)) => false
  | (DOT) => false
  | (NUMBER(_)) => false
  | (EOF) => false
(* end case *))
    fun isEOF EOF = true
      | isEOF _ = false
  end (* HTML4AttrTokens *)

functor HTML4AttrParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok =
HTML4AttrTokens
    structure UserCode =
      struct

fun attr_PROD_1_SUBRULE_1_PROD_1_ACT (attr_value, EQUALS, NAME, attr_value_SPAN : (Lex.pos * Lex.pos), EQUALS_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (attr_value)
fun attr_PROD_1_ACT (SR, NAME, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ((NAME, SR))
fun attr_value_PROD_2_SUBRULE_1_PROD_1_ACT (NAME, DOT, NAME_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NAME)
fun attr_value_PROD_2_ACT (SR, NAME, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ((Atom.toString NAME) ^ (String.concatWith "."
                                         (map Atom.toString SR)))
fun attr_value_PROD_3_SUBRULE_1_PROD_1_ACT (NUMBER, DOT, NUMBER_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NUMBER)
fun attr_value_PROD_3_ACT (SR, NUMBER, SR_SPAN : (Lex.pos * Lex.pos), NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  (NUMBER ^ (String.concatWith "." SR))
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
fun matchNAME strm = (case (lex(strm))
 of (Tok.NAME(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEQUALS strm = (case (lex(strm))
 of (Tok.EQUALS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSTRINGLIT strm = (case (lex(strm))
 of (Tok.STRINGLIT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchDOT strm = (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNUMBER strm = (case (lex(strm))
 of (Tok.NUMBER(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (attrs_NT) = 
let
fun attr_value_NT (strm) = let
      fun attr_value_PROD_1 (strm) = let
            val (STRINGLIT_RES, STRINGLIT_SPAN, strm') = matchSTRINGLIT(strm)
            val FULL_SPAN = (#1(STRINGLIT_SPAN), #2(STRINGLIT_SPAN))
            in
              ((STRINGLIT_RES), FULL_SPAN, strm')
            end
      fun attr_value_PROD_2 (strm) = let
            val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
            fun attr_value_PROD_2_SUBRULE_1_NT (strm) = let
                  val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm)
                  val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm')
                  val FULL_SPAN = (#1(DOT_SPAN), #2(NAME_SPAN))
                  in
                    (UserCode.attr_value_PROD_2_SUBRULE_1_PROD_1_ACT (NAME_RES, DOT_RES, NAME_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun attr_value_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.DOT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(attr_value_PROD_2_SUBRULE_1_PRED, attr_value_PROD_2_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(NAME_SPAN), #2(SR_SPAN))
            in
              (UserCode.attr_value_PROD_2_ACT (SR_RES, NAME_RES, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun attr_value_PROD_3 (strm) = let
            val (NUMBER_RES, NUMBER_SPAN, strm') = matchNUMBER(strm)
            fun attr_value_PROD_3_SUBRULE_1_NT (strm) = let
                  val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm)
                  val (NUMBER_RES, NUMBER_SPAN, strm') = matchNUMBER(strm')
                  val FULL_SPAN = (#1(DOT_SPAN), #2(NUMBER_SPAN))
                  in
                    (UserCode.attr_value_PROD_3_SUBRULE_1_PROD_1_ACT (NUMBER_RES, DOT_RES, NUMBER_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                      FULL_SPAN, strm')
                  end
            fun attr_value_PROD_3_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.DOT, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(attr_value_PROD_3_SUBRULE_1_PRED, attr_value_PROD_3_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(NUMBER_SPAN), #2(SR_SPAN))
            in
              (UserCode.attr_value_PROD_3_ACT (SR_RES, NUMBER_RES, SR_SPAN : (Lex.pos * Lex.pos), NUMBER_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.NUMBER(_), _, strm') => attr_value_PROD_3(strm)
          | (Tok.STRINGLIT(_), _, strm') => attr_value_PROD_1(strm)
          | (Tok.NAME(_), _, strm') => attr_value_PROD_2(strm)
          | _ => fail()
        (* end case *))
      end
fun attr_NT (strm) = let
      val (NAME_RES, NAME_SPAN, strm') = matchNAME(strm)
      fun attr_PROD_1_SUBRULE_1_NT (strm) = let
            val (EQUALS_RES, EQUALS_SPAN, strm') = matchEQUALS(strm)
            val (attr_value_RES, attr_value_SPAN, strm') = attr_value_NT(strm')
            val FULL_SPAN = (#1(EQUALS_SPAN), #2(attr_value_SPAN))
            in
              (UserCode.attr_PROD_1_SUBRULE_1_PROD_1_ACT (attr_value_RES, EQUALS_RES, NAME_RES, attr_value_SPAN : (Lex.pos * Lex.pos), EQUALS_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun attr_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.EQUALS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(attr_PROD_1_SUBRULE_1_PRED, attr_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(NAME_SPAN), #2(SR_SPAN))
      in
        (UserCode.attr_PROD_1_ACT (SR_RES, NAME_RES, SR_SPAN : (Lex.pos * Lex.pos), NAME_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun attrs_NT (strm) = let
      fun attrs_PROD_1_SUBRULE_1_NT (strm) = let
            val (attr_RES, attr_SPAN, strm') = attr_NT(strm)
            val FULL_SPAN = (#1(attr_SPAN), #2(attr_SPAN))
            in
              ((attr_RES), FULL_SPAN, strm')
            end
      fun attrs_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.NAME(_), _, strm') => true
              | _ => false
            (* end case *))
      val (attr_RES, attr_SPAN, strm') = EBNF.closure(attrs_PROD_1_SUBRULE_1_PRED, attrs_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(attr_SPAN), #2(attr_SPAN))
      in
        ((attr_RES), FULL_SPAN, strm')
      end
in
  (attrs_NT)
end
val attrs_NT =  fn s => unwrap (Err.launch (eh, lexFn, attrs_NT , true) s)

in (attrs_NT) end
  in
fun parse lexFn  s = let val (attrs_NT) = mk lexFn in attrs_NT s end

  end

end
