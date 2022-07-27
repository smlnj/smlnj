structure 
Tokens = struct

    datatype token = EOF
      | REAL of Real.real
      | INT of IntInf.int
      | ID of string
      | MULOP of string
      | ADDOP of string
      | RELOP of string
      | MINUS
      | RP
      | LP
      | RSB
      | LSB
      | DOT
      | SEMI
      | COLON
      | COMMA
      | ASSIGNOP
      | KW_not
      | KW_do
      | KW_while
      | KW_else
      | KW_then
      | KW_if
      | KW_end
      | KW_begin
      | KW_procedure
      | KW_function
      | KW_real
      | KW_integer
      | KW_of
      | KW_array
      | KW_var
      | KW_program

    val allToks = [EOF, ID("bogus"), MINUS, RP, LP, RSB, LSB, DOT, SEMI, COLON, COMMA, ASSIGNOP, KW_not, KW_do, KW_while, KW_else, KW_then, KW_if, KW_end, KW_begin, KW_procedure, KW_function, KW_real, KW_integer, KW_of, KW_array, KW_var, KW_program]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (REAL(_)) => "REAL"
  | (INT(_)) => "INT"
  | (ID(_)) => "ID"
  | (MULOP(_)) => "MULOP"
  | (ADDOP(_)) => "ADDOP"
  | (RELOP(_)) => "RELOP"
  | (MINUS) => "-"
  | (RP) => ")"
  | (LP) => "("
  | (RSB) => "]"
  | (LSB) => "["
  | (DOT) => "."
  | (SEMI) => ";"
  | (COLON) => ":"
  | (COMMA) => ","
  | (ASSIGNOP) => ":="
  | (KW_not) => "not"
  | (KW_do) => "do"
  | (KW_while) => "while"
  | (KW_else) => "else"
  | (KW_then) => "then"
  | (KW_if) => "if"
  | (KW_end) => "end"
  | (KW_begin) => "begin"
  | (KW_procedure) => "procedure"
  | (KW_function) => "function"
  | (KW_real) => "real"
  | (KW_integer) => "integer"
  | (KW_of) => "of"
  | (KW_array) => "array"
  | (KW_var) => "var"
  | (KW_program) => "program"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (REAL(_)) => false
  | (INT(_)) => false
  | (ID(_)) => false
  | (MULOP(_)) => false
  | (ADDOP(_)) => false
  | (RELOP(_)) => false
  | (MINUS) => false
  | (RP) => false
  | (LP) => false
  | (RSB) => false
  | (LSB) => false
  | (DOT) => false
  | (SEMI) => false
  | (COLON) => false
  | (COMMA) => false
  | (ASSIGNOP) => false
  | (KW_not) => true
  | (KW_do) => true
  | (KW_while) => true
  | (KW_else) => true
  | (KW_then) => true
  | (KW_if) => true
  | (KW_end) => true
  | (KW_begin) => true
  | (KW_procedure) => true
  | (KW_function) => true
  | (KW_real) => true
  | (KW_integer) => true
  | (KW_of) => true
  | (KW_array) => true
  | (KW_var) => true
  | (KW_program) => true
(* end case *))
    val changes = []


  fun isEOF EOF = true
    | isEOF _ = false

end

functor ParseFn(Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
Tokens
    structure UserCode =
      struct

  val cat     = String.concat
  val catSp   = String.concatWith " "
  val catNl   = String.concatWith "\n"
  val catNlNl = String.concatWith "\n\n"
  val catCm   = String.concatWith ", "
  val catSemi = String.concatWith "; "
  val catSemiNl = String.concatWith ";\n"

fun program_PROD_1_ACT (ID, LP, RP, DOT, SR1, SR2, SEMI, compound_statement, id_list, KW_program, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), compound_statement_SPAN : (Lex.pos * Lex.pos), id_list_SPAN : (Lex.pos * Lex.pos), KW_program_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catNl [ 
	 cat ["program ", ID, "(", id_list, ");"],
	 catNl   SR1, catNlNl SR2,
	 compound_statement ^ "."
      ])
fun id_list_PROD_1_ACT (ID, SR, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catCm (ID::SR) )
fun declaration_PROD_1_ACT (SEMI, id_list_type, KW_var, SEMI_SPAN : (Lex.pos * Lex.pos), id_list_type_SPAN : (Lex.pos * Lex.pos), KW_var_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat ["var ", id_list_type, ";"] )
fun compound_type_PROD_2_ACT (LSB, RSB, DOT1, DOT2, INT1, INT2, KW_array, standard_type, KW_of, LSB_SPAN : (Lex.pos * Lex.pos), RSB_SPAN : (Lex.pos * Lex.pos), DOT1_SPAN : (Lex.pos * Lex.pos), DOT2_SPAN : (Lex.pos * Lex.pos), INT1_SPAN : (Lex.pos * Lex.pos), INT2_SPAN : (Lex.pos * Lex.pos), KW_array_SPAN : (Lex.pos * Lex.pos), standard_type_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat ["array [", IntInf.toString INT1, "..", IntInf.toString INT2, "] of ", standard_type] )
fun standard_type_PROD_1_ACT (KW_integer, KW_integer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "integer" )
fun standard_type_PROD_2_ACT (KW_real, KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "real" )
fun id_list_type_PROD_1_ACT (compound_type, COLON, id_list, compound_type_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), id_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catSp [id_list, ":", compound_type] )
fun subprogram_declaration_PROD_1_ACT (SR, SEMI, compound_statement, subprogram_head, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), compound_statement_SPAN : (Lex.pos * Lex.pos), subprogram_head_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catNl [subprogram_head, catNl SR, compound_statement ^ ";"] )
fun subprogram_head_PROD_1_ACT (ID, SEMI, standard_type, COLON, arguments, KW_function, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), standard_type_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), arguments_SPAN : (Lex.pos * Lex.pos), KW_function_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat ["function ", ID, arguments, " : ", standard_type, ";"] )
fun subprogram_head_PROD_2_ACT (ID, SEMI, KW_procedure, arguments, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_procedure_SPAN : (Lex.pos * Lex.pos), arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat ["procedure ", ID, arguments, ";"] )
fun arguments_PROD_1_ACT (LP, RP, parameter_list, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), parameter_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "(" ^ parameter_list ^ ")" )
fun arguments_PROD_2_ACT (FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "" )
fun parameter_list_PROD_1_ACT (SR, id_list_type, SR_SPAN : (Lex.pos * Lex.pos), id_list_type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catSemi (id_list_type::SR) )
fun compound_statement_PROD_1_SUBRULE_1_PROD_1_ACT (SR, KW_begin, statement, SR_SPAN : (Lex.pos * Lex.pos), KW_begin_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catSemiNl (statement::SR) )
fun compound_statement_PROD_1_ACT (SR, KW_begin, KW_end, SR_SPAN : (Lex.pos * Lex.pos), KW_begin_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catNl ["begin", getOpt(SR, ""), "end"] )
fun statement_PROD_1_ACT (exp, variable, ASSIGNOP, exp_SPAN : (Lex.pos * Lex.pos), variable_SPAN : (Lex.pos * Lex.pos), ASSIGNOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat [variable, " := ", exp] )
fun statement_PROD_4_ACT (exp, KW_else, KW_then, statement1, statement2, KW_if, exp_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), statement1_SPAN : (Lex.pos * Lex.pos), statement2_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat ["if ", exp, " then ", statement1, " else ", statement2] )
fun statement_PROD_5_ACT (exp, KW_while, KW_do, statement, exp_SPAN : (Lex.pos * Lex.pos), KW_while_SPAN : (Lex.pos * Lex.pos), KW_do_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat ["while ", exp, " do ", statement] )
fun variable_PROD_2_ACT (ID, LSB, RSB, exp, ID_SPAN : (Lex.pos * Lex.pos), LSB_SPAN : (Lex.pos * Lex.pos), RSB_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat [ID, "[", exp, "]"] )
fun procedure_statement_PROD_2_ACT (ID, LP, RP, SR, exp, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat [ID, "(", catCm (exp::SR), ")" ] )
fun exp_PROD_1_SUBRULE_1_PROD_1_ACT (simple_exp, RELOP, simple_exp_SPAN : (Lex.pos * Lex.pos), RELOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( RELOP ^ " " ^ simple_exp )
fun exp_PROD_1_ACT (SR, simple_exp, SR_SPAN : (Lex.pos * Lex.pos), simple_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catSp [simple_exp, getOpt(SR, "")] )
fun simple_exp_PROD_1_SUBRULE_1_PROD_1_ACT (signed_term, ADDOP, signed_term_SPAN : (Lex.pos * Lex.pos), ADDOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( ADDOP ^ " " ^signed_term )
fun simple_exp_PROD_1_ACT (SR, signed_term, SR_SPAN : (Lex.pos * Lex.pos), signed_term_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catSp (signed_term::SR) )
fun signed_term_PROD_1_ACT (term, MINUS, term_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "-" ^ term )
fun term_PROD_1_SUBRULE_1_PROD_1_ACT (factor, MULOP, factor_SPAN : (Lex.pos * Lex.pos), MULOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( MULOP ^ " " ^ factor )
fun term_PROD_1_ACT (SR, factor, SR_SPAN : (Lex.pos * Lex.pos), factor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( catSp (factor::SR) )
fun factor_PROD_2_ACT (ID, LP, RP, SR, exp, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( cat [ID, "(", catCm (exp::SR), ")" ] )
fun factor_PROD_3_ACT (INT, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( IntInf.toString INT )
fun factor_PROD_4_ACT (REAL, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( Real.toString REAL )
fun factor_PROD_5_ACT (LP, RP, exp, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "(" ^ exp ^ ")" )
fun factor_PROD_6_ACT (factor, KW_not, factor_SPAN : (Lex.pos * Lex.pos), KW_not_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)) = 
  ( "not " ^ factor )
      end (* UserCode *)

    structure Err = AntlrErrHandler(
      structure Tok = Tok
      structure Lex = Lex)
    structure EBNF = AntlrEBNF(
      struct
	type strm = Err.wstream
	val getSpan = Err.getSpan
      end)

    fun mk lexFn = let
fun getS() = {}
fun putS{} = ()
fun unwrap (ret, strm, repairs) = (ret, strm, repairs)        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
	fun fail() = Err.failure eh
	fun tryProds (strm, prods) = let
	  fun try [] = fail()
	    | try (prod :: prods) = 
	        (Err.whileDisabled eh (fn() => prod strm)) 
		handle Err.ParseError => try (prods)
          in try prods end
fun matchEOF strm = (case (lex(strm))
 of (Tok.EOF, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchREAL strm = (case (lex(strm))
 of (Tok.REAL(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchINT strm = (case (lex(strm))
 of (Tok.INT(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchMULOP strm = (case (lex(strm))
 of (Tok.MULOP(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchADDOP strm = (case (lex(strm))
 of (Tok.ADDOP(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchRELOP strm = (case (lex(strm))
 of (Tok.RELOP(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRP strm = (case (lex(strm))
 of (Tok.RP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLP strm = (case (lex(strm))
 of (Tok.LP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchRSB strm = (case (lex(strm))
 of (Tok.RSB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchLSB strm = (case (lex(strm))
 of (Tok.LSB, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchDOT strm = (case (lex(strm))
 of (Tok.DOT, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOLON strm = (case (lex(strm))
 of (Tok.COLON, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchCOMMA strm = (case (lex(strm))
 of (Tok.COMMA, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchASSIGNOP strm = (case (lex(strm))
 of (Tok.ASSIGNOP, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_not strm = (case (lex(strm))
 of (Tok.KW_not, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_do strm = (case (lex(strm))
 of (Tok.KW_do, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_while strm = (case (lex(strm))
 of (Tok.KW_while, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_else strm = (case (lex(strm))
 of (Tok.KW_else, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_then strm = (case (lex(strm))
 of (Tok.KW_then, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_if strm = (case (lex(strm))
 of (Tok.KW_if, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_end strm = (case (lex(strm))
 of (Tok.KW_end, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_begin strm = (case (lex(strm))
 of (Tok.KW_begin, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_procedure strm = (case (lex(strm))
 of (Tok.KW_procedure, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_function strm = (case (lex(strm))
 of (Tok.KW_function, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_real strm = (case (lex(strm))
 of (Tok.KW_real, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_integer strm = (case (lex(strm))
 of (Tok.KW_integer, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_of strm = (case (lex(strm))
 of (Tok.KW_of, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_array strm = (case (lex(strm))
 of (Tok.KW_array, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_var strm = (case (lex(strm))
 of (Tok.KW_var, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_program strm = (case (lex(strm))
 of (Tok.KW_program, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (program_NT) = 
let
fun exp_NT (strm) = let
      val (simple_exp_RES, simple_exp_SPAN, strm') = simple_exp_NT(strm)
      fun exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (RELOP_RES, RELOP_SPAN, strm') = matchRELOP(strm)
            val (simple_exp_RES, simple_exp_SPAN, strm') = simple_exp_NT(strm')
            val FULL_SPAN = (#1(RELOP_SPAN), #2(simple_exp_SPAN))
            in
              (UserCode.exp_PROD_1_SUBRULE_1_PROD_1_ACT (simple_exp_RES, RELOP_RES, simple_exp_SPAN : (Lex.pos * Lex.pos), RELOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.RELOP(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(exp_PROD_1_SUBRULE_1_PRED, exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(simple_exp_SPAN), #2(SR_SPAN))
      in
        (UserCode.exp_PROD_1_ACT (SR_RES, simple_exp_RES, SR_SPAN : (Lex.pos * Lex.pos), simple_exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and simple_exp_NT (strm) = let
      val (signed_term_RES, signed_term_SPAN, strm') = signed_term_NT(strm)
      fun simple_exp_PROD_1_SUBRULE_1_NT (strm) = let
            val (ADDOP_RES, ADDOP_SPAN, strm') = matchADDOP(strm)
            val (signed_term_RES, signed_term_SPAN, strm') = signed_term_NT(strm')
            val FULL_SPAN = (#1(ADDOP_SPAN), #2(signed_term_SPAN))
            in
              (UserCode.simple_exp_PROD_1_SUBRULE_1_PROD_1_ACT (signed_term_RES, ADDOP_RES, signed_term_SPAN : (Lex.pos * Lex.pos), ADDOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun simple_exp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.ADDOP(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(simple_exp_PROD_1_SUBRULE_1_PRED, simple_exp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(signed_term_SPAN), #2(SR_SPAN))
      in
        (UserCode.simple_exp_PROD_1_ACT (SR_RES, signed_term_RES, SR_SPAN : (Lex.pos * Lex.pos), signed_term_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and signed_term_NT (strm) = let
      fun signed_term_PROD_1 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (term_RES, term_SPAN, strm') = term_NT(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(term_SPAN))
            in
              (UserCode.signed_term_PROD_1_ACT (term_RES, MINUS_RES, term_SPAN : (Lex.pos * Lex.pos), MINUS_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun signed_term_PROD_2 (strm) = let
            val (term_RES, term_SPAN, strm') = term_NT(strm)
            val FULL_SPAN = (#1(term_SPAN), #2(term_SPAN))
            in
              ((term_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_not, _, strm') => signed_term_PROD_2(strm)
          | (Tok.LP, _, strm') => signed_term_PROD_2(strm)
          | (Tok.ID(_), _, strm') => signed_term_PROD_2(strm)
          | (Tok.INT(_), _, strm') => signed_term_PROD_2(strm)
          | (Tok.REAL(_), _, strm') => signed_term_PROD_2(strm)
          | (Tok.MINUS, _, strm') => signed_term_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and term_NT (strm) = let
      val (factor_RES, factor_SPAN, strm') = factor_NT(strm)
      fun term_PROD_1_SUBRULE_1_NT (strm) = let
            val (MULOP_RES, MULOP_SPAN, strm') = matchMULOP(strm)
            val (factor_RES, factor_SPAN, strm') = factor_NT(strm')
            val FULL_SPAN = (#1(MULOP_SPAN), #2(factor_SPAN))
            in
              (UserCode.term_PROD_1_SUBRULE_1_PROD_1_ACT (factor_RES, MULOP_RES, factor_SPAN : (Lex.pos * Lex.pos), MULOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun term_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.MULOP(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(term_PROD_1_SUBRULE_1_PRED, term_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(factor_SPAN), #2(SR_SPAN))
      in
        (UserCode.term_PROD_1_ACT (SR_RES, factor_RES, SR_SPAN : (Lex.pos * Lex.pos), factor_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and factor_NT (strm) = let
      fun factor_PROD_1 (strm) = let
            val (variable_RES, variable_SPAN, strm') = variable_NT(strm)
            val FULL_SPAN = (#1(variable_SPAN), #2(variable_SPAN))
            in
              ((variable_RES), FULL_SPAN, strm')
            end
      fun factor_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            fun factor_PROD_2_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(exp_SPAN))
                  in
                    ((exp_RES), FULL_SPAN, strm')
                  end
            fun factor_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(factor_PROD_2_SUBRULE_1_PRED, factor_PROD_2_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(RP_SPAN))
            in
              (UserCode.factor_PROD_2_ACT (ID_RES, LP_RES, RP_RES, SR_RES, exp_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun factor_PROD_3 (strm) = let
            val (INT_RES, INT_SPAN, strm') = matchINT(strm)
            val FULL_SPAN = (#1(INT_SPAN), #2(INT_SPAN))
            in
              (UserCode.factor_PROD_3_ACT (INT_RES, INT_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun factor_PROD_4 (strm) = let
            val (REAL_RES, REAL_SPAN, strm') = matchREAL(strm)
            val FULL_SPAN = (#1(REAL_SPAN), #2(REAL_SPAN))
            in
              (UserCode.factor_PROD_4_ACT (REAL_RES, REAL_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun factor_PROD_5 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.factor_PROD_5_ACT (LP_RES, RP_RES, exp_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun factor_PROD_6 (strm) = let
            val (KW_not_RES, KW_not_SPAN, strm') = matchKW_not(strm)
            val (factor_RES, factor_SPAN, strm') = factor_NT(strm')
            val FULL_SPAN = (#1(KW_not_SPAN), #2(factor_SPAN))
            in
              (UserCode.factor_PROD_6_ACT (factor_RES, KW_not_RES, factor_SPAN : (Lex.pos * Lex.pos), KW_not_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_not, _, strm') => factor_PROD_6(strm)
          | (Tok.REAL(_), _, strm') => factor_PROD_4(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_end, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_then, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_else, _, strm') => factor_PROD_1(strm)
                | (Tok.KW_do, _, strm') => factor_PROD_1(strm)
                | (Tok.COMMA, _, strm') => factor_PROD_1(strm)
                | (Tok.SEMI, _, strm') => factor_PROD_1(strm)
                | (Tok.LSB, _, strm') => factor_PROD_1(strm)
                | (Tok.RSB, _, strm') => factor_PROD_1(strm)
                | (Tok.RP, _, strm') => factor_PROD_1(strm)
                | (Tok.RELOP(_), _, strm') => factor_PROD_1(strm)
                | (Tok.ADDOP(_), _, strm') => factor_PROD_1(strm)
                | (Tok.MULOP(_), _, strm') => factor_PROD_1(strm)
                | (Tok.LP, _, strm') => factor_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.INT(_), _, strm') => factor_PROD_3(strm)
          | (Tok.LP, _, strm') => factor_PROD_5(strm)
          | _ => fail()
        (* end case *))
      end
and variable_NT (strm) = let
      fun variable_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun variable_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (LSB_RES, LSB_SPAN, strm') = matchLSB(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (RSB_RES, RSB_SPAN, strm') = matchRSB(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(RSB_SPAN))
            in
              (UserCode.variable_PROD_2_ACT (ID_RES, LSB_RES, RSB_RES, exp_RES, ID_SPAN : (Lex.pos * Lex.pos), LSB_SPAN : (Lex.pos * Lex.pos), RSB_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_end, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_then, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_else, _, strm') => variable_PROD_1(strm)
                | (Tok.KW_do, _, strm') => variable_PROD_1(strm)
                | (Tok.ASSIGNOP, _, strm') => variable_PROD_1(strm)
                | (Tok.COMMA, _, strm') => variable_PROD_1(strm)
                | (Tok.SEMI, _, strm') => variable_PROD_1(strm)
                | (Tok.RSB, _, strm') => variable_PROD_1(strm)
                | (Tok.RP, _, strm') => variable_PROD_1(strm)
                | (Tok.RELOP(_), _, strm') => variable_PROD_1(strm)
                | (Tok.ADDOP(_), _, strm') => variable_PROD_1(strm)
                | (Tok.MULOP(_), _, strm') => variable_PROD_1(strm)
                | (Tok.LSB, _, strm') => variable_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun procedure_statement_NT (strm) = let
      fun procedure_statement_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun procedure_statement_PROD_2 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            val (LP_RES, LP_SPAN, strm') = matchLP(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            fun procedure_statement_PROD_2_SUBRULE_1_NT (strm) = let
                  val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
                  val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
                  val FULL_SPAN = (#1(COMMA_SPAN), #2(exp_SPAN))
                  in
                    ((exp_RES), FULL_SPAN, strm')
                  end
            fun procedure_statement_PROD_2_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.COMMA, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(procedure_statement_PROD_2_SUBRULE_1_PRED, procedure_statement_PROD_2_SUBRULE_1_NT, strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(ID_SPAN), #2(RP_SPAN))
            in
              (UserCode.procedure_statement_PROD_2_ACT (ID_RES, LP_RES, RP_RES, SR_RES, exp_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), exp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.KW_end, _, strm') => procedure_statement_PROD_1(strm)
                | (Tok.KW_else, _, strm') => procedure_statement_PROD_1(strm)
                | (Tok.SEMI, _, strm') => procedure_statement_PROD_1(strm)
                | (Tok.LP, _, strm') => procedure_statement_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | _ => fail()
        (* end case *))
      end
fun compound_statement_NT (strm) = let
      val (KW_begin_RES, KW_begin_SPAN, strm') = matchKW_begin(strm)
      fun compound_statement_PROD_1_SUBRULE_1_NT (strm) = let
            val (statement_RES, statement_SPAN, strm') = statement_NT(strm)
            fun compound_statement_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT (strm) = let
                  val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
                  val (statement_RES, statement_SPAN, strm') = statement_NT(strm')
                  val FULL_SPAN = (#1(SEMI_SPAN), #2(statement_SPAN))
                  in
                    ((statement_RES), FULL_SPAN, strm')
                  end
            fun compound_statement_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
                   of (Tok.SEMI, _, strm') => true
                    | _ => false
                  (* end case *))
            val (SR_RES, SR_SPAN, strm') = EBNF.closure(compound_statement_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_PRED, compound_statement_PROD_1_SUBRULE_1_PROD_1_SUBRULE_1_NT, strm')
            val FULL_SPAN = (#1(statement_SPAN), #2(SR_SPAN))
            in
              (UserCode.compound_statement_PROD_1_SUBRULE_1_PROD_1_ACT (SR_RES, KW_begin_RES, statement_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_begin_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun compound_statement_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_begin, _, strm') => true
              | (Tok.KW_if, _, strm') => true
              | (Tok.KW_while, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.optional(compound_statement_PROD_1_SUBRULE_1_PRED, compound_statement_PROD_1_SUBRULE_1_NT, strm')
      val (KW_end_RES, KW_end_SPAN, strm') = matchKW_end(strm')
      val FULL_SPAN = (#1(KW_begin_SPAN), #2(KW_end_SPAN))
      in
        (UserCode.compound_statement_PROD_1_ACT (SR_RES, KW_begin_RES, KW_end_RES, SR_SPAN : (Lex.pos * Lex.pos), KW_begin_SPAN : (Lex.pos * Lex.pos), KW_end_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
and statement_NT (strm) = let
      fun statement_PROD_1 (strm) = let
            val (variable_RES, variable_SPAN, strm') = variable_NT(strm)
            val (ASSIGNOP_RES, ASSIGNOP_SPAN, strm') = matchASSIGNOP(strm')
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val FULL_SPAN = (#1(variable_SPAN), #2(exp_SPAN))
            in
              (UserCode.statement_PROD_1_ACT (exp_RES, variable_RES, ASSIGNOP_RES, exp_SPAN : (Lex.pos * Lex.pos), variable_SPAN : (Lex.pos * Lex.pos), ASSIGNOP_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_2 (strm) = let
            val (procedure_statement_RES, procedure_statement_SPAN, strm') = procedure_statement_NT(strm)
            val FULL_SPAN = (#1(procedure_statement_SPAN),
              #2(procedure_statement_SPAN))
            in
              ((procedure_statement_RES), FULL_SPAN, strm')
            end
      fun statement_PROD_3 (strm) = let
            val (compound_statement_RES, compound_statement_SPAN, strm') = compound_statement_NT(strm)
            val FULL_SPAN = (#1(compound_statement_SPAN),
              #2(compound_statement_SPAN))
            in
              ((compound_statement_RES), FULL_SPAN, strm')
            end
      fun statement_PROD_4 (strm) = let
            val (KW_if_RES, KW_if_SPAN, strm') = matchKW_if(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (KW_then_RES, KW_then_SPAN, strm') = matchKW_then(strm')
            val (statement1_RES, statement1_SPAN, strm') = statement_NT(strm')
            val (KW_else_RES, KW_else_SPAN, strm') = matchKW_else(strm')
            val (statement2_RES, statement2_SPAN, strm') = statement_NT(strm')
            val FULL_SPAN = (#1(KW_if_SPAN), #2(statement2_SPAN))
            in
              (UserCode.statement_PROD_4_ACT (exp_RES, KW_else_RES, KW_then_RES, statement1_RES, statement2_RES, KW_if_RES, exp_SPAN : (Lex.pos * Lex.pos), KW_else_SPAN : (Lex.pos * Lex.pos), KW_then_SPAN : (Lex.pos * Lex.pos), statement1_SPAN : (Lex.pos * Lex.pos), statement2_SPAN : (Lex.pos * Lex.pos), KW_if_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun statement_PROD_5 (strm) = let
            val (KW_while_RES, KW_while_SPAN, strm') = matchKW_while(strm)
            val (exp_RES, exp_SPAN, strm') = exp_NT(strm')
            val (KW_do_RES, KW_do_SPAN, strm') = matchKW_do(strm')
            val (statement_RES, statement_SPAN, strm') = statement_NT(strm')
            val FULL_SPAN = (#1(KW_while_SPAN), #2(statement_SPAN))
            in
              (UserCode.statement_PROD_5_ACT (exp_RES, KW_while_RES, KW_do_RES, statement_RES, exp_SPAN : (Lex.pos * Lex.pos), KW_while_SPAN : (Lex.pos * Lex.pos), KW_do_SPAN : (Lex.pos * Lex.pos), statement_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_while, _, strm') => statement_PROD_5(strm)
          | (Tok.KW_begin, _, strm') => statement_PROD_3(strm)
          | (Tok.ID(_), _, strm') =>
              (case (lex(strm'))
               of (Tok.ASSIGNOP, _, strm') => statement_PROD_1(strm)
                | (Tok.LSB, _, strm') => statement_PROD_1(strm)
                | (Tok.KW_end, _, strm') => statement_PROD_2(strm)
                | (Tok.KW_else, _, strm') => statement_PROD_2(strm)
                | (Tok.SEMI, _, strm') => statement_PROD_2(strm)
                | (Tok.LP, _, strm') => statement_PROD_2(strm)
                | _ => fail()
              (* end case *))
          | (Tok.KW_if, _, strm') => statement_PROD_4(strm)
          | _ => fail()
        (* end case *))
      end
fun standard_type_NT (strm) = let
      fun standard_type_PROD_1 (strm) = let
            val (KW_integer_RES, KW_integer_SPAN, strm') = matchKW_integer(strm)
            val FULL_SPAN = (#1(KW_integer_SPAN), #2(KW_integer_SPAN))
            in
              (UserCode.standard_type_PROD_1_ACT (KW_integer_RES, KW_integer_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun standard_type_PROD_2 (strm) = let
            val (KW_real_RES, KW_real_SPAN, strm') = matchKW_real(strm)
            val FULL_SPAN = (#1(KW_real_SPAN), #2(KW_real_SPAN))
            in
              (UserCode.standard_type_PROD_2_ACT (KW_real_RES, KW_real_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_real, _, strm') => standard_type_PROD_2(strm)
          | (Tok.KW_integer, _, strm') => standard_type_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun compound_type_NT (strm) = let
      fun compound_type_PROD_1 (strm) = let
            val (standard_type_RES, standard_type_SPAN, strm') = standard_type_NT(strm)
            val FULL_SPAN = (#1(standard_type_SPAN), #2(standard_type_SPAN))
            in
              ((standard_type_RES), FULL_SPAN, strm')
            end
      fun compound_type_PROD_2 (strm) = let
            val (KW_array_RES, KW_array_SPAN, strm') = matchKW_array(strm)
            val (LSB_RES, LSB_SPAN, strm') = matchLSB(strm')
            val (INT1_RES, INT1_SPAN, strm') = matchINT(strm')
            val (DOT1_RES, DOT1_SPAN, strm') = matchDOT(strm')
            val (DOT2_RES, DOT2_SPAN, strm') = matchDOT(strm')
            val (INT2_RES, INT2_SPAN, strm') = matchINT(strm')
            val (RSB_RES, RSB_SPAN, strm') = matchRSB(strm')
            val (KW_of_RES, KW_of_SPAN, strm') = matchKW_of(strm')
            val (standard_type_RES, standard_type_SPAN, strm') = standard_type_NT(strm')
            val FULL_SPAN = (#1(KW_array_SPAN), #2(standard_type_SPAN))
            in
              (UserCode.compound_type_PROD_2_ACT (LSB_RES, RSB_RES, DOT1_RES, DOT2_RES, INT1_RES, INT2_RES, KW_array_RES, standard_type_RES, KW_of_RES, LSB_SPAN : (Lex.pos * Lex.pos), RSB_SPAN : (Lex.pos * Lex.pos), DOT1_SPAN : (Lex.pos * Lex.pos), DOT2_SPAN : (Lex.pos * Lex.pos), INT1_SPAN : (Lex.pos * Lex.pos), INT2_SPAN : (Lex.pos * Lex.pos), KW_array_SPAN : (Lex.pos * Lex.pos), standard_type_SPAN : (Lex.pos * Lex.pos), KW_of_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_array, _, strm') => compound_type_PROD_2(strm)
          | (Tok.KW_integer, _, strm') => compound_type_PROD_1(strm)
          | (Tok.KW_real, _, strm') => compound_type_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun id_list_NT (strm) = let
      val (ID_RES, ID_SPAN, strm') = matchID(strm)
      fun id_list_PROD_1_SUBRULE_1_NT (strm) = let
            val (COMMA_RES, COMMA_SPAN, strm') = matchCOMMA(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val FULL_SPAN = (#1(COMMA_SPAN), #2(ID_SPAN))
            in
              ((ID_RES), FULL_SPAN, strm')
            end
      fun id_list_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.COMMA, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(id_list_PROD_1_SUBRULE_1_PRED, id_list_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(ID_SPAN), #2(SR_SPAN))
      in
        (UserCode.id_list_PROD_1_ACT (ID_RES, SR_RES, ID_SPAN : (Lex.pos * Lex.pos), SR_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun id_list_type_NT (strm) = let
      val (id_list_RES, id_list_SPAN, strm') = id_list_NT(strm)
      val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
      val (compound_type_RES, compound_type_SPAN, strm') = compound_type_NT(strm')
      val FULL_SPAN = (#1(id_list_SPAN), #2(compound_type_SPAN))
      in
        (UserCode.id_list_type_PROD_1_ACT (compound_type_RES, COLON_RES, id_list_RES, compound_type_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), id_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun declaration_NT (strm) = let
      val (KW_var_RES, KW_var_SPAN, strm') = matchKW_var(strm)
      val (id_list_type_RES, id_list_type_SPAN, strm') = id_list_type_NT(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(KW_var_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.declaration_PROD_1_ACT (SEMI_RES, id_list_type_RES, KW_var_RES, SEMI_SPAN : (Lex.pos * Lex.pos), id_list_type_SPAN : (Lex.pos * Lex.pos), KW_var_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun parameter_list_NT (strm) = let
      val (id_list_type_RES, id_list_type_SPAN, strm') = id_list_type_NT(strm)
      fun parameter_list_PROD_1_SUBRULE_1_NT (strm) = let
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm)
            val (id_list_type_RES, id_list_type_SPAN, strm') = id_list_type_NT(strm')
            val FULL_SPAN = (#1(SEMI_SPAN), #2(id_list_type_SPAN))
            in
              ((id_list_type_RES), FULL_SPAN, strm')
            end
      fun parameter_list_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.SEMI, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(parameter_list_PROD_1_SUBRULE_1_PRED, parameter_list_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(id_list_type_SPAN), #2(SR_SPAN))
      in
        (UserCode.parameter_list_PROD_1_ACT (SR_RES, id_list_type_RES, SR_SPAN : (Lex.pos * Lex.pos), id_list_type_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun arguments_NT (strm) = let
      fun arguments_PROD_1 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (parameter_list_RES, parameter_list_SPAN, strm') = parameter_list_NT(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              (UserCode.arguments_PROD_1_ACT (LP_RES, RP_RES, parameter_list_RES, LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), parameter_list_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun arguments_PROD_2 (strm) = let
            val FULL_SPAN = (Err.getPos(strm), Err.getPos(strm))
            in
              (UserCode.arguments_PROD_2_ACT (FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm)
            end
      in
        (case (lex(strm))
         of (Tok.COLON, _, strm') => arguments_PROD_2(strm)
          | (Tok.SEMI, _, strm') => arguments_PROD_2(strm)
          | (Tok.LP, _, strm') => arguments_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun subprogram_head_NT (strm) = let
      fun subprogram_head_PROD_1 (strm) = let
            val (KW_function_RES, KW_function_SPAN, strm') = matchKW_function(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (arguments_RES, arguments_SPAN, strm') = arguments_NT(strm')
            val (COLON_RES, COLON_SPAN, strm') = matchCOLON(strm')
            val (standard_type_RES, standard_type_SPAN, strm') = standard_type_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_function_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.subprogram_head_PROD_1_ACT (ID_RES, SEMI_RES, standard_type_RES, COLON_RES, arguments_RES, KW_function_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), standard_type_SPAN : (Lex.pos * Lex.pos), COLON_SPAN : (Lex.pos * Lex.pos), arguments_SPAN : (Lex.pos * Lex.pos), KW_function_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      fun subprogram_head_PROD_2 (strm) = let
            val (KW_procedure_RES, KW_procedure_SPAN, strm') = matchKW_procedure(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (arguments_RES, arguments_SPAN, strm') = arguments_NT(strm')
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(KW_procedure_SPAN), #2(SEMI_SPAN))
            in
              (UserCode.subprogram_head_PROD_2_ACT (ID_RES, SEMI_RES, KW_procedure_RES, arguments_RES, ID_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), KW_procedure_SPAN : (Lex.pos * Lex.pos), arguments_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.KW_procedure, _, strm') => subprogram_head_PROD_2(strm)
          | (Tok.KW_function, _, strm') => subprogram_head_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
fun subprogram_declaration_NT (strm) = let
      val (subprogram_head_RES, subprogram_head_SPAN, strm') = subprogram_head_NT(strm)
      fun subprogram_declaration_PROD_1_SUBRULE_1_NT (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = declaration_NT(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              ((declaration_RES), FULL_SPAN, strm')
            end
      fun subprogram_declaration_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_var, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(subprogram_declaration_PROD_1_SUBRULE_1_PRED, subprogram_declaration_PROD_1_SUBRULE_1_NT, strm')
      val (compound_statement_RES, compound_statement_SPAN, strm') = compound_statement_NT(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      val FULL_SPAN = (#1(subprogram_head_SPAN), #2(SEMI_SPAN))
      in
        (UserCode.subprogram_declaration_PROD_1_ACT (SR_RES, SEMI_RES, compound_statement_RES, subprogram_head_RES, SR_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), compound_statement_SPAN : (Lex.pos * Lex.pos), subprogram_head_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
fun program_NT (strm) = let
      val (KW_program_RES, KW_program_SPAN, strm') = matchKW_program(strm)
      val (ID_RES, ID_SPAN, strm') = matchID(strm')
      val (LP_RES, LP_SPAN, strm') = matchLP(strm')
      val (id_list_RES, id_list_SPAN, strm') = id_list_NT(strm')
      val (RP_RES, RP_SPAN, strm') = matchRP(strm')
      val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
      fun program_PROD_1_SUBRULE_1_NT (strm) = let
            val (declaration_RES, declaration_SPAN, strm') = declaration_NT(strm)
            val FULL_SPAN = (#1(declaration_SPAN), #2(declaration_SPAN))
            in
              ((declaration_RES), FULL_SPAN, strm')
            end
      fun program_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_var, _, strm') => true
              | _ => false
            (* end case *))
      val (SR1_RES, SR1_SPAN, strm') = EBNF.closure(program_PROD_1_SUBRULE_1_PRED, program_PROD_1_SUBRULE_1_NT, strm')
      fun program_PROD_1_SUBRULE_2_NT (strm) = let
            val (subprogram_declaration_RES, subprogram_declaration_SPAN, strm') = subprogram_declaration_NT(strm)
            val FULL_SPAN = (#1(subprogram_declaration_SPAN),
              #2(subprogram_declaration_SPAN))
            in
              ((subprogram_declaration_RES), FULL_SPAN, strm')
            end
      fun program_PROD_1_SUBRULE_2_PRED (strm) = (case (lex(strm))
             of (Tok.KW_function, _, strm') => true
              | (Tok.KW_procedure, _, strm') => true
              | _ => false
            (* end case *))
      val (SR2_RES, SR2_SPAN, strm') = EBNF.closure(program_PROD_1_SUBRULE_2_PRED, program_PROD_1_SUBRULE_2_NT, strm')
      val (compound_statement_RES, compound_statement_SPAN, strm') = compound_statement_NT(strm')
      val (DOT_RES, DOT_SPAN, strm') = matchDOT(strm')
      val FULL_SPAN = (#1(KW_program_SPAN), #2(DOT_SPAN))
      in
        (UserCode.program_PROD_1_ACT (ID_RES, LP_RES, RP_RES, DOT_RES, SR1_RES, SR2_RES, SEMI_RES, compound_statement_RES, id_list_RES, KW_program_RES, ID_SPAN : (Lex.pos * Lex.pos), LP_SPAN : (Lex.pos * Lex.pos), RP_SPAN : (Lex.pos * Lex.pos), DOT_SPAN : (Lex.pos * Lex.pos), SR1_SPAN : (Lex.pos * Lex.pos), SR2_SPAN : (Lex.pos * Lex.pos), SEMI_SPAN : (Lex.pos * Lex.pos), compound_statement_SPAN : (Lex.pos * Lex.pos), id_list_SPAN : (Lex.pos * Lex.pos), KW_program_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos)),
          FULL_SPAN, strm')
      end
in
  (program_NT)
end
val program_NT =  fn s => unwrap (Err.launch (eh, lexFn, program_NT , true) s)

in (program_NT) end
  in
fun parse lexFn  s = let val (program_NT) = mk lexFn in program_NT s end

  end

end
