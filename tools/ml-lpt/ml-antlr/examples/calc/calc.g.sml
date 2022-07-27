structure 
CalcParseTokens = struct

    datatype token = EOF
      | DummyExp of int
      | SEMI
      | RP
      | LP
      | MINUS
      | TIMES
      | PLUS
      | EQ
      | NUM of Int.int
      | ID of string
      | KW_in
      | KW_let

    val allToks = [EOF, SEMI, RP, LP, MINUS, TIMES, PLUS, EQ, KW_in, KW_let]

    fun toString tok =
(case (tok)
 of (EOF) => "EOF"
  | (DummyExp(_)) => "DummyExp"
  | (SEMI) => ";"
  | (RP) => ")"
  | (LP) => "("
  | (MINUS) => "-"
  | (TIMES) => "*"
  | (PLUS) => "+"
  | (EQ) => "="
  | (NUM(_)) => "NUM"
  | (ID(_)) => "ID"
  | (KW_in) => "in"
  | (KW_let) => "let"
(* end case *))
    fun isKW tok =
(case (tok)
 of (EOF) => false
  | (DummyExp(_)) => false
  | (SEMI) => false
  | (RP) => false
  | (LP) => false
  | (MINUS) => false
  | (TIMES) => false
  | (PLUS) => false
  | (EQ) => false
  | (NUM(_)) => false
  | (ID(_)) => false
  | (KW_in) => false
  | (KW_let) => false
(* end case *))
    val changes = []


  fun isEOF EOF = true
    | isEOF _ = false

end

functor CalcParseFn (Lex : ANTLR_LEXER) = struct

  local
    structure Tok = 
CalcParseTokens
    structure UserCode =
      struct

fun exp_PROD_1_ACT (EQ, ID, env, exp1, exp2, KW_in, KW_let, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums, vars) = 
  ( vars := ID::(!vars); exp2 )
fun addExp_PROD_1_ACT (SR, env, multExp, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums, vars) = 
  ( List.foldl op+ multExp SR )
fun multExp_PROD_1_ACT (SR, env, prefixExp, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums, vars) = 
  ( List.foldl op* prefixExp SR )
fun prefixExp_PROD_2_ACT (env, MINUS, prefixExp, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums, vars) = 
  ( ~prefixExp )
fun atomicExp_PROD_1_ACT (ID, env, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums, vars) = 
  ( valOf(AtomMap.find (env, Atom.atom ID)) )
fun atomicExp_PROD_2_ACT (NUM, env, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums, vars) = 
  ( nums := NUM::(!nums); NUM )
fun atomicExp_PROD_1_PRED (ID, env, nums, vars) = 
  ( AtomMap.inDomain (env, Atom.atom ID) )
fun ARGS_4 (nums, vars) = 
  (AtomMap.empty)
fun ARGS_6 (EQ, ID, env, KW_let, nums, vars) = 
  (env)
fun ARGS_7 (EQ, ID, env, exp1, KW_in, KW_let, nums, vars) = 
  (AtomMap.insert(env, Atom.atom ID, exp1))
fun ARGS_8 (env, nums, vars) = 
  (env)
fun ARGS_11 (env, PLUS, multExp, nums, vars) = 
  (env)
fun ARGS_10 (env, nums, vars) = 
  (env)
fun ARGS_14 (env, TIMES, prefixExp, nums, vars) = 
  (env)
fun ARGS_13 (env, nums, vars) = 
  (env)
fun ARGS_15 (env, nums, vars) = 
  (env)
fun ARGS_17 (env, MINUS, nums, vars) = 
  (env)
fun ARGS_21 (LP, env, nums, vars) = 
  (env)
fun mknums_REFC() : (int list) ref = ref ([])
fun mkvars_REFC() : (string list) ref = ref ([])
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
val nums_REFC = UserCode.mknums_REFC()
val vars_REFC = UserCode.mkvars_REFC()
fun getS() = {nums = !nums_REFC, vars = !vars_REFC}
fun putS{nums, vars} = (nums_REFC := nums; vars_REFC := vars)
fun unwrap (ret, strm, repairs) = (ret, strm, repairs, getS())        val (eh, lex) = Err.mkErrHandler {get = getS, put = putS}
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
fun matchDummyExp strm = (case (lex(strm))
 of (Tok.DummyExp(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchSEMI strm = (case (lex(strm))
 of (Tok.SEMI, span, strm') => ((), span, strm')
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
fun matchMINUS strm = (case (lex(strm))
 of (Tok.MINUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchTIMES strm = (case (lex(strm))
 of (Tok.TIMES, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchPLUS strm = (case (lex(strm))
 of (Tok.PLUS, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchEQ strm = (case (lex(strm))
 of (Tok.EQ, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchNUM strm = (case (lex(strm))
 of (Tok.NUM(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchID strm = (case (lex(strm))
 of (Tok.ID(x), span, strm') => (x, span, strm')
  | _ => fail()
(* end case *))
fun matchKW_in strm = (case (lex(strm))
 of (Tok.KW_in, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))
fun matchKW_let strm = (case (lex(strm))
 of (Tok.KW_let, span, strm') => ((), span, strm')
  | _ => fail()
(* end case *))

val (prog_NT, exp_NT) = 
let
fun exp_NT (env_RES) (strm) = let
      fun exp_PROD_1 (strm) = let
            val (KW_let_RES, KW_let_SPAN, strm') = matchKW_let(strm)
            val (ID_RES, ID_SPAN, strm') = matchID(strm')
            val (EQ_RES, EQ_SPAN, strm') = matchEQ(strm')
            val (exp1_RES, exp1_SPAN, strm') = (exp_NT (UserCode.ARGS_6 (EQ_RES, ID_RES, env_RES, KW_let_RES, nums_REFC, vars_REFC)))(strm')
            val (KW_in_RES, KW_in_SPAN, strm') = matchKW_in(strm')
            val (exp2_RES, exp2_SPAN, strm') = (exp_NT (UserCode.ARGS_7 (EQ_RES, ID_RES, env_RES, exp1_RES, KW_in_RES, KW_let_RES, nums_REFC, vars_REFC)))(strm')
            val FULL_SPAN = (#1(KW_let_SPAN), #2(exp2_SPAN))
            in
              (UserCode.exp_PROD_1_ACT (EQ_RES, ID_RES, env_RES, exp1_RES, exp2_RES, KW_in_RES, KW_let_RES, EQ_SPAN : (Lex.pos * Lex.pos), ID_SPAN : (Lex.pos * Lex.pos), exp1_SPAN : (Lex.pos * Lex.pos), exp2_SPAN : (Lex.pos * Lex.pos), KW_in_SPAN : (Lex.pos * Lex.pos), KW_let_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun exp_PROD_2 (strm) = let
            val (addExp_RES, addExp_SPAN, strm') = (addExp_NT (UserCode.ARGS_8 (env_RES, nums_REFC, vars_REFC)))(strm)
            val FULL_SPAN = (#1(addExp_SPAN), #2(addExp_SPAN))
            in
              ((addExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.ID(_), _, strm') => exp_PROD_2(strm)
          | (Tok.NUM(_), _, strm') => exp_PROD_2(strm)
          | (Tok.MINUS, _, strm') => exp_PROD_2(strm)
          | (Tok.LP, _, strm') => exp_PROD_2(strm)
          | (Tok.DummyExp(_), _, strm') => exp_PROD_2(strm)
          | (Tok.KW_let, _, strm') => exp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and addExp_NT (env_RES) (strm) = let
      val (multExp_RES, multExp_SPAN, strm') = (multExp_NT (UserCode.ARGS_10 (env_RES, nums_REFC, vars_REFC)))(strm)
      fun addExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (PLUS_RES, PLUS_SPAN, strm') = matchPLUS(strm)
            val (multExp_RES, multExp_SPAN, strm') = (multExp_NT (UserCode.ARGS_11 (env_RES, PLUS_RES, multExp_RES, nums_REFC, vars_REFC)))(strm')
            val FULL_SPAN = (#1(PLUS_SPAN), #2(multExp_SPAN))
            in
              ((multExp_RES), FULL_SPAN, strm')
            end
      fun addExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.PLUS, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(addExp_PROD_1_SUBRULE_1_PRED, addExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(multExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.addExp_PROD_1_ACT (SR_RES, env_RES, multExp_RES, SR_SPAN : (Lex.pos * Lex.pos), multExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
and multExp_NT (env_RES) (strm) = let
      val (prefixExp_RES, prefixExp_SPAN, strm') = (prefixExp_NT (UserCode.ARGS_13 (env_RES, nums_REFC, vars_REFC)))(strm)
      fun multExp_PROD_1_SUBRULE_1_NT (strm) = let
            val (TIMES_RES, TIMES_SPAN, strm') = matchTIMES(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (prefixExp_NT (UserCode.ARGS_14 (env_RES, TIMES_RES, prefixExp_RES, nums_REFC, vars_REFC)))(strm')
            val FULL_SPAN = (#1(TIMES_SPAN), #2(prefixExp_SPAN))
            in
              ((prefixExp_RES), FULL_SPAN, strm')
            end
      fun multExp_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.TIMES, _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(multExp_PROD_1_SUBRULE_1_PRED, multExp_PROD_1_SUBRULE_1_NT, strm')
      val FULL_SPAN = (#1(prefixExp_SPAN), #2(SR_SPAN))
      in
        (UserCode.multExp_PROD_1_ACT (SR_RES, env_RES, prefixExp_RES, SR_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums_REFC, vars_REFC),
          FULL_SPAN, strm')
      end
and prefixExp_NT (env_RES) (strm) = let
      fun prefixExp_PROD_1 (strm) = let
            val (atomicExp_RES, atomicExp_SPAN, strm') = (atomicExp_NT (UserCode.ARGS_15 (env_RES, nums_REFC, vars_REFC)))(strm)
            val FULL_SPAN = (#1(atomicExp_SPAN), #2(atomicExp_SPAN))
            in
              ((atomicExp_RES), FULL_SPAN, strm')
            end
      fun prefixExp_PROD_2 (strm) = let
            val (MINUS_RES, MINUS_SPAN, strm') = matchMINUS(strm)
            val (prefixExp_RES, prefixExp_SPAN, strm') = (prefixExp_NT (UserCode.ARGS_17 (env_RES, MINUS_RES, nums_REFC, vars_REFC)))(strm')
            val FULL_SPAN = (#1(MINUS_SPAN), #2(prefixExp_SPAN))
            in
              (UserCode.prefixExp_PROD_2_ACT (env_RES, MINUS_RES, prefixExp_RES, MINUS_SPAN : (Lex.pos * Lex.pos), prefixExp_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.MINUS, _, strm') => prefixExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.NUM(_), _, strm') => prefixExp_PROD_1(strm)
          | (Tok.LP, _, strm') => prefixExp_PROD_1(strm)
          | (Tok.DummyExp(_), _, strm') => prefixExp_PROD_1(strm)
          | _ => fail()
        (* end case *))
      end
and atomicExp_NT (env_RES) (strm) = let
      fun atomicExp_PROD_1 (strm) = let
            val (ID_RES, ID_SPAN, strm') = matchID(strm)
            in
              if (UserCode.atomicExp_PROD_1_PRED (ID_RES, env_RES, nums_REFC, vars_REFC))
                then let
                  val FULL_SPAN = (#1(ID_SPAN), #2(ID_SPAN))
                  in
                    (UserCode.atomicExp_PROD_1_ACT (ID_RES, env_RES, ID_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums_REFC, vars_REFC),
                      FULL_SPAN, strm')
                  end
                else fail()
            end
      fun atomicExp_PROD_2 (strm) = let
            val (NUM_RES, NUM_SPAN, strm') = matchNUM(strm)
            val FULL_SPAN = (#1(NUM_SPAN), #2(NUM_SPAN))
            in
              (UserCode.atomicExp_PROD_2_ACT (NUM_RES, env_RES, NUM_SPAN : (Lex.pos * Lex.pos), FULL_SPAN : (Lex.pos * Lex.pos), nums_REFC, vars_REFC),
                FULL_SPAN, strm')
            end
      fun atomicExp_PROD_3 (strm) = let
            val (LP_RES, LP_SPAN, strm') = matchLP(strm)
            val (exp_RES, exp_SPAN, strm') = (exp_NT (UserCode.ARGS_21 (LP_RES, env_RES, nums_REFC, vars_REFC)))(strm')
            val (RP_RES, RP_SPAN, strm') = matchRP(strm')
            val FULL_SPAN = (#1(LP_SPAN), #2(RP_SPAN))
            in
              ((exp_RES), FULL_SPAN, strm')
            end
      fun atomicExp_PROD_4 (strm) = let
            val (DummyExp_RES, DummyExp_SPAN, strm') = matchDummyExp(strm)
            val FULL_SPAN = (#1(DummyExp_SPAN), #2(DummyExp_SPAN))
            in
              ((DummyExp_RES), FULL_SPAN, strm')
            end
      in
        (case (lex(strm))
         of (Tok.DummyExp(_), _, strm') => atomicExp_PROD_4(strm)
          | (Tok.NUM(_), _, strm') => atomicExp_PROD_2(strm)
          | (Tok.ID(_), _, strm') => atomicExp_PROD_1(strm)
          | (Tok.LP, _, strm') => atomicExp_PROD_3(strm)
          | _ => fail()
        (* end case *))
      end
fun prog_NT (strm) = let
      fun prog_PROD_1_SUBRULE_1_NT (strm) = let
            val (exp_RES, exp_SPAN, strm') = (exp_NT (UserCode.ARGS_4 (nums_REFC, vars_REFC)))(strm)
            val (SEMI_RES, SEMI_SPAN, strm') = matchSEMI(strm')
            val FULL_SPAN = (#1(exp_SPAN), #2(SEMI_SPAN))
            in
              ((exp_RES), FULL_SPAN, strm')
            end
      fun prog_PROD_1_SUBRULE_1_PRED (strm) = (case (lex(strm))
             of (Tok.KW_let, _, strm') => true
              | (Tok.ID(_), _, strm') => true
              | (Tok.NUM(_), _, strm') => true
              | (Tok.MINUS, _, strm') => true
              | (Tok.LP, _, strm') => true
              | (Tok.DummyExp(_), _, strm') => true
              | _ => false
            (* end case *))
      val (SR_RES, SR_SPAN, strm') = EBNF.closure(prog_PROD_1_SUBRULE_1_PRED, prog_PROD_1_SUBRULE_1_NT, strm)
      val FULL_SPAN = (#1(SR_SPAN), #2(SR_SPAN))
      in
        ((SR_RES), FULL_SPAN, strm')
      end
in
  (prog_NT, exp_NT)
end
val prog_NT =  fn s => unwrap (Err.launch (eh, lexFn, prog_NT , true) s)
val exp_NT =  fn x => fn s => unwrap (Err.launch (eh, lexFn, exp_NT x , false) s)

in (prog_NT, exp_NT) end
  in
fun parse lexFn  s = let val (prog_NT, exp_NT) = mk lexFn in prog_NT s end

fun parseexp lexFn  x s = let val (prog_NT, exp_NT) = mk lexFn in exp_NT x s end

  end

end
