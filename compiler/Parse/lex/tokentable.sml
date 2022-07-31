(* tokentable.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(***************************************************************************

  TOKEN.SML: hash table for token recognition

 ***************************************************************************)

signature SMLNJ_TOKENS =
  sig
    type ('a,'b) token
    type svalue
    val FUNSIG : 'a * 'a -> (svalue,'a) token
    val ANDALSO : 'a * 'a -> (svalue,'a) token
    val ORELSE : 'a * 'a -> (svalue,'a) token
    val COLONGT : 'a * 'a -> (svalue,'a) token
    val COLON : 'a * 'a -> (svalue,'a) token
    val ASTERISK : 'a * 'a -> (svalue,'a) token
    val WITHTYPE : 'a * 'a -> (svalue,'a) token
    val WITH : 'a * 'a -> (svalue,'a) token
    val WHILE : 'a * 'a -> (svalue,'a) token
    val WHERE : 'a * 'a -> (svalue,'a) token
    val VAL : 'a * 'a -> (svalue,'a) token
    val TYPE : 'a * 'a -> (svalue,'a) token
    val THEN : 'a * 'a -> (svalue,'a) token
    val STRUCTURE : 'a * 'a -> (svalue,'a) token
    val STRUCT : 'a * 'a -> (svalue,'a) token
    val SIGNATURE : 'a * 'a -> (svalue,'a) token
    val SIG : 'a * 'a -> (svalue,'a) token
    val SHARING : 'a * 'a -> (svalue,'a) token
    val REC : 'a * 'a -> (svalue,'a) token
    val RAISE : 'a * 'a -> (svalue,'a) token
    val OVERLOAD : 'a * 'a -> (svalue,'a) token
    val OPEN : 'a * 'a -> (svalue,'a) token
    val OP : 'a * 'a -> (svalue,'a) token
    val OF : 'a * 'a -> (svalue,'a) token
    val NONFIX : 'a * 'a -> (svalue,'a) token
    val LOCAL : 'a * 'a -> (svalue,'a) token
    val LET : 'a * 'a -> (svalue,'a) token
    val LAZY : 'a * 'a -> (svalue,'a) token
    val INFIXR : 'a * 'a -> (svalue,'a) token
    val INFIX : 'a * 'a -> (svalue,'a) token
    val INCLUDE : 'a * 'a -> (svalue,'a) token
    val IN : 'a * 'a -> (svalue,'a) token
    val IF : 'a * 'a -> (svalue,'a) token
    val HASH : 'a * 'a -> (svalue,'a) token
    val HANDLE : 'a * 'a -> (svalue,'a) token
    val FUNCTOR : 'a * 'a -> (svalue,'a) token
    val FUN : 'a * 'a -> (svalue,'a) token
    val FN : 'a * 'a -> (svalue,'a) token
    val DARROW : 'a * 'a -> (svalue,'a) token
    val DO : 'a * 'a -> (svalue,'a) token
    val EXCEPTION : 'a * 'a -> (svalue,'a) token
    val EQTYPE : 'a * 'a -> (svalue,'a) token
    val EQUALOP : 'a * 'a -> (svalue,'a) token
    val END : 'a * 'a -> (svalue,'a) token
    val ELSE : 'a * 'a -> (svalue,'a) token
    val DATATYPE : 'a * 'a -> (svalue,'a) token
    val CASE : 'a * 'a -> (svalue,'a) token
    val BAR : 'a * 'a -> (svalue,'a) token
    val AS : 'a * 'a -> (svalue,'a) token
    val ARROW : 'a * 'a -> (svalue,'a) token
    val AND : 'a * 'a -> (svalue,'a) token
    val ABSTYPE : 'a * 'a -> (svalue,'a) token
    val TYVAR : FastSymbol.raw_symbol *  'a * 'a -> (svalue, 'a) token
    val IDA : FastSymbol.raw_symbol *  'a * 'a -> (svalue, 'a) token
    val IDS : FastSymbol.raw_symbol *  'a * 'a -> (svalue, 'a) token
  end

functor TokenTable (Tokens : SMLNJ_TOKENS) : sig

    val checkId : (string * int) -> (Tokens.svalue,int) Tokens.token
    val checkSymId : (string * int) -> (Tokens.svalue,int) Tokens.token
    val makeTyvar : (string * int) -> (Tokens.svalue,int) Tokens.token

  end = struct

    exception NotToken

    structure Tbl = WordStringHashTable

    val hashStr = HashString.hashString

    fun mkTable (sz, l) = let
	val t = Tbl.mkTable (sz, NotToken)
	fun ins (str, tokfn) =
	    Tbl.insert t ((hashStr str, str), tokfn)
    in
	List.app ins l;
	t
    end

    (* symIdTbl: table of reserved symbolic identifiers *)
    val symIdTbl = mkTable (16, [
	    ("*"	, fn yypos => Tokens.ASTERISK(yypos,yypos+1)),
	    ("|"	, fn yypos => Tokens.BAR(yypos,yypos+1)),
	    (":"	, fn yypos => Tokens.COLON(yypos,yypos+1)),
	    (":>"	, fn yypos => Tokens.COLONGT(yypos,yypos+1)),
	    ("="	, fn yypos => Tokens.EQUALOP(yypos,yypos+1)),
	    ("#"	, fn yypos => Tokens.HASH(yypos,yypos+1)),
	    ("->"	, fn yypos => Tokens.ARROW(yypos,yypos+2)),
	    ("=>"	, fn yypos => Tokens.DARROW(yypos,yypos+2))
	  ])

    (* idTbl: table of alphanumeric reserved identifiers *)
    val idTbl = mkTable (64, [
	    ("and"	, fn yypos => Tokens.AND(yypos,yypos+3)),
	    ("abstype"	, fn yypos => Tokens.ABSTYPE(yypos,yypos+7)),
	    ("as"	, fn yypos => Tokens.AS(yypos,yypos+2)),
	    ("case"	, fn yypos => Tokens.CASE(yypos,yypos+4)),
	    ("datatype"	, fn yypos => Tokens.DATATYPE(yypos,yypos+8)),
	    ("else"	, fn yypos => Tokens.ELSE(yypos,yypos+4)),
	    ("end"	, fn yypos => Tokens.END(yypos,yypos+3)),
	    ("eqtype"	, fn yypos => Tokens.EQTYPE(yypos,yypos+6)),
	    ("exception", fn yypos => Tokens.EXCEPTION(yypos,yypos+9)),
	    ("do"	, fn yypos => Tokens.DO(yypos,yypos+2)),
	    ("fn"	, fn yypos => Tokens.FN(yypos,yypos+2)),
	    ("fun"	, fn yypos => Tokens.FUN(yypos,yypos+3)),
	    ("functor"	, fn yypos => Tokens.FUNCTOR(yypos,yypos+7)),
	    ("funsig"	, fn yypos => Tokens.FUNSIG(yypos,yypos+7)),
	    ("handle"	, fn yypos => Tokens.HANDLE(yypos,yypos+6)),
	    ("if"	, fn yypos => Tokens.IF(yypos,yypos+2)),
	    ("in"	, fn yypos => Tokens.IN(yypos,yypos+2)),
	    ("include"	, fn yypos => Tokens.INCLUDE(yypos,yypos+7)),
	    ("infix"	, fn yypos => Tokens.INFIX(yypos,yypos+5)),
	    ("infixr"	, fn yypos => Tokens.INFIXR(yypos,yypos+6)),
	    ("lazy"	, fn yypos =>
			     if !ParserControl.lazysml then
				 Tokens.LAZY(yypos,yypos+4)
			     else raise NotToken),
	    ("let"	, fn yypos => Tokens.LET(yypos,yypos+3)),
	    ("local"	, fn yypos => Tokens.LOCAL(yypos,yypos+5)),
	    ("nonfix"	, fn yypos => Tokens.NONFIX(yypos,yypos+6)),
	    ("of"	, fn yypos => Tokens.OF(yypos,yypos+2)),
	    ("op"	, fn yypos => Tokens.OP(yypos,yypos+2)),
	    ("open"	, fn yypos => Tokens.OPEN(yypos,yypos+4)),
	    ("overload"	, fn yypos =>
			     if !ParserControl.overloadKW then
				 Tokens.OVERLOAD(yypos,yypos+8)
			     else raise NotToken),
	    ("raise"	, fn yypos => Tokens.RAISE(yypos,yypos+5)),
	    ("rec"	, fn yypos => Tokens.REC(yypos,yypos+3)),
	    ("sharing"	, fn yypos => Tokens.SHARING(yypos,yypos+7)),
	    ("sig"	, fn yypos => Tokens.SIG(yypos,yypos+3)),
	    ("signature", fn yypos => Tokens.SIGNATURE(yypos,yypos+9)),
	    ("struct"	, fn yypos => Tokens.STRUCT(yypos,yypos+6)),
	    ("structure", fn yypos => Tokens.STRUCTURE(yypos,yypos+9)),
	    ("then"	, fn yypos => Tokens.THEN(yypos,yypos+4)),
	    ("type"	, fn yypos => Tokens.TYPE(yypos,yypos+4)),
	    ("val"	, fn yypos => Tokens.VAL(yypos,yypos+3)),
	    ("where"	, fn yypos => Tokens.WHERE(yypos,yypos+5)),
	    ("while"	, fn yypos => Tokens.WHILE(yypos,yypos+5)),
	    ("with"	, fn yypos => Tokens.WITH(yypos,yypos+4)),
	    ("withtype"	, fn yypos => Tokens.WITHTYPE(yypos,yypos+8)),
	    ("orelse"	, fn yypos => Tokens.ORELSE(yypos,yypos+6)),
	    ("andalso"	, fn yypos => Tokens.ANDALSO(yypos,yypos+7))
	  ])

    val overloadHash = hashStr "overload"
    val lazyHash = hashStr "lazy"

  (* checkID: look-up an alphanumeric string in idTbl. If it is found, the corresponding
   * reserved word token is generated, with the identifier's position info. Otherwise it is a regular
   * identifier, so generate an IDA (alphanumeric id) token with the same position info.
   *)
    fun checkId (str, yypos) = let
	  val hash = hashStr str
	  in
	    Tbl.lookup idTbl (hash, str) yypos
	      handle NotToken =>
		     Tokens.IDA(FastSymbol.rawSymbol(hash,str), yypos, yypos+size(str))
    end

  (* checkSymId: look-up a symbolic string in idSymTbl. If it is found, the corresponding
   * reserved symbol token is generated, with the identifier's position info. Otherwise it is a regular
   * symbolic identifier, so generate an IDS (symbolic identifier) token with the same position info.
   *)
    fun checkSymId (str, yypos) = let
	  val hash = hashStr str
	  in
	    Tbl.lookup symIdTbl (hash, str) yypos
	      handle NotToken =>
		     Tokens.IDS(FastSymbol.rawSymbol(hash,str), yypos, yypos+size(str))
	  end

  (* makeTyvar: convert a tyvar string into a TYVAR token *)
    fun makeTyvar (str, yypos) =
        Tokens.TYVAR (FastSymbol.rawSymbol(hashStr str,str),yypos,yypos+size(str))

  end
