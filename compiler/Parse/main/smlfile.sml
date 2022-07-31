(* smlfile.sml
 *
 * COPYRIGHT (c) 2016 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature SMLFILE =
  sig
    val parseOne : Source.inputSource -> unit -> Ast.dec option
    val parse : Source.inputSource -> Ast.dec
  end

structure SmlFile :> SMLFILE = struct

    structure R = ParseResult

    val parsePhase = Stats.makePhase "Compiler 010 parse"

    fun fail s = raise (CompileExn.Compile s)

    fun parser source = if !ParserControl.succML
	  then SMLParser.parse source
	  else MLParser.parse source

    fun parseOne source = let
	val parser = parser source
	val parser = Stats.doPhase parsePhase parser (* for correct timing *)
	fun doit () = (
	      case parser ()
	       of R.EOF => NONE
		| R.ABORT => fail "syntax error"
		| R.ERROR => fail "syntax error"
		| R.PARSE ast => SOME ast
	      (* end case *))
	in
	  doit
	end

    fun parse source = let
	val parser = parser source
	val parser = Stats.doPhase parsePhase parser (* for correct timing *)
	fun loop asts = (
	      case parser ()
	       of R.EOF => Ast.SeqDec(rev asts)
		| R.ABORT => fail "syntax error"
		| R.ERROR => fail "syntax error"
		| R.PARSE ast => loop(ast::asts)
	      (* end case *))
	in
	  loop []
	end

end
