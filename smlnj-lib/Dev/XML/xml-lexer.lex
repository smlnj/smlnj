(* xml-lexer.lex
 *
 * COPYRIGHT (c) 2013 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An ML-ULex parser for parsing XML files.
 *
 * TODO: line ending normalization?
 *)

%name XMLLexer;

%arg (lexErr);

%defs (
  structure T = XMLTokens
  type lex_result = T.token
  fun eof () = T.EOF

(* list of strings to build attribute values *)
  val text : string list ref = ref []
  fun addText s = (text := s :: !text)
  fun addDecimalEscape s = addText(UTF8.encode(Word.fromInt(Option.valOf(Int.fromString s))))
  fun addHexEscape s = addText(UTF8.encode(Option.valOf(Word.fromString s)))
  fun textToString () = let
	val s = String.concat(List.rev(!text))
	in
	  text := []; s
	end

(* trim m characters from the left and n characters from the right *)
  fun trim (m, ss, n) = Substring.string(Substring.triml m (Substring.trimr n ss))
);

%let ws = [ \t\n\v\f\r];
%let digit = [0-9];
%let alpha = [a-zA-Z];
%let idstartchr = [a-zA-Z_:];
%let idchr = ({idstartchr}|[-.0-9]);
%let pubidchr1 = [ \n\n\t] | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%];
%let pubidchr2 = [ \n\n\t] | [a-zA-Z0-9] | [-()+,./:=?;!*#@$_%]; (* without ' *)

(* the lexer states:
 * INITIAL
 * COM		scanning inside "<!--"
 * TAG		scanning inside "<" or "<?xml"; whitespace is skipped.
 * LIT1		double-quoted literal
 * LIT2		single-quoted literal
 * DOCTYPE	scanning inside "<!DOCTYPE"; whitespace is skipped.
 *)
%states INITIAL COM TAG LIT1 LIT2 DOCTYPE;

<INITIAL>"<!--"			=> (addText yytext; YYBEGIN COM; continue());
<COM>"-->"			=> (addText yytext; YYBEGIN INITIAL; T.COM(textToString()));
<COM>.				=> (addText yytext; continue());

<INITIAL>"<"			=> (YYBEGIN TAG; T.OPEN_START_TAG);
<INITIAL>"</"			=> (YYBEGIN TAG; T.OPEN_END_TAG);
<INITIAL>"<?"[xX][mM][lL]	=> (YYBEGIN TAG; T.OPEN_XML_TAG);
<INITIAL>"<!DOCTYPE"		=> (YYBEGIN DOCTYPE; T.OPEN_DOCTYPE);

<DOCTYPE>[pP][uU][bB][lL][iI][cC]
				=> (T.PUBLIC);
<DOCTYPE>[sS][yY][sS][tT][eE][mM]
				=> (T.SYSTEM);
<DOCTYPE>"\""{pubidchr1}*"\""	=> (T.LIT(String.substring(yytext, 1, size yytext - 2)));
<DOCTYPE>"'"{pubidchr2}*"'"	=> (T.LIT(String.substring(yytext, 1, size yytext - 2)));
<DOCTYPE>">"			=> (YYBEGIN INITIAL; T.CLOSE_TAG);

<TAG,DOCTYPE>{ws}+		=> (skip());
<TAG>"?>"			=> (YYBEGIN INITIAL; T.CLOSE_PI_TAG);
<TAG>">"			=> (YYBEGIN INITIAL; T.CLOSE_TAG);
<TAG>"/>"			=> (YYBEGIN INITIAL; T.CLOSE_EMPTY_TAG);
<TAG>"="			=> (T.SYM_EQ);
<TAG,DOCTYPE>{idstartchr}{idchr}*
				=> (T.ID yytext);
<TAG>"\""			=> (YYBEGIN LIT1; continue());
<TAG>"'"			=> (YYBEGIN LIT2; continue());

<LIT1>"\""			=> (YYBEGIN TAG; T.LIT(textToString()));
<LIT2>"\'"			=> (YYBEGIN TAG; T.LIT(textToString()));
<LIT1,LIT2>"&quot;"		=> (addText ("\""); continue());
<LIT1,LIT2>"&lt;"		=> (addText ("<"); continue());
<LIT1,LIT2>"&gt;"		=> (addText (">"); continue());
<LIT1,LIT2>"&amp;"		=> (addText ("&"); continue());
<LIT1,LIT2>"&apos;"		=> (addText ("'"); continue());
<LIT1,LIT2>"&#"[0-9]+";"	=> (addDecimalEscape(trim(2, yysubstr, 1)); continue());
<LIT1,LIT2>"&#x"[a-fA-F0-9]+";"	=> (addHexEscape(trim(3, yysubstr, 1)); continue());
<LIT1>[^"<>&]+			=> (addText yytext; continue());
<LIT2>[^'<>&]+			=> (addText yytext; continue());

(* we handle whitespace specially, so that initial/trailing whitespace can be preserved
 * when necessary.
 *)
<INITIAL>{ws}+			=> (T.WS yytext);
<INITIAL>[^ \n\t\r<&]+		=> (T.TEXT yytext);
<INITIAL>"&quot;"		=> (T.TEXT "\"");
<INITIAL>"&lt;"			=> (T.TEXT "<");
<INITIAL>"&gt;"			=> (T.TEXT ">");
<INITIAL>"&amp;"		=> (T.TEXT "&");
<INITIAL>"&apos;"		=> (T.TEXT "'");
<INITIAL>"<![CDATA[".*"]]>"	=> (T.CDATA(trim (9, yysubstr, 3)));

<INITIAL>.		        => (lexErr(yypos, [
                                        "bad character `", String.toString yytext, "'"
                                      ]);
                                    continue());
<DOCTYPE>.		        => (lexErr(yypos, [
                                        "bad character `", String.toString yytext, "' in DOCTYPE"
                                      ]);
                                    continue());
<TAG>.		                => (lexErr(yypos, [
                                        "bad character `", String.toString yytext, "' in tag"
                                      ]);
                                    continue());
<LIT1,LIT2>.		        => (lexErr(yypos, [
                                        "bad character `", String.toString yytext, "' in attribute value"
                                      ]);
                                    continue());
