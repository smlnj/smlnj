(* Copyright (c) 1998 by Lucent Technologies *)

(***************************************************************************

  TOKEN.SML: hash table for token recognition

 ***************************************************************************)


signature TOKENTABLE =
sig
	structure Tokens : C_TOKENS
	val checkToken : (string * int) -> (Tokens.svalue,int)Tokens.token
end

functor TokenTable(structure Tokens : C_TOKENS): TOKENTABLE = 
struct
  
  structure Tokens = Tokens
  structure ParseControl = Config.ParseControl
  type item = (int * int) -> (Tokens.svalue, int) Tokens.token
  exception Keyword
  exception LexError
  val keywords : item AtomTable.hash_table = AtomTable.mkTable(64, Keyword)
    
  local
    val insert = AtomTable.insert keywords
    fun ins (s, item) = insert (Atom.atom s, item)

    fun idTok (s, pos, endPos) =
	if TypeDefs.checkTdef(s) = true then 
	    Tokens.TYPE_NAME(s,pos,endPos)
	else Tokens.ID(s,pos,endPos)

    (* to enter GCC-style 'underscore'-versions of certain keywords *)
    fun insaug (s, item) = let
	fun item' (p as (pos, endPos)) =
	    case ParseControl.underscoreKeywords of
		NONE => idTok (s, pos, endPos)
	      | SOME true => item p
	      | SOME false =>
		(ParseControl.violation
		     (concat ["gcc-style keywords '__", s, "' or '__",
			      s, "__' are not allowed"]);
		     raise LexError)
    in
	ins ("__" ^ s, item');
	ins ("__" ^ s ^ "__", item')
    end
			    
    val normaltokens =
	[("auto", Tokens.AUTO),
	 ("extern", Tokens.EXTERN),
	 ("register", Tokens.REGISTER),
	 ("static", Tokens.STATIC),
	 ("unsigned", Tokens.UNSIGNED),
	 ("break", Tokens.BREAK),
	 ("case", Tokens.CASE),
	 ("continue", Tokens.CONTINUE),
	 ("default", Tokens.DEFAULT),
	 ("do", Tokens.DO),
	 ("else", Tokens.ELSE),
	 ("for", Tokens.FOR),
	 ("goto", Tokens.GOTO),
	 ("if", Tokens.IF),
	 ("enum", Tokens.ENUM),
	 ("float", Tokens.FLOAT),
	 ("double", Tokens.DOUBLE),
	 ("char", Tokens.CHAR),
	 ("int", Tokens.INT),
	 ("long", Tokens.LONG),
	 ("short", Tokens.SHORT),
	 ("struct", Tokens.STRUCT),
	 ("union", Tokens.UNION),
	 ("void", Tokens.VOID),
	 ("sizeof", Tokens.SIZEOF),
	 ("typedef", Tokens.TYPEDEF),
	 ("return", Tokens.RETURN),
	 ("switch", Tokens.SWITCH),
	 ("while", Tokens.WHILE)]

    (* tokens for which gcc has __* and __*__ versions *)
    val augmentabletokens =
	[("signed", Tokens.SIGNED),
	 ("const", fn p => if ParseControl.constAllowed
			       then (Tokens.CONST p)
			   else (ParseControl.violation
				 "the keyword 'const' not allowed";
				 raise LexError)),
	 ("volatile", fn p => if ParseControl.volatileAllowed 
				  then (Tokens.VOLATILE p)
			      else (ParseControl.violation
				    "the keyword 'volatile' not allowed";
				    raise LexError))]

    (* tokens for D *)
    val dtokens =
	[
	 ]

    val _ =
	(app ins normaltokens;
	 app ins augmentabletokens;
	 app insaug augmentabletokens;
	 (* enter D keywords only when allowed...
	  * (I think the ParseControl test is done at the wrong time here.
	  *  - Blume) *)
	 if ParseControl.Dkeywords then app ins dtokens else ())
  in
      fun checkToken (s, pos) = let
	  val endPos = pos + size s
	  val name = Atom.atom s
      in
	  case (AtomTable.find keywords name) of
	      SOME tokFn => tokFn(pos, endPos)
	    | NONE => idTok (s, pos, endPos)
      end
  end (* local *)
end
