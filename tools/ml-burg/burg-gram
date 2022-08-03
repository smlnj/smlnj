(* burg-gram
**
** ML-Yacc grammar for BURG.
*)

structure A = BurgAST;
fun outputRaw s = print (s:string)

%%

%term K_EOF
    | K_TERM
    | K_START
    | K_TERMPREFIX
    | K_RULEPREFIX
    | K_SIG
    | K_COLON
    | K_SEMICOLON
    | K_COMMA
    | K_LPAREN | K_RPAREN
    | K_EQUAL
    | K_PIPE
    | PPERCENT of string list
    | INT of int
    | ID  of string
    | RAW of string list

%nonterm full 		of A.spec_ast
       | spec 		of A.spec_ast
       | decl 		of A.decl_ast
       | binding 	of (string * string option)
       | cost 		of int list
       | costtail 	of int list
       | rulename	of string
       | pattern 	of A.pattern_ast
       | patterntail 	of A.pattern_ast list
       | decls 		of A.decl_ast list
       | rules 		of A.rule_ast list
       | rule 		of A.rule_ast
       | bindinglist 	of (string * string option) list
       | raw	 	of unit
       | prelude	of unit
       | postlude	of unit

%start full

%pos int
%pure

%eop K_EOF 

%name Burg

%%

full		: decls PPERCENT rules PPERCENT	
					(A.SPEC{head=PPERCENT1,
						decls=rev decls,
						rules=rev rules,
						tail=PPERCENT2})

decls		: (* empty *)		([])
		| decls decl		(decl :: decls)

decl		: K_TERM bindinglist	(A.TERM (rev bindinglist))
		| K_START ID		(A.START ID)
		| K_TERMPREFIX ID	(A.TERMPREFIX ID)
		| K_RULEPREFIX ID	(A.RULEPREFIX ID)
		| K_SIG ID		(A.SIG ID)


bindinglist	: binding		([binding])
		| bindinglist K_PIPE binding
					(binding :: bindinglist)

binding		: ID			((ID, NONE))
		| ID K_EQUAL ID		((ID1, SOME ID2))

rules		: (* empty *)		([])
		| rules rule		(rule :: rules)

rule		: ID K_COLON pattern K_EQUAL rulename cost K_SEMICOLON
					(A.RULE(ID, pattern, rulename, cost))

rulename	: ID			(ID)

pattern		: ID 			(A.PAT(ID, []))
		| ID K_LPAREN pattern patterntail K_RPAREN		
					(A.PAT(ID, pattern :: patterntail))

patterntail	: (* empty *)		([])
		| K_COMMA pattern patterntail
					(pattern :: patterntail)


cost		: (* empty *)		([])
		| K_LPAREN INT costtail K_RPAREN	
					(INT :: costtail)

costtail	: (* empty *)		([])
		| K_COMMA INT costtail	(INT :: costtail) 
