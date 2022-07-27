structure Preitem = 
  struct

    structure S = LLKSpec

    fun ntToString nt = 
	  if not (Nonterm.isSubrule nt) then Nonterm.name nt
	  else "(" ^ 
	       (String.concatWith " | " (map prodToString (Nonterm.prods nt))) ^
	       ")"

    and prodToString (S.PROD{rhs, ...}) = listToString (map symOf (!rhs))
    and symOf (S.ITEM {sym, ...}) = sym
    and toString (S.TOK t) = Token.toString t
      | toString (S.NONTERM (nt, args)) = 
	  String.concat ([ntToString nt] @ (
	    case args
	     of SOME args =>
		["@(", Action.toString args, ")"]
	      | _ =>  []))
      | toString (S.CLOS nt) = ntToString nt ^ "*"
      | toString (S.POSCLOS nt) = ntToString nt ^ "+"
      | toString (S.OPT nt) = ntToString nt ^ "?"
    and listToString l = String.concatWith " " (map toString l)

    fun name (S.TOK t) = Token.name t
      | name (S.NONTERM (nt, _)) = Nonterm.name nt
      | name (S.CLOS nt) = Nonterm.name nt
      | name (S.POSCLOS nt) = Nonterm.name nt
      | name (S.OPT nt) = Nonterm.name nt

  end