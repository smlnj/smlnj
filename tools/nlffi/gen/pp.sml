(*
 * pp.sml - Some simple pretty-printing infrastructure for the ml-ffigen
 *          program.
 *
 *  (C) 2001, Lucent Technologies, Bell Labs
 *
 * author: Matthias Blume (blume@research.bell-labs.com)
 *)
structure PrettyPrint = struct

    structure PP = PPStreamFn (structure Token = StringToken
			       structure Device = CPIFDev)

    datatype mltype =
	ARROW of mltype * mltype
      | TUPLE of mltype list
      | CON of string * mltype list
      | RECORD of (string * mltype) list

    val Unit = TUPLE []
    fun Type t = CON (t, [])
    fun St tag = Type (concat ["ST_", tag, ".tag"])
    fun Un tag = Type (concat ["UT_", tag, ".tag"])
    fun En tag = Type (concat ["ET_", tag, ".tag"])

    datatype tcontext = C_STAR | C_ARROW | C_COMMA | C_CON

    fun simplify (CON ("unit", [])) = Unit
      | simplify (TUPLE [t]) = simplify t
      | simplify (CON (obj as ("obj" | "obj'"),
		       [CON (k as ("schar" | "uchar" | "sint" | "uint" |
				   "sshort" | "ushort" | "slong" | "ulong" |
				   "float" | "double" | "voidptr"), []),
			c])) =
	CON (concat [k, "_", obj], [simplify c])
      | simplify (CON (obj as ("obj" | "obj'"),
		       [CON ("fptr", [f]), c])) =
	CON ("fptr_" ^ obj, [simplify f, simplify c])
      | simplify (CON (obj as ("obj" | "obj'"),
		       [CON ("su", [s]), c])) =
	CON ("su_" ^ obj, [simplify s, simplify c])
      | simplify (CON ("Dim.dim", [n, CON (("Dim.nonzero" | "nonzero"), [])])) =
	CON ("dim", [simplify n])
      | simplify (CON ("Dim.dec", [])) = CON ("dec", [])
      | simplify (CON (k as ("Dim.dg0" | "Dim.dg1" | "Dim.dg2" | "Dim.dg3" |
			     "Dim.dg4" | "Dim.dg5" | "Dim.dg6" | "Dim.dg7" |
			     "Dim.dg8" | "Dim.dg9"), [n])) =
	CON (String.extract (k, 4, NONE), [simplify n])
      | simplify (ARROW (t1, t2)) = ARROW (simplify t1, simplify t2)
      | simplify (TUPLE tl) = TUPLE (map simplify tl)
      | simplify (RECORD ml) = RECORD (map (fn (n, t) => (n, simplify t)) ml)
      | simplify (CON (k, tl)) = CON (k, map simplify tl)

    fun ppType0 s (t as ARROW _, c) =
	let fun loop (ARROW (x, y)) =
		(ppType0 s (x, C_ARROW); PP.string s " ->"; PP.space s 1;
		 loop y)
	      | loop t = ppType0 s (t, C_ARROW)
	    val paren = not (c = C_COMMA)
	    val indent = if paren then 5 else 4
	in
	    PP.openHOVBox s (PP.Rel indent);
	    if paren then PP.string s "(" else ();
	    loop t;
	    if paren then PP.string s ")" else ();
	    PP.closeBox s
	end
      | ppType0 s (TUPLE [], _) = PP.string s "unit"
      | ppType0 s (TUPLE [t], c) = ppType0 s (t, c)
      | ppType0 s (TUPLE tl, c) = let
	    fun loop [] = ()	(* cannot happen *)
	      | loop [t] = ppType0 s (t, C_STAR)
	      | loop (h :: tl) = (ppType0 s (h, C_STAR);
				  PP.string s " *";
				  PP.space s 1;
				  loop tl)
	    val paren =
		case c of (C_STAR | C_CON) => true
			| (C_ARROW | C_COMMA) => false
	    val indent = if paren then 1 else 0
	in
	    PP.openHVBox s (PP.Rel indent);
	    if paren then PP.string s "(" else ();
	    loop tl;
	    if paren then PP.string s ")" else ();
	    PP.closeBox s
	end
      | ppType0 s (RECORD [], _) = PP.string s "{}"
      | ppType0 s (RECORD tl, _) = let
	    fun loop [] = ()		(* cannot happen *)
	      | loop [(n, t)] = (PP.string s (n ^ " : ");
				 ppType0 s (t, C_COMMA))
	      | loop ((n, t) :: tl) = (PP.string s (n ^ " : ");
				       ppType0 s (t, C_COMMA);
				       PP.string s ",";
				       PP.space s 1;
				       loop tl)
	in
	    PP.openHVBox s (PP.Rel 2);
	    PP.string s "{ ";
	    loop tl;
	    PP.string s " }";
	    PP.closeBox s
	end
      | ppType0 s (CON (k, []), _) = PP.string s k
      | ppType0 s (CON (k, [t]), _) =
	(PP.openHBox s;
	 ppType0 s (t, C_CON);
	 PP.space s 1;
	 PP.string s k;
	 PP.closeBox s)
      | ppType0 s (CON (k, tl), _) = let
	    fun loop [] = ()	(* cannot happen *)
	      | loop [t] = ppType0 s (t, C_COMMA)
	      | loop (h :: tl) =
		(ppType0 s (h, C_COMMA); PP.string s ","; PP.space s 1; loop tl)
	in
	    PP.openHBox s;
	    PP.openHVBox s (PP.Rel 1);
	    PP.string s "(";
	    loop tl;
	    PP.string s ")";
	    PP.closeBox s;
	    PP.space s 1;
	    PP.string s k;
	    PP.closeBox s
	end

    (* start with comma context *)
    fun ppType s t = ppType0 s (simplify t, C_COMMA)
    fun ppType' s (t, c) = ppType0 s (simplify t, c)

    datatype mlexp =
	ETUPLE of mlexp list
      | ERECORD of (string * mlexp) list
      | EVAR of string
      | EAPP of mlexp * mlexp
      | ECONSTR of mlexp * mltype
      | ESEQ of mlexp * mlexp

    datatype econtext = EC_APP | EC_COMMA

    fun ppExp0 s (ETUPLE [], _) = PP.string s "()"
      | ppExp0 s (ETUPLE [x], c) = ppExp0 s (x, c)
      | ppExp0 s (ETUPLE xl, _) = let
	    fun loop [] = ()
	      | loop [x] = ppExp0 s (x, EC_COMMA)
	      | loop (x :: xl) =
		(ppExp0 s (x, EC_COMMA); PP.string s ","; PP.space s 1;
		 loop xl)
	in
	    PP.openHVBox s (PP.Rel 1);
	    PP.string s "(";
	    loop xl;
	    PP.string s ")";
	    PP.closeBox s
	end
      | ppExp0 s (ERECORD [], _) = PP.string s "{}"
      | ppExp0 s (ERECORD xl, _) = let
	    fun loop [] = ()
	      | loop [(n, x)] = (PP.string s (n ^ " =");
				 PP.space s 1;
				 ppExp0 s (x, EC_COMMA))
	      | loop ((n, x) :: xl) = (PP.string s (n ^ " =");
				       PP.space s 1;
				       ppExp0 s (x, EC_COMMA);
				       PP.string s ",";
				       PP.space s 1;
				       loop xl)
	in
	    PP.openHVBox s (PP.Rel 2);
	    PP.string s "{ ";
	    loop xl;
	    PP.string s " }";
	    PP.closeBox s
	end
      | ppExp0 s (EVAR v, _) = PP.string s v
      | ppExp0 s (EAPP (x, y), c) = let
	    fun loop (EAPP (x, y)) =
		(loop x; ppExp0 s (y, EC_APP); PP.space s 1)
	      | loop x = (ppExp0 s (x, EC_APP);
			  PP.space s 1;
			  PP.openHOVBox s (PP.Rel 0))
	    val paren = c = EC_APP
	in
	    PP.openHOVBox s (PP.Abs 4);
	    if paren then PP.string s "(" else ();
	    loop x;
	    ppExp0 s (y, EC_APP);
	    if paren then PP.string s ")" else ();
	    PP.closeBox s;
	    PP.closeBox s
	end
      | ppExp0 s (ECONSTR (x, t), c) = let
	    val paren = c = EC_APP
	    val indent = if paren then 5 else 4
	    val tc = if paren then C_CON else C_COMMA
	in
	    PP.openHOVBox s (PP.Rel indent);
	    if paren then PP.string s "(" else ();
	    ppExp0 s (x, c);
	    PP.nbSpace s 1;
	    PP.string s ":";
	    PP.space s 1;
	    ppType' s (t, tc);
	    if paren then PP.string s ")" else  ();
	    PP.closeBox s
	end
      | ppExp0 s (ESEQ (x, y), c) = let
	in
	    PP.string s "(";
	    PP.openHVBox s (PP.Rel 0);
	    ppExp0 s (x, EC_COMMA);
	    PP.string s ";";
	    PP.space s 1;
	    ppExp0 s (y, EC_COMMA);
	    PP.string s ")";
	    PP.closeBox s
	end

    fun ppExp s x = ppExp0 s (x, EC_COMMA)
    fun ppExp' s x = ppExp0 s (x, EC_APP)

    fun ppFun s (name, args, body) =
	(PP.openHOVBox s (PP.Rel 4);
	 PP.string s ("fun " ^ name);
	 PP.nbSpace s 1;
	 app (fn a => (ppExp' s a; PP.space s 1)) args;
	 PP.string s "=";
	 PP.nbSpace s 1;
	 PP.openBox s (PP.Rel 0);
	 ppExp s body;
	 PP.closeBox s;
	 PP.closeBox s)
end
