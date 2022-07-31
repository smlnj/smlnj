structure PPLib = struct

  structure PP = OldPrettyPrint

  type 'a pp = OldPrettyPrint.ppstream -> 'a -> unit

  exception ppExn of string

  val suppressPidUnderscores = ref true
  val suppressPidGlobalUnderscores = ref true
    (* usually want to do this to preserve linkability *)
  val suppressTidUnderscores = ref true
    (* These flags are set to true temporarily during parsing to make error messages
     * more readable, and are then resored to their original values.  See
     * parse-to-ast.sml.
     *)

  fun warning f msg = (print f; print ":"; print msg)

  fun ppToStrm pp strm v = 
      let val pps = PP.mk_ppstream {consumer = (fn s => TextIO.output (strm,s)),
				    flush = (fn () => TextIO.flushOut(strm)),
				    linewidth = 80}
       in pp pps v;
	  PP.flush_ppstream pps
      end

  fun ppToString pp v = PP.pp_to_string 80 pp v

  val addStr = PP.add_string
  val newline = PP.add_newline
  val bBlock  = PP.begin_block
  val eBlock = PP.end_block

  fun ppInt pps i = 
      if i >= 0 then addStr pps (Int.toString i)
      else (addStr pps "-"; addStr pps (Int.toString (~i)))

  fun ppInt32 pps i =
      if i >= 0 then addStr pps (Int32.toString i)
      else (addStr pps "-"; addStr pps (Int32.toString (~i)))

  fun ppLI pps i = 
      if i >= 0 then addStr pps (LargeInt.toString i)
      else (addStr pps "-"; addStr pps (LargeInt.toString (~i)))

  fun ppReal pps r = addStr pps (Real.toString r)

  fun ppString pps s =
      (addStr pps "\"";
       addStr pps (String.toCString s);
       addStr pps "\"")

  fun separate (pp,sep) pps [] = ()
    | separate (pp,sep) pps [x] = pp pps x
    | separate (pp,sep) pps (x::xs) =
      (pp pps x; sep pps; separate (pp,sep) pps xs)

  fun ppList {pp, sep, lDelim, rDelim} pps items = 
      (addStr pps lDelim;
       separate (pp,fn pps => addStr pps sep) pps items;
       addStr pps rDelim)

  fun space pps = addStr pps " "

  fun spaces pps 0 = ()
    | spaces pps n = (space pps; spaces pps (n-1))

  fun blockify n pp pps v = 
    ( newline pps
    ; bBlock pps PP.INCONSISTENT n
    ; spaces pps n
    ; pp pps v
    ; eBlock pps
    )

  fun ppOpt pp pps NONE = ()
    | ppOpt pp pps (SOME x) = pp pps x

  fun ppSp pp pps v = (space pps; pp pps v)

  fun ppSpOpt pp pps opt = ppOpt (ppSp pp) pps opt

  fun ppGuarded s bool pps = if bool then addStr pps s else ()
(*      
  fun ppPid (pidtab: Tables.pidtab, _) pps pid = 
      let fun ppSymbolQuietly symbol = addStr pps (Symbol.name symbol)
	  fun ppSymbolVerbose symbol = ( addStr pps (Symbol.name symbol)
				       ; addStr pps "_"
				       ; addStr pps (Pid.toString pid)
				       )
	  val ppSymbol = if !suppressPidUnderscores then ppSymbolQuietly
			 else ppSymbolVerbose
      in case Pidtab.find (pidtab,pid)
	   of SOME {symbol,kind,...} =>
	       (case kind
		  of (Info.FIELDp _ |
		      Info.VARIABLEp{stClass=SOME Ast.EXTERN,...} |
		      Info.VARIABLEp{global=true,...}) =>
		      addStr pps (symbol2string symbol)
		   | Info.VARIABLEp{global=false,...} => ppSymbol symbol
		   | Info.LABEL => ppSymbol symbol
		   | Info.TYPEDEFp _ => ppSymbol symbol
		   | Info.TAGp _ => ppSymbol symbol)
	 | _ => addStr pps (Pid.toString pid)
      end
*)
  fun ppSymbol' pps symbol = addStr pps (Symbol.name symbol)

  fun ppSymbol pps (symbol: Symbol.symbol, uid: Pid.uid) =
      (addStr pps (Symbol.name symbol);
       if !suppressPidUnderscores then ()
       else (addStr pps "_";
	     addStr pps (Pid.toString uid)))

  fun ppId pps ({name,uid,kind,stClass,global,...}: Ast.id) = 
      case (stClass,global)
	of ((Ast.EXTERN,_) | (_, true)) => (* globals *)
	     if !suppressPidGlobalUnderscores then ppSymbol' pps name
	     else ppSymbol pps (name,uid)
	 | _ => ppSymbol pps (name,uid)
          (* no uids printed for globals to preserve linkability *)

  fun ppLabel pps ({name,uid,...}: Ast.label) = 
      ppSymbol pps (name,uid)

  fun ppMember pps ({name,...}: Ast.member) = 
      ppSymbol' pps name

  fun ppTid (tidtab: Tables.tidtab) pps tid = 
      case Tidtab.find (tidtab,tid)
	of SOME {name=NONE,...} =>
	    addStr pps (Tid.toString tid)
	 | SOME {name=SOME id,...} =>
	    if !suppressTidUnderscores then addStr pps id
	    else (addStr pps id;
		  addStr pps "_";
		  addStr pps (Tid.toString tid))
	 | NONE => addStr pps (Tid.toString tid)

end
