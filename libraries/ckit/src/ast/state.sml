(* Copyright (c) 1998 by Lucent Technologies *)

structure State : STATE =
struct

  structure Sym = Symbol
  (* uid structures: program, type and adornment identifiers *)
  structure Pid = Pid
  structure Tid = Tid
  structure Aid = Aid
  (* imperative uid tables (hashtables) *)
  structure TT = Tidtab
  structure AT = Aidtab  (* was TypeAddornmentTab *)

  (* symbol table binary maps *)
  structure ST = BinaryMapFn (struct 
			        type ord_key = Sym.symbol
			        val compare = Sym.compare 
			      end)

  (* int binary maps *)
  structure IT = BinaryMapFn (struct 
			        type ord_key = LargeInt.int
			        val compare = LargeInt.compare
			      end)


  (* environments *)
  type symtab = Bindings.symBinding ST.map
  type env = symtab list       (* local environments *)


  (* global context types *)

  type uidTables =
    {ttab : Tables.tidtab,     (* type name table *)
     atab : Tables.aidtab,     (* adornment table *)
     implicits : Tables.aidtab}   (* "optional" adornment table -- for special casts *)

  type envContext =
    {globalEnv : symtab ref,   (* the global symbol table *)
     localEnv : env ref}       (* the local environment stack *)


  (* local ("working") context types *)

  (* tidsContext: sequence of tids of types created while processing a phrase *)
  type tidsContext =
    {newTids : Tid.uid list ref}

  (* tmpVariables: sequence of pids created while processing a phrase *)
     (* used when inserting explicit coercions in the case of ++, --, += *)
  type tmpVariables =
    {newVariables : Ast.id list ref}

  (* for use in D *)
  type typeContext =
    {typeCxts : Ast.ctype option list ref}  

  (* information for the current function def *)
  type funContext =
    {labelTab : (Ast.label * bool) ST.map ref,
     gotos : Sym.symbol list ref,
     returnTy : Ast.ctype option ref}

  (* table for collecting switch labels while processing switch statements *)
  type switchContext =
    {switchLabels : {switchTab : unit IT.map, default : bool} list ref}

  (* location context, mainly for error messages *)
  type locContext =
    {locStack : SourceMap.location list ref}


  (* global state components *)
  type globalState =
    {uidTables : uidTables,
     envContext : envContext, (* contains some local working state in localEnv *)
     errorState : Error.errorState}

  (* local, "working", state components *)
  type localState =
    {locContext: locContext,
     tidsContext : tidsContext,
     tmpVariables : tmpVariables,
     funContext: funContext,
     switchContext: switchContext,
     typeContext: typeContext}


  (* initial state information for calling makeAst *)
  datatype stateInfo
    = STATE of uidTables * symtab  (* previous state info *)
    | INITIAL (* no previous state info *)

  type stateFuns =
    {globalState : globalState,
     localState : localState,
       (* the state records, included for convenience *)

     locFuns :
      {pushLoc : SourceMap.location -> unit,
       popLoc : unit -> unit,
       getLoc : unit -> SourceMap.location,
       error : string -> unit,
       warn : string -> unit},

     tidsFuns :
      {pushTids : Tid.uid -> unit,
       resetTids : unit -> Tid.uid list},

     tmpVarsFuns :
      {pushTmpVars : Ast.id -> unit,
       resetTmpVars : unit -> Ast.id list},

     envFuns :
      {topLevel : unit -> bool,
       pushLocalEnv : unit -> unit,
       popLocalEnv : unit -> unit,
       lookSym : Sym.symbol -> Bindings.symBinding option,
       bindSym : Sym.symbol * Bindings.symBinding -> unit,
       lookSymGlobal : Sym.symbol -> Bindings.symBinding option,
       bindSymGlobal : Sym.symbol * Bindings.symBinding -> unit,
       lookLocalScope : Sym.symbol -> Bindings.symBinding option,
       getGlobalEnv : unit -> symtab},

     uidTabFuns :
      {bindAid : Ast.ctype -> Aid.uid,
       lookAid : Aid.uid -> Ast.ctype option,
       bindTid : Tid.uid * Bindings.tidBinding -> unit,
       lookTid : Tid.uid -> Bindings.tidBinding option},

     funFuns :
      {newFunction : Ast.ctype -> unit,
       getReturnTy : unit -> Ast.ctype option,
       checkLabels : unit -> (Symbol.symbol * SourceMap.location) option,
       addLabel : Sym.symbol * SourceMap.location -> Ast.label,
       addGoto : Sym.symbol * SourceMap.location -> Ast.label},

     switchFuns :
      {pushSwitchLabels : unit -> unit,
       popSwitchLabels : unit -> unit,
       addSwitchLabel : LargeInt.int -> string option,
         (* returns error message option *)
       addDefaultLabel : unit -> string option}}
         (* returns error message option *)


(* state initialization *)
fun initLocal () : localState =
    {tidsContext =
      {newTids = ref []},
     tmpVariables =
      {newVariables = ref []},
     typeContext =
      {typeCxts = ref []},
     funContext =
      {labelTab = ref ST.empty,
       gotos = ref [],
       returnTy = ref NONE},
     switchContext = 
      {switchLabels = ref []},
     locContext =
      {locStack = ref [SourceMap.UNKNOWN]}}

fun initGlobal(INITIAL, errorState: Error.errorState) : globalState =
    {uidTables =
      {ttab = TT.uidtab(),
       atab = AT.uidtab(),
       implicits = AT.uidtab()},
     envContext =
      {globalEnv = ref ST.empty,
       localEnv = ref []},
     errorState = errorState}

  | initGlobal(STATE({ttab,atab,implicits},globalEnv), errorState) =
    {uidTables =
      {ttab = ttab,
       atab = atab,
       implicits = implicits},
     envContext =
      {globalEnv = ref(globalEnv),
       localEnv = ref []},
     errorState = errorState}


(* provide packages of implicit state manipulation functions *)
fun stateFuns(globalState as {uidTables, envContext, errorState} : globalState,
	      localState as 
	      {tidsContext, tmpVariables, funContext, switchContext, locContext,...}: localState)
    : stateFuns =
let

val bug = Error.bug errorState

(* tidsContext functions ***********************************************)
local val {newTids} = tidsContext in

  fun pushTids tid =
      newTids := tid :: !newTids

  fun resetTids () =
      rev(!newTids) before (newTids := [])
      (* ct's pushed onto newTids as encountered; need to reverse list to
       * give original program order*)
end


(* newVariables functions ***********************************************)
local val {newVariables} = tmpVariables in

  fun pushTmpVars pidTy =
      newVariables := pidTy :: !newVariables

  fun resetTmpVars () =
      rev(!newVariables) before (newVariables := [])
      (* pidTy pairs are pushed onto newVariables as encountered; need to reverse list to
       * give original program order*)
end


(* location functions *************************************************)
local val {locStack} = locContext  (* also uses errorState *)
in

  (* accesses: locStack *)
  fun error (msg: string) =
      case !locStack
        of loc :: _ => Error.error(errorState,loc,msg)
         | nil => bug "Empty location stack"

  (* accesses: locStack *)
  fun warn (msg: string) =
      case !locStack
        of loc :: _ => Error.warning(errorState,loc,msg)
         | nil => bug "Empty location stack"

  (* get "current" location *)
  (* accesses: locStack *)
  fun getLoc () =
      case !locStack
	of loc :: _ => loc
         | nil => (bug "getLoc: empty location stack"; SourceMap.UNKNOWN)

  (* push the location stack, on entering a marked phrase *)
  (* affects: locStack *)
  fun pushLoc loc =
      locStack := loc :: !locStack

  (* pop the location stack, on exiting a marked phrase *)
  (* affects: locStack *)
  fun popLoc () =
      case !locStack
	of _ :: rest => locStack := rest
         | nil => bug "popLoc: empty location stack"

end (* locContext *)

(* switch label functions ***************************************************)
local val {switchLabels} = switchContext in

  (* effects: switchLabels *)
  fun popSwitchLabels () =
      (case !switchLabels
	 of _ :: swLabels => switchLabels := swLabels
	  | nil => bug "State: can't pop empty switchlabels")

  (* effects: switchLabels *)
  fun pushSwitchLabels () =
      switchLabels := {switchTab = IT.empty, default=false} :: !switchLabels

  (* effects: switchLabels *)
  fun addSwitchLabel (i: LargeInt.int) : string option =
      case !switchLabels
	of {switchTab, default} :: rest =>
	    (case IT.find(switchTab, i)
	       of NONE =>
		   let val switchTab = IT.insert(switchTab, i, ())
		    in switchLabels := {switchTab=switchTab, default=default}::rest;
		       NONE
		   end
		| SOME _ =>  (* error return *)
		   SOME ("Duplicate case label " ^ (LargeInt.toString i) ^
			 " in the same switch statement"))
	 | nil => (* error return *)
	    SOME ("Case label " ^ (LargeInt.toString i) ^
		  " appears outside a switch statement")
    
  (* effects: switchLabels *)
  fun addDefaultLabel () : string option =
      case !switchLabels
        of {switchTab, default} :: rest =>
	     if default then (* error return *)
	        SOME "Duplicate default label in the same switch statement"
	     else (switchLabels := {switchTab=switchTab, default=true} :: rest;
		   NONE)
	 | nil => (* error return *)
	     SOME "Default label appears outside a switch statement"

end (* switchContext *)

(* identifier table functions *******************************************)
local val {ttab,atab,...} = uidTables in

  (* generate a new aid, bind it to ty in atab, and return it *)
  fun bindAid ty =
      let val aid = Aid.new ()
       in AT.insert(atab,aid,ty);
	  aid
      end

  fun lookAid aid = AT.find (atab,aid)

  fun bindTid (tid, binding) = TT.insert(ttab,tid,binding)

  fun lookTid tid = TT.find (ttab,tid)

end (* identifier functions *)


(* funContext functions *************************************************)
local val {labelTab,gotos,returnTy} = funContext
in

  (* effects: funContext *)
  fun newFunction(returnty) =
      (labelTab := ST.empty;
       gotos := [];
       returnTy := SOME returnty)

  (* accesses funContext *)
  fun getReturnTy () = !returnTy

  (* accesses: ? 
   * effects: labelTab *)
  (* DBM: labToPid called only with definition=false from addGoto,
   * so errorFl will always be returned false in that case.  On the
   * other hand, in addLabel, the value of the error flag is discarded. *)
  fun symbolToLabel (definition: bool, labSym : Symbol.symbol, loc: SourceMap.location)
         : (Ast.label * bool) =
      case ST.find(!labelTab,labSym)
        of SOME(label, true) => (* previously defined *)
	     if definition then (label, true) (* error, multiple defitions *)
	     else (label, false) (* no error *)
	 | SOME(label, false) => (* label has been seen previously but not defined *)
	    (if definition
	     then (labelTab := ST.insert(!labelTab, labSym, (label, true)))
		   (* mark as defined, rebinding labSym in labelTab *)
             else ();
	     (label, false)) (* no error *)
	 | NONE => (* new label *)
	     let val label = {name=labSym, uid = Pid.new (), location=loc}
	      in labelTab := ST.insert(!labelTab, labSym, (label, definition));
		 (label, false)
	     end

  (* accesses: funContext
   * effects: labelTab, gotos *)
  fun addGoto (labSym, loc) =
      let val (label, _) = symbolToLabel(false, labSym, loc)
	     (* discard error flag: no possibility of an error condition,
	      * since not a defining occurrence of the label *)
       in gotos := labSym :: !gotos;
	  label
      end
  
  (* global: funContext
   * effects: labelTab *)
  fun addLabel (labSym, loc) =
      let val (label, errorFlag) = symbolToLabel(true, labSym, loc)
       in if errorFlag then
	    error("Repeated definition of label " ^ (Sym.name labSym))
	  else ();
	  label
      end

  (* access: labelTab *)
  fun checkLabels () =
      let fun check(g :: gl) =
	      (case ST.find(!labelTab,g)
		 of SOME(pid, true) => check gl
		  | SOME({name,location,...}, false) =>
		      SOME(name, location)
		      (* error in program -- label used but not defined *)
		  | NONE => (bug "State: checkLabels: goto label not in table"; NONE))
	    | check nil = NONE (* ok -- all goto target labels defined *)
       in check (!gotos)
      end

end (* funContext *)


(* environment functions *************************************************)
local val {localEnv, globalEnv} = envContext
in

  (* Are we in a top-level environment *)
  (* accesses: localEnv *)
  fun topLevel () = List.null(!localEnv)

  (* effects: localEnv *)
  fun pushLocalEnv () = localEnv := ST.empty :: !localEnv

  (* effects: localEnv *)
  fun popLocalEnv () =
      case !localEnv
	of st :: env => localEnv := env
	 | nil => bug "State: popping an empty local environment"

  (* lookSym: lookup a symbol in the full environment (localEnv over globalEnv) *)
  (* accesses: globalEnv, localEnv *)
  fun lookSym (sym: Sym.symbol) : Bindings.symBinding option =
      let fun lookup [] = 
	       ST.find(!globalEnv,sym)
	    | lookup (st::rest) =
	       (case ST.find (st, sym)
		  of SOME x => SOME x
		   | NONE => lookup rest)
       in lookup (!localEnv)
      end

  (* accesses: globalEnv *)
  fun lookSymGlobal (sym: Sym.symbol): Bindings.symBinding option =
      ST.find(!globalEnv,sym)

  (* bindSym : symbol * binding -> unit
   * bind a new symbol.
   * affects: environment *)
  fun bindSym (sym, binding) =
      case !localEnv
	of st :: outer => 
	     localEnv := ST.insert(st,sym,binding) :: outer
	 | nil =>
	     globalEnv := ST.insert(!globalEnv,sym,binding)

  (* forces entry into the global env (used for patching up undeclared
   * function calls).  WARNING: new pid/uid generated *)
  (* effects: globalEnv *)
  fun bindSymGlobal (sym, binding) =
      globalEnv := ST.insert(!globalEnv, sym, binding)

  (* is symbol bound in current innermost scope level *)
  (* accesses: globalEnv *)
  fun lookLocalScope sym =
      case !localEnv
	of nil => ST.find(!globalEnv,sym)
	 | st :: _ => ST.find(st,sym)

  (* return the current global environment (symtab) *)
  fun getGlobalEnv (): symtab =
      !globalEnv

end (* environment *)

in (* state function package *)
   {globalState = globalState,
    localState = localState,
    locFuns =
     {pushLoc = pushLoc,
      popLoc = popLoc,
      getLoc = getLoc,
      error = error,
      warn = warn},

    tidsFuns =
     {pushTids = pushTids,
      resetTids = resetTids},

    tmpVarsFuns =
     {pushTmpVars = pushTmpVars,
      resetTmpVars = resetTmpVars},

    envFuns =
     {topLevel = topLevel,
      pushLocalEnv = pushLocalEnv,
      popLocalEnv = popLocalEnv,
      lookSym = lookSym,
      bindSym = bindSym,
      lookSymGlobal = lookSymGlobal,
      bindSymGlobal = bindSymGlobal,
      lookLocalScope = lookLocalScope,
      getGlobalEnv = getGlobalEnv},

    uidTabFuns =
     {bindAid = bindAid,
      lookAid = lookAid,
      bindTid = bindTid,
      lookTid = lookTid},

    funFuns =
     {newFunction = newFunction,
      getReturnTy = getReturnTy,
      checkLabels = checkLabels,
      addLabel = addLabel,
      addGoto = addGoto},

    switchFuns =
     {pushSwitchLabels = pushSwitchLabels,
      popSwitchLabels = popSwitchLabels,
      addSwitchLabel = addSwitchLabel,
      addDefaultLabel = addDefaultLabel}}

end (* fun stateFuns *)

end (* structure State *)
