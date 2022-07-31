(* Copyright (c) 1998 by Lucent Technologies *)

(* --------------------------------------------------------------------
 * State: a local structure for operating on context state during the build-ast
 * (elaboration) phase.
 * State includes:
 *   - a global symbol table (#globalEnv envContext)
 *   - a stack of local symbol tables  (#localEnv envContext)
 *   - a stack of locations for error reporting (#locStack locContext)
 *   - a table of named type identifiers (uidTables.ttab)
 *   - a table of adornment types (uidTables.atab) giving the type for each expression
 *   - a table of adornment types (uidTables.implicits)
        giving implicit coercions for each expression (if any)
 *   - a list of type identifiers (defined in the current "context")
 *      (tidsContext.newTids)
 *   - a stack of tables of switch statement labels (switchContext.switchLabels)
 * --------------------------------------------------------------------
 *)
      
signature STATE =
sig

(* finite map structures *)

  structure ST : ORD_MAP where type Key.ord_key = Symbol.symbol
  structure IT : ORD_MAP where type Key.ord_key = LargeInt.int


(* environments *)

  type symtab = Bindings.symBinding ST.map
  type env = symtab list  (* local environment stack *)


(* global context types *)

  type uidTables =
    {ttab : Tables.tidtab,     (* type name table *)
     atab : Tables.aidtab,     (* adornment table *)
     implicits : Tables.aidtab}   (* "optional" adornment table -- for special casts *)

  type envContext =
    {globalEnv : symtab ref,   (* the global symbol table *)
     localEnv : env ref}       (* the local environment stack *)


(* local context types : temporary information used during elaboration *)

  (* tidsContext: sequence of tids of types created while processing a phrase *)
  type tidsContext =
    {newTids : Tid.uid list ref}

  (* tmpVariables: sequence of (pid, ty) pairs created while processing a phrase *)
     (* used when inserting explicit coercions in the case of ++, --, += *)
  type tmpVariables =
    {newVariables : Ast.id list ref}

  (* for use in D *)
  type typeContext =
    {typeCxts : Ast.ctype option list ref}

  (* funContext: information for the current function def *)
  type funContext =
    {labelTab : (Ast.label * bool) ST.map ref,
     gotos : Symbol.symbol list ref,
     returnTy : Ast.ctype option ref}

  (* table for collecting switch labels while processing switch statements *)
  type switchContext =
    {switchLabels : {switchTab : unit IT.map, default : bool} list ref}

  type locContext = (* location context *)
    {locStack : SourceMap.location list ref}

  (* global state components *)
  type globalState =
    {uidTables : uidTables,
     envContext : envContext,  (* contains some local working state in localEnv *)
     errorState : Error.errorState}

  (* local, "working", state components, holding temporary information *)
  type localState =
    {locContext: locContext,
     tidsContext : tidsContext,
     tmpVariables : tmpVariables,
     funContext: funContext,
     switchContext: switchContext,
     typeContext: typeContext}

  (* initial information for calling makeAst *)
  datatype stateInfo
    = STATE of uidTables * symtab  (* state carried over from previous translation unit *)
    | INITIAL  (* no previous state *)


(* packages of functions to manipulate state implicitly *)

  type stateFuns =
    {globalState : globalState,
     localState : localState,
       (* the state records, included in case direct access to the
	* state is required (probably shouldn't be) *)

     locFuns :
      {pushLoc : SourceMap.location -> unit,
         (* push location onto location stack *)
       popLoc : unit -> unit,
         (* pop location stack *)
       getLoc : unit -> SourceMap.location,
         (* get top location from location stack *)
       error : string -> unit,
         (* report an error and its location *)
       warn : string -> unit},
         (* (if warnings are on) report a warning and its location *)

     tidsFuns :
      {pushTids : Tid.uid -> unit,
         (* records tids from new structs/unions/typdefs
          * introduced in declarations, casts, etc. *)
       resetTids : unit -> Tid.uid list},
         (* returns list of recently generated tids (since last resetTids call) *)

     tmpVarsFuns :
      {pushTmpVars : Ast.id -> unit,
         (* records pids for temporary introduced in decompilation of ++, --, +=, and their friends *)
       resetTmpVars : unit -> Ast.id list},
         (* returns list of recently generated pids (since last resetTmpVars call) *)

     envFuns :
      {topLevel : unit -> bool,
         (* are we at top level? *)
       pushLocalEnv : unit -> unit,
         (* push a fresh symbol table onto the stack *)
       popLocalEnv : unit -> unit,
         (* pop symbol table stack *)
       lookSym : Symbol.symbol -> Bindings.symBinding option,
         (* lookup type of a symbol in symbol table stack *)
       bindSym : Symbol.symbol * Bindings.symBinding -> unit,
         (* insert (i.e. bind) a symbol in the top (most local) symbol table *)
       lookSymGlobal : Symbol.symbol -> Bindings.symBinding option,
         (* lookup type of a symbol in the global symbol table *)
       bindSymGlobal : Symbol.symbol * Bindings.symBinding -> unit,
         (* insert (i.e. bind) a symbol in the global symbol table *)
       lookLocalScope : Symbol.symbol -> Bindings.symBinding option,
         (* look for a binding in the most local symbol table *)
       getGlobalEnv : unit -> symtab},
         (* return the global symbol table *)

     uidTabFuns :
      {bindAid : Ast.ctype -> Aid.uid,
         (* generate a new adornment identifier and bind it to the type *)
       lookAid : Aid.uid -> Ast.ctype option,
         (* lookup adornment identifier in state aidtab *)
       bindTid : Tid.uid * Bindings.tidBinding -> unit,
         (* insert a type identifier into the type symbol table *)
       lookTid : Tid.uid -> Bindings.tidBinding option},
         (* lookup a type identifier in the type symbol table *)

     funFuns :  (* manipulate current function context *)
      {newFunction : Ast.ctype -> unit,
         (* enter a new function context with the given return type *)
       getReturnTy : unit -> Ast.ctype option,
         (* get the return type of the current function context *)
       checkLabels : unit -> (Symbol.symbol * SourceMap.location) option,
         (* verify that all goto targets are defined as labels *)
       addLabel : Symbol.symbol * SourceMap.location -> Ast.label,
         (* define a label, returning an error flag if multiple defs *)
       addGoto : Symbol.symbol * SourceMap.location -> Ast.label},
         (* record a label as a goto target *)

     switchFuns : (* manipulate current switch context *)
      {pushSwitchLabels : unit -> unit, (* enter a switch statement *)
       popSwitchLabels : unit -> unit,  (* leave a switch statement *)
       addSwitchLabel : LargeInt.int -> string option,
         (* record a new switch label; returns SOME errormsg if duplicate
	  * or not within a switch *)
       addDefaultLabel : unit -> string option}}
         (* record a default label; returns SOME errormsg if multiple defaults,
	  * or not within a switch *)

  (* state initialization functions *)
  val initGlobal : (stateInfo * Error.errorState) -> globalState
  val initLocal : unit -> localState

  val stateFuns : globalState * localState -> stateFuns
      (* returns a collection of state functions specialized to
       * operate on the state passed as argument *)

end (* sigature STATE *)
