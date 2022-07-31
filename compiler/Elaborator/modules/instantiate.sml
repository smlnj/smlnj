(* Copyright 1996 by AT&T Bell Laboratories *)
(* instantiate.sml *)

(*
 * This? function constructs a dummy structure which satisfies all sharing
 * constraints (explicit or induced) of a given signature.  The resulting
 * structure is used as the dummy parameter of a functor while elaborating
 * and abstracting the functor body.
 *
 * The process of constructing the structure is essentially a unification
 * problem.  The algorithm used here is based on the Linear Unification
 * algorithm first presented in [1] which was subsequently corrected
 * and cleaned up in [2].
 *
 * The basic algorithm makes 2 passes.  The first pass builds a DAG in
 * a quasi-top down fashion which corresponds to the minimal structure
 * needed to match the signature.  The second pass takes the DAG and
 * constructs the actualy dummy structure in a bottom-up fashion.
 * Pass 1 has a fairly complicated control structure.  The major
 * invariant is that no node in the graph is expanded unless all
 * of its ancestors have been expanded.  This insures that all sharing
 * constraints (explicit or derived) have reached the node at the
 * time of its expansion.  The second major invariant is that no
 * node is finalized until all members in its equivalence class have
 * been found.
 *
 * [1] Paterson, M.S., and Wegman, M.N., "Linear Unification",
 *     J. Comp. Sys. Sci. 16,2 (April 1978), pp. 158-167.
 *
 * [2] de Champeaux, D., "About the Paterson-Wegman Linear Unification
 *     Algorithm", J. of Comp. Sys. Sci. 32, 1986, pp. 79-88.
 *)

signature INSTANTIATE =
sig

(*  structure Param : INSTANTIATE_PARAM *)

  (*** instantiation of functor parameter signatures ***)
  val instParam :
         {sign     : Modules.Signature,
          entEnv   : Modules.entityEnv,
          tdepth   : DebIndex.depth,	(* # of enclosing fct abstractions? *)
          rpath    : InvPath.path,
          region   : SourceMap.region,
          compInfo : ElabUtil.compInfo}
      -> {rlzn: Modules.strEntity,
          tycpaths: Types.tycpath list}

  (*** instantiation of formal functor body signatures ***)
  val instFmBody :
         {sign     : Modules.Signature,
          entEnv   : Modules.entityEnv,
          tycpath  : Types.tycpath,
          rpath    : InvPath.path,
          region   : SourceMap.region,
          compInfo : ElabUtil.compInfo}
      -> {rlzn: Modules.strEntity,
          abstycs: Types.tycon list,
          tyceps: EntPath.entPath list}

  (*** instantiation of structure abstractions (results of opaque sig ascriptions) ***)
  val instAbstr :
         {sign     : Modules.Signature,
          rlzn     : Modules.strEntity,  (* used for? see mod-elab-notes.txt *)
          entEnv   : Modules.entityEnv,
          rpath    : InvPath.path,
          region   : SourceMap.region,
          compInfo : ElabUtil.compInfo}
      -> {rlzn: Modules.strEntity,
          abstycs: Types.tycon list,     (* tycons made abstract? *)
          tyceps: EntPath.entPath list}  (* entity paths of tycons made abstract? *)

  (*** fetching the list of tycpaths for a particular structure ***)
  val getTycPaths :
         {sign     : Modules.Signature,
          rlzn     : Modules.strEntity,
          entEnv   : Modules.entityEnv,
          compInfo : ElabUtil.compInfo}
      -> Types.tycpath list

end (* signature INSTANTIATE *)

(* no longer functorized
(* functorized to factor out dependencies on FLINT... *)
functor InstantiateFn (Param: INSTANTIATE_PARAM) : INSTANTIATE =
	*)
structure Instantiate : INSTANTIATE =
struct

local structure A  = Access
      (* structure DI = DebIndex *)
      structure ED = ElabDebug
      structure EE = EntityEnv
      structure EM = ErrorMsg
      structure EP = EntPath
      structure EU = ElabUtil
      (* structure II = InlInfo *)
      structure IP = InvPath
      (* structure LT = PLambdaType *)
      structure M  = Modules
      structure MU = ModuleUtil
      structure PU = PrintUtil
      structure S  = Symbol
      structure SP = SymPath
      structure ST = Stamps
      structure T  = Types
      structure TU = TypesUtil
      structure SPL = SigPropList
      open Modules Types
in

(* structure Param = Param *)

(* ----------------------- utility functions ----------------------------- *)

(* debugging *)
val say = Control_Print.say
val debugging = ElabControl.insdebugging (* ref false *)
fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()
fun bug s = EM.impossible ("Instantiate: " ^ s)

fun wrap fname f arg =
    (debugmsg (">>> [INS]" ^ fname);
     f arg before (debugmsg ("<<< [INS]" ^ fname)))

fun debugType(msg: string, tyc: T.tycon) =
    ED.withInternals(fn () =>
     ED.debugPrint debugging
      (msg, PPType.ppTycon StaticEnv.empty, tyc))


(* error state *)
val error_found = ref false

val infinity = 1000000 (* a big integer *)

fun push(r,x) = (r := x::(!r))

fun pathName (path: IP.path) : string =
    SP.toString(ConvertPaths.invertIPath path)

val eqOrigin = MU.eqOrigin
val eqSig = MU.eqSign

fun sameStructure (STR { sign = sg1, rlzn = { stamp = s1, ... }, ... },
		   STR { sign = sg2, rlzn = { stamp = s2, ... }, ... }) =
    eqSig (sg1, sg2) andalso ST.eq (s1, s2)
  | sameStructure _ = false

fun signName (SIG { name, ... }) = getOpt (Option.map S.name name, "Anonymous")
  | signName ERRORsig = "ERRORsig"


(* -------------------- important data structures ------------------------ *)

(*
 * the different kinds of instantiations
 *)
datatype instKind
  = INST_ABSTR of M.strEntity
  | INST_FMBD of T.tycpath
  | INST_PARAM of DebIndex.depth

(* datatype stampInfo
 * encodes an instruction about how to get a stamp for a new entity
 *)
datatype stampInfo
  = STAMP of ST.stamp   (* here is the stamp *)
  | PATH of EP.entPath  (* get the stamp of the entity designated by the path *)
  | GENERATE            (* generate a new stamp (using the mkStamp parameter) *)

(* datatype entityInfo
 * The contents of the finalEnt field of the FinalStr inst variant.
 * Defined in finalize (in buildStrClass), used in instToStr to
 * determine how to find or build the realization entity.
 *
 * The bool argument of GENERATE_ENT is normally true when there was
 * a VARstrDef applying to the structure spec with a different signature
 * than the spec. This means that the spec signature should be considered
 * as open, despite what it's "closed" field might say.  This was introduced
 * to fix bug 1238.  [dbm, 8/13/97]
 *)
datatype entityInfo
  = CONST_ENT of M.strEntity  (* here it is *)
  | PATH_ENT of EP.entPath    (* find it via this entityPath *)
  | GENERATE_ENT of bool      (* generate a new one *)

datatype tycInst
  = INST of tycon      (* already instantiated *)
  | NOTINST of tycon   (* needing instantiation *)

(*
 * This datatype represents the continually changing DAG that is being
 * constructed by instantiate.  We start off with just an Initial node.
 * It is expanded into a Partial node whose children are
 * initialized to Initial nodes.  When all of the members of the nodes
 * equivalence class have been found, and converted to Partial nodes,
 * the node is converted to FinalStr.  Finally, we recurse on the children
 * of the node.
 *
 * Invariants:
 *
 *    The parent node is in a singleton equivalence class.
 *
 *    All nodes that are about to be explored are either Initial or Partial.
 *    (Exploring a Final node implies circularity.)
 *
 *    If a Final node's expanded field is true, then all of its children
 *    are Final with expanded field true.
 *)
datatype inst
    (* structure instances *)
  = FinalStr of   (* the equivalence class of this node is fully explored *)
     {sign : M.Signature,
      stamp : stampInfo ref,
      slotEnv : slotEnv,
      finalEnt: entityInfo ref,
      expanded : bool ref}
  | PartialStr of (* we are in the process of exploring the equiv. class *)
     {sign : M.Signature,
      path : IP.path,
      slotEnv : slotEnv,
      comps : (S.symbol * slot) list, (* sorted by symbol *)
      depth : int,
      final_rep : inst option ref}
  | InitialStr of (* haven't started exploring the equiv. class *)
     {sign : M.Signature,
      sigDepth : int,
      path : IP.path,
      epath : EP.entPath,
      slotEnv : slotEnv,
      inherited : constraint list ref}
  | NullStr
  | ErrorStr

    (* tycon instances *)
  | FinalTyc of tycInst ref
  | PartialTyc of
     {tycon : tycon,
      path : IP.path,
      epath: EP.entPath}
  | InitialTyc of
     {tycon : tycon,
      path : IP.path,
      epath: EP.entPath,
      inherited : constraint list ref}
  | NullTyc
  | ErrorTyc

    (* functor instances *)
  | FinalFct of
     {sign : M.fctSig,
      def : M.Functor option ref,
      path: IP.path,
      epath: EP.entPath}
  | NullFct

(*
 * A constraint is essentially a directed arc indicating that two
 * nodes are to be identified.  The constraint is always interpreted
 * relative to a structure inst node.  The my_path field is a symbolic
 * path (in regular order) indicating which subcomponent of the local
 * inst is participating in the sharing.  The other component is accessed
 * by first finding the inst node in the its_ancestor slot, and then following
 * the symbolic path its_path to the node.  By going through the
 * ancestor, we are able to insure that the ancestor is explored
 * before the actual component is, so that its inherited constraints are
 * propagated downward properly.
 *)
and constraint
  = SHARE of {my_path : SP.path,  (* regular symbolic path *)
	      its_ancestor : slot,
	      its_path : SP.path,  (* regular symbolic path *)
	      depth : int}  (* signature nesting depth of base constraint *)
  | SDEFINE of strDef * int (* int is signature nesting depth of defn *)
  | TDEFINE of tycInst * int (* int is signature nesting depth of defn *)

(* slot: a node in the graph (maybe "node" would be a better name?) *)
withtype slot = inst ref

(* slotEnv: association list mapping entVars to slots *)
and slotEnv = (EP.entVar * slot) list


(* debugging *)
fun instToString inst =
  (case inst
    of FinalStr{sign,stamp,slotEnv,finalEnt,expanded} =>
         "FinalStr(" ^ signName(sign) ^ ")"
     | PartialStr{sign,path,slotEnv,comps,depth,final_rep} =>
         "PartialStr(" ^ IP.toString path ^ ")"
     | InitialStr{sign,sigDepth,path,slotEnv,inherited,epath} =>
         "InitialStr(" ^ IP.toString path ^ ")"
     | FinalTyc(ref(INST tycon)) =>
	 "FinalTyc.INST(" ^ (S.name(TU.tycName tycon)) ^ ")"
     | FinalTyc(ref(NOTINST tycon)) =>
	 "FinalTyc.NOTINST(" ^ (S.name(TU.tycName tycon)) ^ ")"
     | PartialTyc{tycon,path,...} => "PartialTyc(" ^ IP.toString path ^ ")"
     | InitialTyc{tycon,path,...} =>
         "InitialTyc(" ^ IP.toString path ^ ")"
     | FinalFct{path, ...} => "FinalFct(" ^ IP.toString path ^ ")"
     | NullTyc => "NullTyc"
     | NullStr => "NullStr"
     | NullFct => "NullFct"
     | ErrorStr => "ErrorStr"
     | ErrorTyc => "ErrorTyc")

fun lookSlot((ev,slot)::rest,ev') =
      if EP.eqEntVar(ev,ev') then slot else lookSlot(rest,ev')
  | lookSlot(nil,_) = bug "lookSlot"

(*
 * Get slot for signature element (tycon or structure) ---
 * Lookup sym in sign, get entVar, lookup this entVar in slotEnv
 *)
fun getElemSlot(sym, SIG {elements,...}, slotEnv) : slot =
    (case MU.getSpecVar(MU.getSpec(elements,sym))
      of SOME v => lookSlot(slotEnv,v)
       | NONE => bug "getElemSlot (1)")
  | getElemSlot _ = bug "getElemSlot (2)"

fun getElemSlots(SIG {elements,...}, slotEnv) : (S.symbol * slot) list =
    let fun f (sym,spec) =
	    case MU.getSpecVar spec
	      of SOME v => SOME(sym,lookSlot(slotEnv,v))
	       | NONE => NONE
     in List.mapPartial f elements
    end
  | getElemSlots _ = bug "getElemSlots"

(* Retrieves all [formal] substructure components from a signature *)
fun getSubSigs (SIG {elements,...}) =
    List.mapPartial
	(fn (sym,STRspec{sign,entVar,...}) => SOME(sym,entVar,sign)
	  | _ => NONE) elements
  | getSubSigs _ = []


(* translate a tycon to a tycInst *)
fun extTycToTycInst tyc =
    case tyc
      of (T.DEFtyc _ | T.PATHtyc _) => NOTINST tyc
	 (* may need instantiation -- could check
	  * first whether body of DEFtyc contains any
	  * PATHtycs -- see bug 1200. *)
       | _ => INST tyc
         (* GENtyc -- won't need instantiation *)

(* getElemDefs : strDef * (unit -> stamp) * int -> (S.symbol * constraint) list
 *   returns the definition constraints for components of a strDef,
 *   sorted by the component name in ascending order
 *)
fun getElemDefs (strDef,mkStamp,depth): (S.symbol * constraint) list =
    let val comps =
	     (case strDef
	       of CONSTstrDef (STR {sign = SIG {elements,...},
				    rlzn as {entities,...}, ... }) =>
		  List.mapPartial
		   (fn (sym,STRspec{sign,entVar,def,slot}) =>
		       (debugmsg (">>getElemDefs.C: STRspec " ^
				  Symbol.name sym);
			SOME(sym,SDEFINE(CONSTstrDef(
					  STR{sign=sign,
					      rlzn=EE.lookStrEnt(entities,
								 entVar),
					      access=A.nullAcc,
					      prim=[]}),
					 depth))
			before debugmsg ("<<getElemDefs.C: STRspec " ^
					 Symbol.name sym))
		     | (sym,(TYCspec{entVar,...})) =>
		       (debugmsg (">>getElemDefs.C: TYCspec " ^
				  Symbol.name sym);
			let val tyc' = EE.lookTycEnt(entities,entVar)
			    val tycInst = extTycToTycInst tyc'
			 in debugType("#getElemDefs:TYCspec",tyc');
			    SOME(sym,TDEFINE(tycInst,depth))
			end)
		     | _ => NONE)
		   elements
		| VARstrDef(SIG {elements,...},entPath) =>
		   List.mapPartial
		   (fn (sym,STRspec{sign,entVar,def,slot}) =>
		       (debugmsg (">>getElemDefs.V: STRspec " ^ Symbol.name sym
				  ^", entPath: "^EP.entPathToString entPath
				  ^", entVar: "^EP.entVarToString entVar);
			SOME(sym,SDEFINE(VARstrDef(sign,entPath@[entVar]),depth)))
		     | (sym,TYCspec{entVar,
                                    info=RegTycSpec{spec=tyc,repl,scope}}) =>
		       (debugmsg (">>getElemDefs.V: TYCspec(Reg) " ^ Symbol.name sym
				  ^", entPath: "^EP.entPathToString entPath
				  ^", entVar: "^EP.entVarToString entVar);
			SOME(sym,TDEFINE(NOTINST(
					   PATHtyc{arity=TU.tyconArity tyc,
						   entPath=entPath@[entVar],
						   path=valOf(TU.tycPath tyc)}),
					 depth)))
		     | (sym,TYCspec{entVar,info=InfTycSpec{name,arity}}) =>
		       (debugmsg (">>getElemDefs.V: TYCspec(Inf) " ^ Symbol.name sym
				  ^", entPath: "^EP.entPathToString entPath
				  ^", entVar: "^EP.entVarToString entVar);
			SOME(sym,TDEFINE(NOTINST(
					   PATHtyc{arity=arity,
						   entPath=entPath@[entVar],
						   path=IP.extend(IP.empty,name)}),
					 depth)))
		     | _ => NONE)
		   elements
		| CONSTstrDef ERRORstr => nil
		| _ => bug "getElemDefs")
      in ListMergeSort.sort(fn((s1,_),(s2,_)) => S.symbolGt(s1,s2)) comps
     end


(* mkElemSlots: Signature * slotEnv * IP.path * entityPath * int
 *              -> slotEnv * (S.symbol * slot) list
 *
 *   create slots with initial insts for the components of the signature
 *   for a structure spec.  slots are associated with element names and
 *   sorted in ascending order by element name.  the slots are also
 *   added to the inherited slotEnv, bound the corresponding element's
 *   entityVar, and the augmented slotEnv is returned
 *)
fun mkElemSlots(SIG {elements,...},slotEnv,rpath,epath,sigDepth) =
    let fun mkSlot((sym,STRspec{sign as SIG {closed,...},
				entVar,def,...}),slotEnv) =
	     (* a definitional structure spec is translated into a SDEFINE
	      * constraint *)
	     let val constraints =
		     case def
		       of NONE => []
			| SOME(strDef,scope) => [SDEFINE(strDef,sigDepth-scope)]
	      in SOME (entVar, ref(InitialStr{sign=sign,
					      sigDepth=sigDepth,
					      path=IP.extend(rpath,sym),
					      slotEnv=(if closed then nil
						       else slotEnv),
					      epath=epath@[entVar],
					      inherited=ref constraints}))
	     end
	  | mkSlot((sym,STRspec{sign as ERRORsig,entVar,...}),slotEnv) =
	     SOME (entVar, ref(ErrorStr))
	  | mkSlot((sym,TYCspec{entVar,info=RegTycSpec{spec=tycon,repl,scope}}),
                   slotEnv) =
	     (case tycon
		of DEFtyc{stamp,path,tyfun=TYFUN{arity,...},...} =>
		    (* translate a DEFtyc spec into a TDEFINE constraint *)
		    let val tycon' = GENtyc{stamp=stamp,arity=arity,path=path,
					    eq=ref(IND),kind=FORMAL,
					    stub = NONE}
		     in SOME(entVar,
			     ref(InitialTyc
				  {tycon=tycon',
				   path=IP.extend(rpath,sym),
				   epath=epath@[entVar],
				   inherited=ref[TDEFINE(NOTINST tycon,
							 sigDepth-scope)]}))
		    end
		 | _ =>
		   SOME(entVar,
			ref(InitialTyc{tycon=tycon,
				       path=IP.extend(rpath,sym),
				       epath=epath@[entVar],
				       inherited=ref []})))
	  | mkSlot((sym,FCTspec{sign,entVar,...}),slotEnv) =
	     SOME (entVar,ref(FinalFct{sign=sign, def=ref NONE,
				       epath=epath@[entVar],
				       path=IP.extend(rpath,sym)}))
	  | mkSlot _ = NONE  (* value element *)

	fun mkSlots(nil,slotEnv,slots) =
	    (slotEnv, ListMergeSort.sort(fn((s1,_),(s2,_)) => S.symbolGt(s1,s2)) slots)
	  | mkSlots((element as (sym,_))::rest,slotEnv,slots) =
	      (case mkSlot(element,slotEnv)
		of SOME(binder as (_,slot)) =>
		    mkSlots(rest, binder::slotEnv, (sym,slot)::slots)
		 | NONE => mkSlots(rest, slotEnv, slots))

     in mkSlots(elements,slotEnv,nil)
    end
 | mkElemSlots _ = bug "mkElemSlots"

(*
  val getSubSigs = wrap "getSubSigs" getSubSigs
  val getElemDefs = wrap "getElemDefs" getElemDefs
  val mkElemSlots = wrap "mkElemSlots" mkElemSlots
*)

(* propDefs : (symbol * slot) list * (symbol * constraint) list -> unit
 *
 *   Propogate definition constraints down to the components of a
 *   structure node that has a definition constraint.  Called only
 *   in constrain in buildStrClass, i.e. when propogating constraints
 *   to children of a node.
 *
 * NOTE: does not check that each element in the first list has
 * an associated constraint in the second list.
 *
 * ASSERT: both arguments of propDefs are sorted in assending order by the
 * symbol component (the arguments are supplied by mkElementSlots and
 * getElemDefs, respectively).
 *
 * ASSERT: all constraints in the second argument are SDEFINE or TDEFINE,
 * as appropriate.
 *)
fun propDefs(nil,_) = ()
  | propDefs(_,nil) = ()
  | propDefs(a1 as (sym1,sl)::rest1, a2 as (sym2,def)::rest2) =
      if S.symbolGt(sym1,sym2) then propDefs(a1,rest2)
      else if S.symbolGt(sym2,sym1) then propDefs(rest1,a2)
      else (case !sl
	      of InitialStr {inherited, ...} => push(inherited, def)
	       | InitialTyc {inherited, ...} => push(inherited, def)
	       | ErrorStr => (error_found := true)
	       | ErrorTyc => ()
	       | _ => bug "propDefs";
	    propDefs(rest1,rest2))


(* propSharing : (S.symbol * slot) list * (S.symbol * slot) list -> unit
 *
 *   Propogates inherited sharing constraints (SHARE) to the matching
 *   elements of two structure nodes.  Called only in addInst in
 *   buildStrClass, i.e. when adding a new instance to an equiv. class.
 *
 * ASSERT: both arguments of propSharing are sorted in assending order by the
 * symbol component.
 *
 * ASSERT: matching slots are either both InitialStr, both InitialTyc,
 * or one is ErrorStr or ErrorTyc.
 *)
fun propSharing(nil,_,_) = ()
  | propSharing(_,nil,_) = ()
  | propSharing(a1 as (sym1,slot1)::rest1,
		a2 as (sym2,slot2)::rest2,
		depth) =
    if S.symbolGt(sym1,sym2) then propSharing(a1,rest2,depth)
    else if S.symbolGt(sym2,sym1) then propSharing(rest1,a2,depth)
    else (case (!slot1, !slot2)
	    of (InitialStr {inherited=inherited1, ...},
		InitialStr {inherited=inherited2, ...}) =>
		(push(inherited1,
		  SHARE{my_path=SP.empty,its_ancestor=slot2,
			its_path=SP.empty,depth=depth});
		 push(inherited2,
		  SHARE{my_path=SP.empty,its_ancestor=slot1,
			its_path=SP.empty,depth=depth}))
	     | (InitialTyc {inherited=inherited1, ...},
		InitialTyc {inherited=inherited2, ...}) =>
		(push(inherited1,
		  SHARE{my_path=SP.empty,its_ancestor=slot2,
			its_path=SP.empty,depth=depth});
		 push(inherited2,
		  SHARE{my_path=SP.empty,its_ancestor=slot1,
			its_path=SP.empty,depth=depth}))
	     | (ErrorStr,_) => ()
	     | (_,ErrorStr) => ()
	     | (ErrorTyc,_) => ()
	     | (_,ErrorTyc) => ()
	     | _ => bug "propSharing";
	  propSharing(rest1,rest2,depth))


(* debugging wrapper
val propSharing = wrap "propSharing" propSharing
*)

(**************************************************************************
 * distributeS : Signature * slotEnv * entEnv * int -> unit               *
 *                                                                        *
 * This function distributes the structure sharing constraints of a       *
 * signature to the children of a corresponding node.  Note that this     *
 * only deals with the explicit constraints.  Implied and inherited       *
 * constraints are propogated by propSharing and the constrain functions  *
 * of buildStrClass and buildTycClass.                                    *
 **************************************************************************)
exception DistributeS

fun distributeS (sign as SIG {strsharing,...}, slotEnv, entEnv, sigDepth) =
     let fun stepPath (SP.SPATH(sym::path)) =
              let val slot = getElemSlot(sym,sign,slotEnv)
               in case !slot
                   of InitialStr{inherited, ...} =>
                         (SP.SPATH path, inherited, slot)
                    | ErrorStr => raise DistributeS
                    | _ => bug "distributeS.stepPath 1"
              end
           | stepPath (SP.SPATH []) = bug "distributeS.stepPath 2"

         fun distShare (p::rest) =
              let val (p1, h1, slot1) = stepPath p
                  fun addConstraints (p2, h2, slot2) =
                    (push(h1,SHARE{my_path=p1, its_path=p2, its_ancestor=slot2,
				   depth=sigDepth});
                     push(h2,SHARE{my_path=p2, its_path=p1, its_ancestor=slot1,
				   depth=sigDepth}))
               in app (fn p' => addConstraints (stepPath p')) rest
              end
           | distShare [] = ()

      in (app distShare strsharing) handle DistributeS => ()
     end
  | distributeS _ = ()


(****************************************************************************
 * distributeT : Signature * slotEnv * entEnv * (unit->stamp) * int -> unit *
 *                                                                          *
 * This function distributes the type sharing constraints that a signature  *
 * has to the children of the corresponding node.                           *
 ****************************************************************************)
exception DistributeT

fun distributeT (sign as SIG {typsharing,...},
		 slotEnv, entEnv, mkStamp, sigDepth) =
     let fun stepPath(SP.SPATH[sym]) =
               let val slot = getElemSlot(sym,sign,slotEnv)
                in case !slot
                    of InitialTyc {inherited, ...} =>
                        (SP.SPATH[], inherited, slot)
                     | ErrorTyc => raise DistributeT
                     | _ => bug "distributeT:stepPath 1"
               end
           | stepPath(SP.SPATH(sym::path)) =
               let val slot = getElemSlot(sym,sign,slotEnv)
                in case !slot
                    of InitialStr {inherited, ...} =>
                        (SP.SPATH path, inherited, slot)
                     | ErrorStr => raise DistributeT
                     | _ => bug "distributeT:stepPath 2"
               end
           | stepPath _ = bug "distributeT:stepPath 3"

         fun distShare (p::rest) =
              let val (p1,h1,slot1) = stepPath p
		      (* stepPath might raise MU.Unbound if there were errors
		         in the signature (testing/modules/tests/101.sml) *)
                  fun g (p2, h2, slot2) =
                    (push(h1,SHARE{my_path=p1, its_path=p2, its_ancestor=slot2,
				   depth=sigDepth});
                     push(h2,SHARE{my_path=p2, its_path=p1, its_ancestor=slot1,
				   depth=sigDepth}))
               in app (fn p' => g (stepPath p')) rest
              end
           | distShare [] = ()

      in (app distShare typsharing) handle DistributeT => ()
     end
  | distributeT _ = ()

(* debugging wrappers
val distributeS = wrap "distributeS" distributeS
val distributeT = wrap "distributeT" distributeT
*)

exception ExploreInst of IP.path


(* THIS COMMENT OBSOLETE *)
(***************************************************************************
 * buildStrClass : slot * int * entityEnv * (unit -> stamp) * EM.complainer
 *                  -> unit
 *
 * The slot argument is assumed to contain an InitialStr.
 * This function computes the equivalence class of the structure
 * element associated with the slot.  It proceeds as follows:
 *
 * 1. New slots are created for the elements of the signature.
 *
 * 2. The InitialStr is replaced by a PartialStr.
 *
 * 3. The signature's explicit type and structure sharing constraints are
 *    propogated to the member elements using distributeS and distributeT.
 *
 * 4. This node's inherited constraints are processed.  If they apply
 *    to this node, the equivalence class is enlarged (using addInst) or
 *    a definition is set (classDef).  If a constraint applies to children
 *    of this node, they are propogated to the children.  Processing a
 *    sharing constraint may require that an ancestor of the other node
 *    in the constraint first be explored by buildStrClass.
 *    Once constrain is complete, class contains a list of equivalent PartialStr
 *    nodes that constitute the sharing equivalence class of the original
 *    node (thisSlot).
 *
 * 5. finalize is applied to the members of the class to turn them
 *    into FinalStrs.  The FinalStrs are memoized in the PartialStr
 *    nodes to insure that equivalent nodes that have the same signature
 *    will contain the same FinalStr value.
 *
 * If two slots in the class have nodes that share the same signature,
 * then the slots are made to point to only one of the nodes.  Of course,
 * the sharing constraints for both must be propogated to the descendants.
 *
 * Also, the "entEnv" argument here is strictly used for interpreting the
 * sharing constraints only. (ZHONG)
 ***************************************************************************)

(* ASSERT: this_slot is an InitialStr *)
fun buildStrClass (this_slot: slot, classDepth: int,
                   entEnv: M.entityEnv, mkStamp, err: EM.complainer) : unit =
let val class = ref ([this_slot] : slot list) (* the equivalence class *)
    val classDef = ref (NONE : (strDef * int) option)
    val minDepth = ref infinity
	(* minimum signature nesting depth of the sharing constraints used
	 * in the construction of the equivalence class. *)

    (* for error messages *)
    val this_path =
	case !this_slot
	  of InitialStr{path,...} => ConvertPaths.invertIPath path
	   | _ => bug "buildStrClass: this_slot not InitialTyc"

    (* addInst(old,new,depth);
     * (1) Adds new to the current equivalence class in response to a sharing
     * constraint relating old to new.
     * (2) Converts the new node from InitialStr to PartialStr.  Propagates sharing
     * to the respective common components.  Propagate downward the sharing
     * constraints in new's signature. Then apply constrain to each of the
     * inherited constraints.
     * depth is the signature nesting depth of this sharing constraint.  *)
    fun addInst (old: slot, new: slot, depth: int) : unit =
       (minDepth := Int.min(!minDepth, depth);
	case !new
	  of ErrorStr => ()
	   | (PartialStr {depth, path, ...}) =>
		if (depth = classDepth) then () (* member current class *)
		else raise ExploreInst path (* member of pending class *)
	   | (InitialStr {sign, sigDepth, path, slotEnv, inherited, epath}) =>
	       (case !old
		  of (p as (PartialStr{sign=sign',
				       slotEnv=slotEnv',
				       comps=oldComps,...})) =>
		       if eqSig(sign,sign') then  (* same sign *)
			  (new := p;   (* share the old instance *)
			   push(class,new);  (* add new slot to class *)
			   constrain(new,!inherited,sign,slotEnv',path))
			     (* may be new inherited constraints *)
		       else (* different sign *)
			(let val sigDepth' = sigDepth + 1
			     val (slotEnv',newComps) =
				 mkElemSlots(sign,slotEnv,path,epath,sigDepth')
			  in new := PartialStr{sign=sign,path=path,
					       slotEnv=slotEnv',
					       comps=newComps,
					       final_rep = ref NONE,
					       depth=classDepth};
			     push(class,new);
			     propSharing(oldComps,newComps,depth);
			     distributeS (sign, slotEnv', entEnv, sigDepth');
			     distributeT (sign, slotEnv', entEnv, mkStamp,
					  sigDepth');
			     constrain (new, !inherited, sign, slotEnv', path)
			  end
			  handle (MU.Unbound _) =>  (* bad sharing paths *)
			      (error_found := true;
			       new := ErrorStr))
		   | ErrorStr => ()
		     (* could do more in this case  -- all the above
		      * except for propSharing *)
		   | _ => bug "addInst 1")
	   | _ => if !error_found then new := ErrorStr
		  else bug "addInst.2")

    and constrain (oldSlot, inherited, sign, slotEnv, path) =
	(* Class shares with some external structure *)
	let fun constrain1 constraint =
            case constraint
              of (SDEFINE(strDef,depth)) =>
		 (debugmsg "constrain: SDEFINE";
		  case !classDef
		   of SOME _ =>
		       (* already defined -- ignore secondary definitions *)
		       if !ElabControl.multDefWarn then
			 err EM.WARN
			   ("multiple defs at structure spec: "
			    ^ SP.toString(ConvertPaths.invertIPath path)
			    ^ "\n    (secondary definitions ignored)")
			   EM.nullErrorBody
		       else ()
		    | NONE =>
		       let val comps = case !oldSlot of
					   PartialStr x => #comps x
					 | _ => bug "constrain:PartialStr"
			in classDef := SOME(strDef,depth);
			   propDefs (comps,getElemDefs(strDef,mkStamp,depth))
		       end)

	       (* Class shares with the structure in slot -- explore it *)
	       | SHARE{my_path=SP.SPATH[],its_ancestor=newSlot,
		       its_path=SP.SPATH [], depth} =>
		 (debugmsg "<calling addInst to add member to this equiv class>";
		  addInst(oldSlot,newSlot,depth)
		  handle (ExploreInst path') =>
		    (err EM.COMPLAIN
		       "sharing structure with a descendent substructure"
		       EM.nullErrorBody;
		     newSlot := ErrorStr))

	       (* Class shares with another structure.  Make sure its ancestor
		* has been explored.  Then push the constraint down a level.
		*)
	       | SHARE{my_path=SP.SPATH[],its_ancestor=slot,
		       its_path=SP.SPATH(sym::rest),depth} =>
		 (case (!slot)
		   of InitialStr _ =>
			(debugmsg "<Having to call buildStrClass on an ancestor \
				   \of a node I'm equivalent to.>";
			 buildStrClass (slot, (classDepth+1), entEnv,
					mkStamp, err)
			 handle (ExploreInst _) => bug "buildStrClass.4")
		    | ErrorStr => ()
		    | _ => ();

		  debugmsg "<finished exploring his ancestor>";

		  case (!slot)
		   of FinalStr {sign=sign', slotEnv=slotEnv', ...} =>
			(debugmsg "<calling constrain recursively>";
			 constrain (oldSlot,
			   [SHARE{my_path=SP.SPATH[], its_path=SP.SPATH rest,
				  its_ancestor=getElemSlot(sym,sign',slotEnv'),
				  depth=depth}],
                           sign, slotEnv, path))
		    | PartialStr _ =>  (* do we need to check depth? *)
			(err EM.COMPLAIN
			 "Sharing structure with a descendent substructure"
			 EM.nullErrorBody;
			 slot := ErrorStr)
		    | ErrorStr => ()
		    | _ => bug "buildStrClass.5")

	       (* One of the node's children shares with someone.  Push the
		* constraint down to the child now that we are explored.
		*)
	       | SHARE{my_path=SP.SPATH(sym::rest),
		       its_ancestor, its_path, depth} =>
		 let val { elements, ... } =
			 case sign of SIG s => s
				    | _ => bug "instantiate:constrain:SIG"
		  in case MU.getSpec(elements,sym)
		       of TYCspec{entVar,info=RegTycSpec{spec=tycon,repl,scope}} =>
			   (* ASSERT: rest = nil *)
			   (case !(lookSlot(slotEnv,entVar))
			     of InitialTyc {inherited, ...} =>
				  push(inherited,
				       SHARE{my_path=SP.SPATH[],
					     its_ancestor=its_ancestor,
					     its_path=its_path,depth=depth})
			      | _ => bug "buildStrClass.6")
			| STRspec{entVar,...} =>
			   (case !(lookSlot(slotEnv,entVar))
			     of InitialStr {inherited, ...} =>
				  push(inherited,
				       SHARE{my_path=SP.SPATH rest,
					     its_ancestor=its_ancestor,
					     its_path=its_path,depth=depth})
			      | _ => bug "buildStrClass.7")
			| _ => bug "buildStrClass.8"
		 end
	       | _ => bug "buildStrClass.9"
	 in app constrain1 (rev inherited)
	end

    (*
     * Converts all of the nodes in the class (which should be PartialStr)
     * to Final nodes.  Note that nodes which share the same signature
     * should share the same FinalStr nodes.  So, they are memoized using
     * the final_rep field of the PartialStr node.
     *)
    fun finalize (stampInfoRef: stampInfo ref) slot =
	case (!slot)
	 of ErrorStr => ()
	  | PartialStr {sign, path, slotEnv, final_rep, ...} =>
	      (case !final_rep
		of SOME inst => slot := inst
		 | NONE =>
		    let val finalEnt =
			    case !classDef
			      of SOME(CONSTstrDef(STR{sign=sign',
						      rlzn, ... }),_) =>
				 if eqSig(sign,sign') then CONST_ENT rlzn
				 else GENERATE_ENT true
			       | SOME(VARstrDef(sign',entPath),_) =>
				  (* if eqSig(sign,sign') then PATH_ENT(entPath)
				   * else ...
				   * DBM: removed to fix bug 1445. Even when
				   * the signatures are equal, a free entvar
				   * reverence can be propogated by the structure
				   * declaration.  See bug1445.1.sml. *)
				  GENERATE_ENT false
			       | SOME(CONSTstrDef(ERRORstr),_) =>
				  CONST_ENT bogusStrEntity
			       | NONE => GENERATE_ENT true
			       | _ => bug "buildStrClass.finalize 1"
			val inst =
			    FinalStr {sign = sign,
				      stamp = stampInfoRef,
				      slotEnv = slotEnv,
				      finalEnt = ref finalEnt,
				      expanded = ref false}
		     in final_rep := SOME inst;  (* memoize *)
			slot := inst
		    end)
	  | _ => bug "buildStrClass.finalize 2"

    (* Should find everyone in the equiv. class and convert them to
     * PartialStr nodes.
     *)
 in (* explore equivalence class, filling the class ref with a
     * list of PartialStr insts *)
    case !this_slot  (* verify that this_slot is InitialStr *)
      of (InitialStr {sign, sigDepth, path, slotEnv, inherited, epath}) =>
	  (let val sigDepth' = sigDepth + 1
	       val (slotEnv',newComps) =
		   mkElemSlots(sign,slotEnv,path,epath,sigDepth')
	    in this_slot :=
		 PartialStr{sign=sign,path=path,
			    slotEnv=slotEnv',
			    comps=newComps,
			    final_rep = ref NONE,
			    depth=classDepth};
	       distributeS (sign, slotEnv', entEnv, sigDepth');
	       distributeT (sign, slotEnv', entEnv, mkStamp, sigDepth');
	       constrain (this_slot, !inherited, sign, slotEnv', path)
	   end
	   handle (MU.Unbound _) =>  (* bad sharing paths *)
	       (error_found := true;
		this_slot := ErrorStr))

       | _ => bug "buildStrClass.10"; (* not InitialStr *)

 (* BUG: needs fixing. DBM *)
    (* verify that any class definition is defined outside of the
     * outermost sharing constraint *)
    case !classDef
      of NONE => () (* no definition - ok *)
       | SOME(_,depth) =>
	  if !minDepth <= depth
	  then (if !ElabControl.shareDefError
		then classDef := SOME(CONSTstrDef ERRORstr,0)
		else ();
		err (if !ElabControl.shareDefError
		    then EM.COMPLAIN
		    else EM.WARN)
		   ("structure definition spec inside of sharing at: "^
		    SymPath.toString this_path)
		   EM.nullErrorBody)
	  else ();

    let val classStampInfo =
	ref(case !classDef
	      of SOME(CONSTstrDef str,_)  => STAMP(MU.getStrStamp str)
	       | SOME(VARstrDef(_,entPath),_) => PATH(entPath)
	       | NONE => GENERATE)

     in app (finalize classStampInfo) (!class)
    end
end (* buildStrClass *)

(* debugging wrappers
val buildStrClass = wrap "buildStrClass" buildStrClass
*)

exception INCONSISTENT_EQ
  (* raised if tycons with both YES and NO eqprops are found in an
   * equivalence class *)

(*************************************************************************
 * buildTycClass: int * slot * entityEnv * instKind * rpath * (unit->stamp)
 *                EM.complainer -> unit
 *
 * This function deals with exploration of type nodes in the instance
 * graph.  It is similar to the buildStrClass function above, but it is
 * simpler since it doesn't have to worry about "children" of
 * type nodes.  However, we must check that the arities of equivalenced
 * types are the same.  Also, if they have constructors, we must check
 * to see that they have the same constructor names.  We don't know how
 * to check that the types of the constructors are satisfiable -- this
 * involves a limited form of second-order unification.
 *
 * But then, probably we should only allow two datatypes to be shared if their
 * types are completely equivalent; otherwise, the behavior of the elaboration
 * would be rather odd sometimes. (ZHONG)
 *
 * Also, the "entEnv" argument here is strictly used for interpreting the
 * sharing constraints only. (ZHONG)
 *
 *************************************************************************)

(* ASSERT: this_slot is an InitialTycon *)
fun buildTycClass (cnt:int, this_slot, entEnv, instKind, rpath, mkStamp: unit -> ST.stamp, err) =
  let val class = ref ([] : slot list)
      val classDef = ref (NONE : (tycInst * int) option)
      val minDepth = ref infinity
	(* minimum signature nesting depth of the sharing constraints used
	 * in the construction of the equivalence class. *)

      (* for error messages *)
      val this_path =
	  case !this_slot
	    of InitialTyc{path,...} => ConvertPaths.invertIPath path
	     | _ => bug "buildTycClass: this_slot not InitialTyc"

      val newTycKind =
        case instKind
         of INST_ABSTR {entities,...} =>
	      (fn (ep,_) => T.ABSTRACT(EE.lookTycEP (entities, ep)))
          | INST_PARAM tdepth =>
              (fn (ep,tk) =>
                  T.FLEXTYC(T.TP_VAR ({tdepth=tdepth, num=cnt, kind=tk})))
          | INST_FMBD tp => (fn (ep,_) => T.FLEXTYC(T.TP_SEL(tp,cnt)))

      fun addInst (slot, depth)=
	  let val _ = debugmsg ">>> [INS]addInst"
	   in minDepth := Int.min(!minDepth, depth);
	      case !slot
		of InitialTyc {tycon, path, epath, inherited} =>
		      (debugmsg "=== [INS]addInst: setting InitialTyc to PartialTyc";
		       slot := PartialTyc{tycon=tycon, path=path, epath=epath};
		       push(class,slot);
		       app constrain (rev(!inherited)))
		 | PartialTyc _ => ()
		 | ErrorTyc => ()
		 | _ => bug "buildTycClass.addInst"
	  end before (debugmsg "<<< [INS]addInst")

      and constrain def =
          let val _ = debugmsg ">>> [INS]constrain"
	      fun constrain1 def =
		  case def
		   of TDEFINE(d as (tycInst2,depth)) =>
		    (case !classDef
		      of SOME _ =>
			  (* already defined -- ignore secondary definitions *)
			  if !ElabControl.multDefWarn then
			   err EM.WARN
			     ("multiple defs at tycon spec: "
			      ^ SP.toString(ConvertPaths.invertIPath rpath)
			      ^ "\n    (secondary definitions ignored)")
			     EM.nullErrorBody
			  else ()
		       | NONE => classDef := SOME d)

		     | SHARE {my_path=SP.SPATH[], its_ancestor=slot,
			     its_path=SP.SPATH[], depth} =>
		         addInst (slot, depth)

		     | SHARE {my_path=SP.SPATH[], its_ancestor=slot,
			      its_path=SP.SPATH(sym::rest), depth} =>
			 (case !slot
			   of InitialStr _ =>
			       (buildStrClass (slot, 0, entEnv, mkStamp, err)
				handle ExploreInst _ => bug "buildTycClass.2")
			    | _ => ();
			  case !slot
			   of FinalStr{sign, slotEnv, ...} =>
			      constrain1 (SHARE{my_path=SP.SPATH[], its_path=SP.SPATH rest,
					       its_ancestor=getElemSlot(sym,sign,slotEnv),
					       depth=depth})
			    | ErrorStr => ()
			    | _ => bug "buildTycClass.3")

		     | _ => bug "buildTycClass:constrain1: bad def"
	   in constrain1 def
	      before (debugmsg "<<< [INS]constrain")
	  end

      fun checkArity (ar1, ar2, path1: IP.path, path2: IP.path) =
           if ar1 = ar2 then true
           else (err EM.COMPLAIN
                   ("inconsistent arities in type sharing "
                    ^(pathName path1)^" = "^(pathName path2)^" : "
                    ^(pathName path1)^" has arity "^(Int.toString ar1)^" and "
                    ^(pathName path2)^" has arity "^(Int.toString ar2)^".")
                   EM.nullErrorBody;
                 false)

      val sortD = ListMergeSort.sort
           (fn ({name=name1,rep=_,domain=_},{name=name2,rep=_,domain=_}) =>
                 S.symbolGt(name1,name2))

      fun eqDataCons({name=name1,rep=_,domain=_},{name=name2,rep=_,domain=_}) =
            S.eq(name1,name2)

      fun compareD ([], []) = true
        | compareD (d1::r1, d2::r2) =
            eqDataCons(d1,d2) andalso compareD (r1,r2)
        | compareD _ = false

      (* Eta-reduce type abbreviation tycons. Makes sure that DEFtyc is not
       * just an eta-expansion of another tycon.
       *)
      fun simplify(tyc0 as DEFtyc{tyfun=TYFUN{arity,body},...}) =
            (case body
              of CONty(RECORDtyc _,args) => tyc0
               | CONty(tyc,args) =>
                    let fun isvars(IBOUND n ::rest,m) =
                               if n = m then isvars(rest,m+1) else false
                           | isvars (nil,_) = true
			   | isvars _ = bug "simplify:isvars"
                     in if length args = arity andalso
                                  isvars(map TU.prune args,0)
                        then simplify tyc else tyc0
                    end
               | _ => tyc0)
        | simplify tyc = tyc

      (*
       * Potential BUG on equality properties: when selecting the
       * candidate from a set of FORMAL tycons, the equality property
       * should be merged ... but this is not done right now (ZHONG)
       *)

      fun eqMax((NO, OBJ) |(NO, YES) | (YES, NO) | (OBJ, NO)) =
	    raise INCONSISTENT_EQ
        | eqMax(_, YES) = YES
        | eqMax(_, OBJ) = YES
        | eqMax(ep, _) = ep

      (* scanForRep scans the tycons in the class, selecting a representative
       * according to the following rule:
          * if there is a datatype in the class, select the first one
          * otherwise, if there is a DEFtyc, select last of these
            (this case should go away in SML96)
	  * otherwise, all the tycons are FORMAL, select last of these
       * creates a representative tycon for the class, giving
       * it a new stamp if it is a datatype or formal. *)
      fun scanForRep tyc_eps =
	  let fun loop(ERRORtyc,epath,arity,eqprop,(tyc,ep)::rest) =
	            (* initialization *)
		    (case tyc
		      of GENtyc { arity, eq, ... } =>
			 loop(tyc,ep,arity,!eq,rest)
		       | ERRORtyc =>
			  loop(tyc,ep,0,IND,rest)
		       | DEFtyc{tyfun=TYFUN{arity,...},path,...} =>
			  bug "scanForRep 0"
		       | _ => bug "scanForRep 1")

		| loop(tyc as GENtyc { kind, path = path, ... },
		       epath, arity, eqprop, (tyc', epath') :: rest) =
		  (case kind of
		       DATATYPE _ =>
		       (case tyc'
			 of GENtyc {kind,arity=arity',eq,path=path',...} =>
			    (checkArity(arity,arity',path,path');
			     loop(tyc,epath,arity,eqMax(eqprop,!eq),rest))
			  | ERRORtyc => loop(tyc,epath,arity,eqprop,rest)
			  | DEFtyc{tyfun=TYFUN{arity=arity',...},
				   path=path',...} =>
			    bug "scanForRep 2"
			  | _ => bug "scanForRep 2.1")

		     | FORMAL =>
		       (case tyc'
			 of GENtyc {kind,arity=arity',eq,path=path',...} =>
			    (checkArity(arity,arity',path,path');
			     case kind
			      of DATATYPE _ =>
				 loop(tyc',epath',arity,
				      eqMax(eqprop,!eq),rest)
			       | _ => loop(tyc,epath,arity,
					   eqMax(eqprop,!eq),rest))
			  | ERRORtyc => loop(tyc,epath,arity,eqprop,rest)
			  | DEFtyc{tyfun=TYFUN{arity=arity',...},
				   path=path',...} =>
			    bug "scanForRep 3"
			  | _ => bug "scanForRep 3.1")
		     | _ => bug "scanForRep 8")
		| loop(tyc,epath,arity,eprop,nil) = (tyc,epath,eprop)

		| loop _ = bug "scanForRep 4"

	      val (reptyc,epath,eqprop) =
		  case tyc_eps
		    of [(tyc,epath)] =>
			let val eqprop =
			    case tyc
			      of GENtyc {eq, ...} => !eq
			       | DEFtyc{tyfun=TYFUN{arity,...},...} => IND
			       | ERRORtyc => IND
			       | _ => bug "scanForRep 5"
			 in (tyc,epath,eqprop)
			end
		     | _ => loop(ERRORtyc,nil,0,IND,tyc_eps)
	  in
	      case reptyc
	       of GENtyc {kind,arity,eq,path,...} =>
		  (case kind
		    of FORMAL =>
		       let val tk = TKind.TKCint(arity)
			   val knd = newTycKind(epath,tk)
			   val tyc = GENtyc{stamp=mkStamp(), arity=arity,
					    path=IP.append(rpath,path),
					    kind=knd, eq=ref(eqprop),
					    stub = NONE}
		       in (FinalTyc(ref(INST tyc)), SOME(tyc,(epath,tk)))
		       end
		     | DATATYPE _ =>
		       let val tyc = GENtyc{stamp=mkStamp(), kind=kind,
					    arity=arity, stub = NONE,
					    eq=ref(eqprop), path=path}
		       in (FinalTyc(ref(NOTINST tyc)), NONE)
		       (* domains of dataconstructors will be instantiated
			* in instToTyc *)
		       end
		     | _ => bug "scanForRep 9")
		| ERRORtyc => (FinalTyc(ref(INST ERRORtyc)), NONE)
		| DEFtyc _ => bug "scanForRep 6"
		| _ => bug "scanForRep 7"
	  end

      fun getSlotEp slot =
	  case !slot
            of PartialTyc{tycon, epath, ...} => (tycon, epath)
             | ErrorTyc => (ERRORtyc, nil: EP.entPath)
             | _ => bug "getSlotEp"

      fun finalize(defOp,slots) =
	  let val (finalInst, tcOp) =
		  case defOp
		    of SOME(tycInst,_) => (FinalTyc(ref(tycInst)), NONE)
		     | NONE =>
			(scanForRep(map getSlotEp slots)
			 handle INCONSISTENT_EQ =>
			   (err EM.COMPLAIN
			        "inconsistent equality properties in type sharing"
				EM.nullErrorBody;
			    (ErrorTyc,NONE)))
	   in app (fn sl => sl := finalInst) slots;
	      tcOp
	  end

      val _ = addInst(this_slot,infinity)

(* FIXME (DBM): needs fixing (like the similar case in buildStrClass) *)
      (* verify that any class definition is defined outside of the
       * outermost sharing constraint *)
      val _ = case !classDef
		of NONE => () (* no definition - ok *)
		 | SOME(_,depth) =>
		    if !minDepth <= depth
		    then (if !ElabControl.shareDefError
			  then classDef := SOME(INST(ERRORtyc),0)
			  else ();
			  err (if !ElabControl.shareDefError
			      then EM.COMPLAIN
			      else EM.WARN)
			     ("type definition spec inside of sharing at: "^
			      SymPath.toString this_path)
			     EM.nullErrorBody)
		    else ()

   in finalize(!classDef,!class)
  end (* buildTycClass *)

(* debugging wrapper
val buildTycClass = wrap "buildTycClass" buildTycClass
*)

fun sigToInst (ERRORsig, entEnv, instKind, rpath, err, compInfo) =
      (ErrorStr,[],[],0)
  | sigToInst (sign, entEnv, instKind, rpath, err,
	       compInfo as {mkStamp,...}: EU.compInfo) =
  let val flextycs : T.tycon list ref = ref []
      val flexeps : (EP.entPath * TKind.tkind) list ref = ref []
      val cnt = ref 0

      fun addbt NONE = ()
        | addbt (SOME (tc,ep)) =
            (flextycs := (tc::(!flextycs));
             flexeps := (ep::(!flexeps));
             cnt := ((!cnt) + 1))

      fun expand ErrorStr = ()
        | expand (FinalStr {expanded=ref true,...}) = ()
        | expand (FinalStr {sign,slotEnv,expanded,...}) =
            (*
             * We must expand the FinalStr inst in a top-down
             * fashion.  So, we iterate through the bindings.
             * As we encounter structure or type element, we recursively
             * expand it.
             *)
            let fun expandInst (sym, slot) =
                 (debugmsg(">>> [INS]expandInst: " ^ S.symbolToString sym);
                   case !slot
                    of InitialStr _ =>
                         (debugmsg("=== [INS]expandInst: exploring InitialStr "^
                                   S.name sym);
                          buildStrClass (slot,0,entEnv,mkStamp,err)
                              handle ExploreInst _ => bug "expandInst 1";

                          case !slot
                           of (inst as (FinalStr _)) =>
                             (debugmsg("== [INS]expandInst: expanding new FinalStr "^
                                       S.name sym);
                              expand inst)
                            | ErrorStr => ()
                            | _ => bug "expand_substr 2")
                     | PartialStr{path,...} =>
                         bug ("expandInst: PartialStr "^IP.toString path)
                     | inst as FinalStr _ =>
                         (debugmsg("=== [INS]expandInst: expanding old FinalStr "^
                                   S.name sym);
                          expand inst)
                     | InitialTyc _ =>
                         addbt(buildTycClass(!cnt, slot, entEnv, instKind,
					     rpath, mkStamp, err))
                     | _ => ())

             in debugmsg ">>> [INS]expand"; expanded := true;
                app expandInst (getElemSlots(sign,slotEnv));
                debugmsg "<<< [INS]expand"
            end
        | expand _ = bug "expand"

      val baseSlot = ref(InitialStr{sign=sign, sigDepth=1, path=rpath, epath=[],
                                    inherited=ref [], slotEnv=nil})
         (* correct initial value for sigDepth? *)

      val _ = buildStrClass(baseSlot,0,entEnv,mkStamp,err)
              handle (ExploreInst _) => bug "sigToInst 2"

      val strInst = !baseSlot
      val _ = expand strInst

   in (strInst, !flextycs, !flexeps, !cnt)
  end (* fun sigToInst *)

exception Get_Origin  (* who is going to catch it? *)

fun get_stamp_info instance =
  case instance
   of (FinalStr {stamp,...}) => stamp
    | ErrorStr => raise Get_Origin
    | _ => bug "get_stamp_info"


fun instToStr (instance, entEnv, instKind, cnt, addRes, rpath: IP.path, err,
               compInfo as {mkStamp, ...}: EU.compInfo)
              : M.strEntity =
let fun instToStr' (instance as (FinalStr{sign as SIG {closed, elements,... },
					  slotEnv,finalEnt,stamp,...}),
                    entEnv, rpath: IP.path, failuresSoFar: int)
              : M.strEntity * int =
 	(debugmsg (">>instToStr': " ^ IP.toString rpath);
	 case !finalEnt
	  of CONST_ENT strEnt => (strEnt,failuresSoFar)  (* already visited *)
	   | PATH_ENT ep =>
	     (let val strEnt = EE.lookStrEP(entEnv,ep)
	      in finalEnt := CONST_ENT strEnt;
	      (strEnt,failuresSoFar)
	      end
		  handle EE.Unbound =>
			 (debugmsg ("instToStr':PATH_ENT failed: "^
				    EP.entPathToString ep);
			  raise EE.Unbound))
	   | GENERATE_ENT closedDef =>
	     let
		 (* Gets the stamp of an instance -- generates one if
		  * one is not already built. *)
		 fun getStamp instance : Stamps.stamp =
		     let val stamp = get_stamp_info instance
		     in case (!stamp)
			 of STAMP s => (debugmsg "getStamp:STAMP"; s)
			  | PATH ep =>
			    (debugmsg ("getStamp:PATH "^EntPath.entPathToString ep);
			     (let val {stamp=s,...} = EE.lookStrEP(entEnv,ep)
			      in stamp := STAMP s; s
			      end
				  handle EE.Unbound => (debugmsg "getStamp:PATH failed";
						   raise EE.Unbound)))
			 | GENERATE =>
			    let val s = mkStamp()
			     in debugmsg "getStamp:GENERATE";
				 stamp := STAMP s; s
			    end
		  end

		val newFctBody =
		  (case instKind
		    of INST_ABSTR {entities,...} =>
		       let fun f (sign as FSIG{paramvar,bodysig,...},ep,_,_) =
			       let val fctEnt = EE.lookFctEP (entities, ep)
				   val bodyExp =
				       M.ABSstr (bodysig,
						 APPLY(CONSTfct fctEnt,
						       VARstr [paramvar]))
			       in (bodyExp, NONE)
			       end
			     | f _ = bug "newFctBody:INST_ABSTR"
		       in
			   f
		       end
		     | INST_FMBD tps =>
		       (fn (sign, _, _, _) =>
			 let val i = cnt()
			     val res = T.TP_SEL(tps,i)
			     val _ = addRes(NONE, res)
			  in (M.FORMstr sign, SOME res)
			 end)

		     | INST_PARAM tdepth =>
		       (fn (sign, ep, rp, nenv) =>
			 let val tk = getTkFct{sign=sign,entEnv=nenv,
					       rpath=rp,compInfo=compInfo}
			     val res = T.TP_VAR ({ tdepth = tdepth,
						   num = cnt (),
						   kind = tk })
			     val _ = addRes(SOME(ep, tk), res)
			  in (M.FORMstr sign, SOME res)
			 end))

		fun instToTyc(ref(INST tycon),_) = tycon
		      (* already instantiated *)
		  | instToTyc(r as ref(NOTINST tycon), entEnv) = let
			fun badtycon () =(* bogus tycon *)
			    (debugType("#instToTyc(NOTINST/bogus)",tycon);
			     r := INST ERRORtyc;
			     ERRORtyc)
		    in
		     case tycon
		       of T.DEFtyc{tyfun=T.TYFUN{arity, body}, strict, stamp, path} =>
			   (* DEFtyc body gets instantiated here *)
			   (* debugging version *)
			   (let val tc =
				    let val tf = T.TYFUN{arity=arity,
							 body=MU.transType entEnv body}
			             in T.DEFtyc{tyfun=tf, strict=strict,
						 stamp=mkStamp(),
						 path=IP.append(rpath,path)}
				     end
			     in debugType("#instToTyc(NOTINST/DEFtyc)",tc);
			        r := INST tc;
			        tc
			    end
			    handle EE.Unbound =>
			      (debugmsg "#instToTyc(NOTINST/DEFtyc) failed";
			       raise EE.Unbound))

			| T.GENtyc {stamp,arity,eq,path,kind,...} =>
			  (case kind of
			       z as T.DATATYPE {index,freetycs,stamps,
                                                family,root,stripped} =>
			       (let
			       (* no coordination of stamps between mutually
				* recursive families of datatypes? *)
				   val nstamps =
				       (case root
					 of NONE =>
					    (* this is the lead dt of family *)
					    Vector.map
						(fn _ => mkStamp()) stamps
					  | SOME rootev =>
				      (* this is a secondary dt of a family,
				       * find the stamp vector for the root
				       * dt of the family, which should already
				       * have been instantiated *)
					    (case EE.lookTycEnt(entEnv, rootev)
                                              of T.GENtyc { kind =
						      T.DATATYPE{stamps, ...},
							    ... } => stamps
                                               | T.ERRORtyc =>
						 Vector.map
						     (fn _ => mkStamp()) stamps
                                               | _ =>
					(* oops, the root instantiation
					 * is not a datatype (see bug 1414) *)
					      bug "unexpected DATATYPE 354"))
				   val s = Vector.sub(nstamps, index)
				   val nfreetycs =
                                       map (MU.transTycon entEnv) freetycs

				   val nkind =
				       T.DATATYPE{index=index,
						  family=family,
						  stripped=stripped,
						  stamps=nstamps,
						  freetycs=nfreetycs,
						  root=NONE} (* root ??? *)

				   val tc =
				       T.GENtyc{stamp=s, arity=arity, eq=eq,
						path=IP.append(rpath,path),
						kind=nkind,stub=NONE}

				in
				    r := INST tc;
				    tc
				end handle EE.Unbound =>
				   (debugmsg "#instToTyc(NOTINST/DATA) failed";
				    raise EE.Unbound))
			     | _ => badtycon ())
			| PATHtyc{entPath,...} =>
			  (let val _ =
				   debugmsg ("#instToTyc(NOTINST/PATHtyc): "^
					     EP.entPathToString entPath)
			       val tyc = EE.lookTycEP(entEnv,entPath)
			   in
			       r := INST tyc;
			       tyc
			   end
			   handle EE.Unbound =>
			     (debugmsg "#instToTyc(NOTINST/PATHtyc) failed";
			      raise EE.Unbound))
			| _ => badtycon ()
		    end

		(*
		 * Creates an entity from the instance node found
		 * in the given slot.
		 *)
		fun instToEntity (sym,slot,entEnv,failuresSoFar:int)
		      : M.entity * int =
		    (debugmsg ("instToEntity: "^Symbol.name sym^" "^
			       Int.toString failuresSoFar);
		    case !slot
		     of (inst as (FinalStr _)) =>
			  let val (strEntity,n) =
			          instToStr'(inst, entEnv, IP.extend(rpath,sym),
					     failuresSoFar)
			   in (STRent strEntity, n)
			  end

		      | FinalTyc r =>
			  (TYCent(instToTyc(r,entEnv)),failuresSoFar)

		      | FinalFct{sign as FSIG{paramvar,...},
				 def, epath, path} =>
			 (case !def
			   of SOME(FCT { rlzn, ... }) => FCTent rlzn
				(*** would this case ever occur ??? ***)

			    | NONE =>
			      let val stamp = mkStamp()
				  val (bodyExp, tpOp) =
				      newFctBody(sign, epath, path, entEnv)
				  val cl = CLOSURE{param=paramvar,
						   body=bodyExp,
						   env=entEnv}
			      in FCTent {stamp = stamp,
					 rpath=path,
					 closure=cl,
					 properties = PropList.newHolder (),
					 (* lambdaty=ref NONE,*)
					 tycpath=tpOp,
					 stub=NONE}
			      end

			    | _ => bug "unexpected functor def in instToStr",
			   failuresSoFar)

		      | ErrorStr => (ERRORent,failuresSoFar)
		      | ErrorTyc => (ERRORent,failuresSoFar)
		      | inst => (say("bad inst: " ^ instToString inst ^ "\n");
				 (ERRORent,failuresSoFar)))

                 (* a DEFtyc entity instantiating a datatype spec (an
		  * explicit or implicit datatype replication spec), must
		  * be unwrapped, so that the instantiation is a datatype.
		  * This replaces the unwrapping that was formerly done
		  * in checkTycBinding in SigMatch.  Fixes bugs 1364 and
		  * 1432. [DBM]
		  *)
		 fun fixUpTycEnt (TYCspec{info=RegTycSpec{spec=GENtyc{kind=DATATYPE _,
                                                                      ...},...},...},
				  TYCent(tyc)) =
		       (* possible indirect datatype repl.  See bug1432.7.sml *)
		       TYCent(TU.unWrapDefStar tyc)
		   | fixUpTycEnt (TYCspec{info=RegTycSpec{repl=true,...},...},
                                  TYCent(tyc)) =
		       (* direct or indirect datatype repl.  Original spec
			* was a datatype spec. See bug1432.1.sml *)
		       TYCent(TU.unWrapDefStar tyc)
		   | fixUpTycEnt (_,ent) = ent

		 fun mkEntEnv (baseEntC) =
		     foldl (fn ((sym,spec),(env,failCount)) =>
			      (debugmsg ("mkEntEnv: "^Symbol.name sym);
			       case MU.getSpecVar spec
				 of SOME v =>
				     (let val s = lookSlot(slotEnv,v)
					  val (e,failures) =
					      instToEntity(sym, s, env, failCount)
					  val e = fixUpTycEnt(spec,e)
				       in debugmsg ("ok: "^EP.entVarToString v);
					  (EE.bind(v, e, env), failures)
				      end
				      handle EE.Unbound =>  (* tycon inst *)
					(debugmsg ("failed at: "^S.name sym);
					 (env, failCount+1)))
				  | NONE => (env,failCount)))
			   baseEntC elements

 	        val (entEnv',failCount) =
                  if closed andalso closedDef
                  then (let val _ = debugmsg "mkEntEnv: closed"
                            val (ee, fc) = mkEntEnv(EE.empty, 0)
                         in (ee, fc+failuresSoFar)
                        end)
                  else (let val _ = debugmsg "mkEntEnv: not closed";
                            val baseEntC =
                              (MARKeenv{stamp = mkStamp(),
					env = entEnv,
					stub = NONE },
			       failuresSoFar)
                            val (ee, fc) = mkEntEnv(baseEntC)
                         in (ee, fc)
                        end)

		 val strEnt={stamp =getStamp instance,
			     rpath=rpath,
			     entities=entEnv',
			     properties = PropList.newHolder (),
			     (* lambdaty=ref NONE, *)
			     stub=NONE}
		 val _ = debugmsg (String.concat["--instToStr': failuresSoFar = ",
						 Int.toString failuresSoFar,
						 ", failCount = ",
						 Int.toString failCount])

	      in if failCount = 0
		 then finalEnt := CONST_ENT strEnt
		 else ();
		 ED.withInternals(fn () =>
		   ED.debugPrint debugging
		    (("<<instToStr':" ^ IP.toString rpath^":"),
		     (fn ppstrm => fn ent =>
		        PPModules_DB.ppEntity ppstrm StaticEnv.empty (ent, 20)),
		     M.STRent strEnt));
		 (strEnt, failCount)
	     end)
      | instToStr'(ErrorStr, _, _, failuresSoFar) =
           (bogusStrEntity,failuresSoFar)
      | instToStr' _ = bug "instToStr - instance not FinalStr"

    fun loop(strEnt,failures) =
       (debugmsg ("instToStr': failures = " ^ Int.toString failures);
        if failures = 0
	then strEnt
	else let val (strEnt',failures') =
	             instToStr'(instance,entEnv,rpath,0)
	     in if failures' < failures
		then loop(strEnt',failures')
		else (err EM.COMPLAIN "dependency cycle in instantiate"
		         EM.nullErrorBody;
		      strEnt')
	     end)
 in loop(instToStr'(instance,entEnv,rpath,0))
end (* fun instToStr *)


(*** fetching the TycKind for a particular functor signature ***)
and getTkFct{sign as M.FSIG{paramvar, paramsig, bodysig, ...}, entEnv,
             rpath, compInfo as {mkStamp, ...} : EU.compInfo} =
      let val (arg_eps, res_eps) =
           (case (paramsig, bodysig)
             of (SIG psg, SIG bsg) =>
		(case (SPL.sigBoundeps psg, SPL.sigBoundeps bsg)
		  of (SOME x, SOME y) => (x, y)
		   | (_, z) =>
                     let val region=SourceMap.nullRegion
			 val (rlzn, _, _, args, _) =
                             instGeneric{sign=paramsig, entEnv=entEnv,
					 rpath=rpath,
					 instKind=INST_PARAM DebIndex.top,
					 region=region, compInfo=compInfo}
                                     (*
                                      * We use DI.top temporarily,
                                      * the tycpath result is discarded
                                      * anyway. (ZHONG)
                                      *)

                     in (case z
                         of SOME u => (args, u)
                          | NONE =>
                             let val entEnv' =
                                   EE.mark(mkStamp,
                                       EE.bind(paramvar, STRent rlzn, entEnv))

                                 val (_, _, _, res, _) =
                                   instGeneric{sign=bodysig, entEnv=entEnv',
                                               rpath=rpath, region=region,
                                               instKind=INST_PARAM
							    DebIndex.top,
                                               compInfo=compInfo}
                                     (*
                                      * We use DI.top temporarily,
                                      * the tycpath result is discarded
                                      * anyway. (ZHONG)
                                      *)

                              in (args, res)
                             end)
                     end)
              | _ => ([], []))

          val arg_tks = map #2 arg_eps
          val res_tks = map #2 res_eps

       in TKind.TKCfun(arg_tks, TKind.TKCseq res_tks)
      end

  | getTkFct _ = TKind.TKCfun([], TKind.TKCseq [])

(*** the generic instantiation function ***)
and instGeneric{sign, entEnv, instKind, rpath, region,
                compInfo as {mkStamp,error,...} : EU.compInfo} =
  let val _ = debugmsg (">>> [INS]instGeneric: " ^ signName sign)
      val _ = error_found := false
      fun err sev msg = (error_found := true; error region sev msg)
      val baseStamp = mkStamp()

      val (inst, abstycs, tyceps, cnt) =
          sigToInst(sign, entEnv, instKind, rpath, err, compInfo)

      val counter = ref cnt
      fun cntf x =
	  let val k = !counter
	      val _ = (counter := k + 1)
	   in k
	  end

      val alleps = ref (tyceps)
      val alltps : T.tycpath list ref = ref []
      fun addRes(NONE, tp) = (alltps := tp::(!alltps))
        | addRes(SOME z, tp) =
             (alleps := (z::(!alleps)); alltps := tp::(!alltps))

      val strEnt =
          instToStr(inst,entEnv,instKind,cntf,addRes,rpath,err,compInfo)

      val (abs_tycs, fct_tps, all_eps) =
        (rev abstycs, rev(!alltps), rev(!alleps))

      (* let's memoize the resulting boundeps list *)
      val _ = case sign
               of M.SIG sr =>
		  (case SPL.sigBoundeps sr of
		       NONE => SPL.setSigBoundeps (sr, SOME all_eps)
		     | _ => ())
		| _ => ()

      val _ = debugmsg ("<<< [INS]instGeneric " ^ signName sign)
   in (strEnt, abs_tycs, fct_tps, all_eps, rev tyceps)
  end

(* debugging wrappers 
val sigToInst = wrap "sigToInst" sigToInst
val instToStr = wrap "instToStr" instToStr
val instGeneric = wrap "instGeneric" instGeneric 
*)

(*** instantiation of the formal functor body signatures ***)
fun instFmBody{sign, entEnv, tycpath, rpath, region, compInfo} =
  let val (rlzn, tycs, _, _, tyceps)
        = instGeneric{sign=sign, entEnv=entEnv, instKind=INST_FMBD tycpath,
                      rpath=rpath, region=region, compInfo=compInfo}
   in {rlzn=rlzn, abstycs=tycs, tyceps=map #1 tyceps}
  end

(*** instantiation of the structure abstractions **)
fun instAbstr{sign, rlzn, entEnv, rpath, region, compInfo} =
  let val _ = debugmsg ">>> [INS]instAbstr"
      val (newRlzn, tycs, _, _, tyceps) =
          instGeneric{sign=sign, entEnv=entEnv, instKind=INST_ABSTR rlzn,
                      rpath=rpath, region=region, compInfo=compInfo}
   in {rlzn=newRlzn, abstycs=tycs, tyceps=map #1 tyceps}
      before (debugmsg "<<< [INS]instAbstr")
  end

(*** instantiation of the functor parameter signatures ***)
fun instParam{sign, entEnv, tdepth, rpath, region, compInfo} =
  let val (rlzn, tycs, fcttps, _, _)
        = instGeneric{sign=sign, entEnv=entEnv, instKind=INST_PARAM tdepth,
                      rpath=rpath, region=region, compInfo=compInfo}

      fun h1(T.GENtyc { kind = T.FLEXTYC tp, ... }) = tp
        | h1 _ = bug "unexpected h1 in instParam"

      val tps = (map h1 tycs) @ fcttps
   in {rlzn=rlzn, tycpaths=tps}
  end

(*** fetching the list of tycpaths for a particular structure ***)
fun getTycPaths{sign as M.SIG sr, rlzn : M.strEntity, entEnv,
	        compInfo as {error,...}: EU.compInfo} =
      let val { entities, ... } = rlzn
	  val epslist =
           case SPL.sigBoundeps sr
             of SOME x => x
              | NONE =>
                  let val (_, _, _, all_eps, _) =
                        instGeneric{sign=sign, entEnv=entEnv,
                                    rpath=IP.IPATH[], compInfo=compInfo,
                                    instKind=INST_PARAM DebIndex.top,
                                    region=SourceMap.nullRegion}
                                     (*
                                      * We use DI.top temporarily,
                                      * the tycpath result is discarded
                                      * anyway. (ZHONG)
                                      *)
                   in all_eps
                  end

          fun getTps (ep, _) =
            let val ent = EE.lookEP(entities, ep)
             in case ent
                 of M.TYCent (T.GENtyc { kind = T.FLEXTYC tp, ... }) => tp
                  | M.TYCent tyc => T.TP_TYC tyc
		  | M.FCTent {tycpath = SOME tp,...} => tp
                  | M.ERRORent => T.TP_TYC T.ERRORtyc
                  | _ => bug "unexpected entities in getTps"
            end

       in map getTps epslist
      end

  | getTycPaths _ = []


val instParam =
  Stats.doPhase (Stats.makePhase "Compiler 032 instparam") instParam

(*
val instFmBody =
  Stats.doPhase (Stats.makePhase "Compiler 032 2-instFmBody") instFmBody

val instAbstr =
  Stats.doPhase (Stats.makePhase "Compiler 032 3-instAbstr") instAbstr

val getTycPaths =
  Stats.doPhase (Stats.makePhase "Compiler 032 4-getTycPaths") getTycPaths
*)

end (* local *)
end (* structure Instantiate *)

(* [dbm, 6/16/06] Eliminated ii2ty from INSTANTIATE_PARAM. Eventually want
   to eliminate the parameterization completely. *)

