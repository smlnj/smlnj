(* ppobj.sml
 *
 * COPYRIGHT (c) 2017, 2022 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

signature PPOBJ =
sig
  type object
  val fmtObj : StaticEnv.staticEnv
              -> object * Types.ty * int
              -> NewPrettyPrint.format
end

structure PPObj : PPOBJ =
struct

local (* top *)

  structure PP = NewPrettyPrint
  structure S = Symbol
  structure V = Vector
  structure A = Access
  structure T = Types
  structure TU = TypesUtil
  structure BT = BasicTypes
  structure F = Fixity
  structure Obj = Unsafe.Object

  (* debugging
  NOT USED:
  val say = Control.Print.say
  val debugging = ref false
  fun dbsaynl (msg: string) =
      if !debugging then (say msg; say "\n") else ()
  *)

  fun bug msg = ErrorMsg.impossible ("PPObj: " ^ msg)

in

type object = Obj.object

(* gettag : object -> int -- should be defined in Unsafe.Object *)
fun gettag obj = Obj.toInt (Obj.nth (obj, 0))

exception Switch

fun switch(obj, dcons) =
    let fun check (f : object -> int, tag : int) : bool =
	    (f obj = tag) handle Obj.Representation => false
	fun try ((d as {name,rep,domain})::r) =
	    (case rep
	       of A.TAGGED i =>
		    if check (gettag, i) then d else try r
		| A.CONSTANT i =>
		    if check (Obj.toInt, i) then d else try r
		| A.TRANSPARENT => d
		| A.UNTAGGED => if Obj.boxed obj then d else try r
		| A.REF => d
		| A.LISTCONS => if (Obj.boxed obj) then d else try r
		| A.LISTNIL => if check (Obj.toInt, 0) then d else try r
		| A.SUSP _ => d  (* LAZY *)
		| _ => bug "switch: funny datacon"
	      (* end case *))
	  | try [] = bug "switch: none of the datacons matched"
     in try dcons
    end

(* FIXME: I think that this function is needed because the "TRANSPARENT"
 * representation was disabled (see ElabData/types/conreps.sml)
 *)
fun isUbxTy (T.VARty(ref (T.INSTANTIATED t))) = isUbxTy t
  | isUbxTy (T.CONty(tc as T.GENtyc _, [])) =
      (Target.is64 andalso
       (TU.eqTycon (tc, BT.int64Tycon) orelse TU.eqTycon (tc, BT.word64Tycon)))
      orelse
      (TU.eqTycon (tc, BT.int32Tycon) orelse TU.eqTycon (tc, BT.word32Tycon))
  | isUbxTy _ = false

fun decon (obj, {rep, name, domain}) =
    (case rep
       of A.UNTAGGED => (case domain
             of SOME t => if (TU.isRecordTy t) orelse (isUbxTy t)
                  then obj
		  else (Obj.nth(obj, 0) handle e => raise e)
              | _ => bug "decon -- unexpected conrep-domain"
	    (* end case *))
	| A.TAGGED _ => (Obj.nth(obj,1) handle e => raise e)
(*	| A.TAGGEDREC _ =>
	   let (* skip first element, i.e. discard tag *)
	       val a = tuple obj
	       fun f i =
		   if i < Vector.length a
		   then Vector.sub (a,i) :: f(i+1)
		   else []
	    in U.cast (Vector.fromList (f 1))
	   end
*)
	| A.CONSTANT _ => Obj.toObject ()
	| A.TRANSPARENT => obj
	| A.REF => !(Obj.toRef obj)
	| A.EXN _ => (Obj.nth(obj,0) handle e => raise e)
	| A.LISTCONS => obj
	| A.LISTNIL => bug "decon - constant datacon in decon"
	| A.SUSP _ => obj
      (* end case *))

local
    fun dconsOf (T.GENtyc {kind = T.DATATYPE {family = {members = #[{dcons, ...}],...},...},...}) = dcons
      | dconsOf _ = bug "(u)listDcons"
in
    val listDcons = dconsOf BT.listTycon
    val ulistDcons = dconsOf BT.ulistTycon
end

type sharingKey = unit ref
type sharingMemo = int option ref
type sharingEnv = (sharingKey * sharingMemo) list

local
  (* counter to generate identifier *)
  val counter : int ref = ref 0

  (* incCounter : unit -> int *)
  fun incCounter () =
      let val id = !counter
       in counter := id + 1;
	  id
      end

(*  val sharingEnvRef : sharingEnv ref = ref nil *)

  (* lookup : sharingKey * sharingEnv -> (int option ref) option
   * association list lookup with unit ref keys *)
  fun sharingLookup (key: sharingKey, senv: sharingEnv) =
      let fun lookup nil = NONE
	    | lookup ((x,r)::rest) = if x = key then SOME r else lookup rest
       in lookup senv
      end

  (* sharingBind : sharingKey * sharingMemo * sharingEnv -> sharingEnv *)
  fun sharingBind (key: sharingKey, memo : sharingMemo, senv : sharingEnv) =
      (key, memo) :: senv

  (* wasSeen : object * (unit ref * int option ref) list -> int option
   * The object is either a ref or array value, i.e. an impure or statefull value.
   * Casts the object to type unit ref to use it as key for lookup in the sharing
   * environment. If a binding is found, returns SOME of an "identification number",
   * creating a new one if object memo was not "set". *)
  fun wasSeen (obj, senv: sharingEnv) =
      let val key : unit ref = Unsafe.cast obj
       in case sharingLookup (key, senv)
	    of NONE => NONE               (* not in sharingEnv domain *)
	     | SOME (memo as ref NONE) => (* uninitialized memo exists *)
	 	 let val idOp = SOME (incCounter ())
		  in memo := idOp;        (* update the memo with the next id *)
		     idOp
		 end
	     | SOME (ref (id as (SOME _))) => id  (* memo exists and is initialized *)
      end
in

val emptySharingEnv : sharingEnv = nil

(* resetSharing : () -> () *)
fun resetSharing () = (counter := 0)

(* formatWithSharing : object * (object * sharingEnv -> PP.format) * sharingEnv -> format
 * Format with "sharing". An occurrence of a mutable object (a ref or array)
 * within its own contents is printed as a reference number (%n) associated with
 * the "outermost occurrence" of the object. See example in [sharing-note.txt]. 
 * A memo for such an object is set only when an occurrence of that object within
 * its own contents (an "inner" occurrence) is detected. Memo bindings for an object
 * are only accessible while formatting its contents, not in "parallel" objects.
 * Thus parallel sharing like "let val x = ref 0 in (x, x) end" are not handled.
 * It looks like self-cyclical values have to involve recursive types. *)
fun formatWithSharing (object: object, formatter: object * sharingEnv -> PP.format, senv: sharingEnv) =
    if !Control.Print.printLoop then
       (case wasSeen (object, senv)  (* was this object seen before, on the way inward? *)
	  of SOME id => PP.ccat (PP.text "%", PP.integer id)  (* format using the sharing "id number" *)
	   | NONE =>  (* this object will now be formatted for the first time *)
	       let val memo = ref NONE
		   val key : unit ref = Unsafe.cast object
	           val senv' = sharingBind (key, memo, senv)  (* bind an unset memo for this object *)
	        in let val objFmt = formatter (object, senv')
			   (* format the object, including its content; this can set the memo *)
		    in case !memo  
		         of NONE => objFmt
			    (* memo was not set, so object not found shared within itself *)
		          | SOME id => (* memo was set, defining an "id number" for this object,  *)
			    PP.hblock [objFmt, PP.text "as", PP.ccat (PP.text "%", PP.integer id)]
		   end
	       end)
    else formatter (object, senv)  (* in this case, senv is always ignored *)

end (* local *)

(* tycContext is a pair of tycon lists, the 
 * first a list of datatype family members (for interpretting RECtyc), and 
 * the second a list of tycons used to interpret FREEtyc tycons *)
type tycContext = T.tycon list * T.tycon list

(* interpArgs : T.ty list * tycContext option -> T.ty list *)
(* Used to "instantiate" RECtyc and FREEtyc tycons in the argument types of ref and array tycons.
 * Instantiation of RECtyc and FREEtyc tycons in the main type are instantiated directly using
 * tycontextOP argument *)
fun interpArgs (tys, NONE) = tys
  | interpArgs (tys, SOME (members,freetycs)) =
        (* subst : T.ty -> T.ty *)
    let fun subst (T.CONty (T.RECtyc n, args)) =
	      let val tyc' = (List.nth (members,n)
	                     handle Subscript => bug "interpArgs 1")
	       in T.CONty (tyc', map subst args)
	      end
	  | subst (T.CONty (T.FREEtyc n,args)) =
	      let val tyc' = (List.nth (freetycs,n)
	                     handle Subscript => bug "interpArgs 2")
	       in T.CONty (tyc', map subst args)
	      end
	  | subst (T.CONty (tyc,args)) = T.CONty (tyc, map subst args)
	  | subst (T.VARty (ref(T.INSTANTIATED ty))) = subst ty
	  | subst ty = ty
     in map subst tys
    end

fun transMembers(stamps: Stamps.stamp vector,
                 freetycs: T.tycon list, root,
                 family as {members,...} : T.dtypeFamily) =
    let fun dtmemberToTycon(n, {tycname,arity,dcons,eq,sign,lazyp}, l) =
	      T.GENtyc{stamp=Vector.sub(stamps,n),arity=arity,eq=ref(T.YES),
		       path=InvPath.IPATH[tycname],
		       kind=T.DATATYPE{index=n,
				       stamps=stamps, freetycs=freetycs,
				       root=root, family=family, stripped=false},
		       stub = NONE } :: l
     in (Vector.foldri dtmemberToTycon nil members,
         freetycs)
    end

(* printing for monomorphic primitive types (i.e., the types in BasicTypes) *)
local
  fun wordPrefx s = "0wx" ^ s
  fun char2str obj = concat["#\"", Char.toString(Char.chr(Obj.toInt obj)), "\""]
  fun exn2str obj = General.exnName(Obj.toExn obj) ^ "(-)"
  val toStringTbl = [
	  (BT.intTycon,		Int.toString o Obj.toInt),
	  (BT.int32Tycon,	Int32.toString o Obj.toInt32),
	  (BT.int64Tycon,	Int64.toString o Obj.toInt64),
	  (BT.intinfTycon,	PrintUtil.formatIntInf o Unsafe.cast),
	  (BT.wordTycon,	wordPrefx o Word.toString o Obj.toWord),
	  (BT.word8Tycon,	wordPrefx o Word8.toString o Obj.toWord8),
	  (BT.word32Tycon,	wordPrefx o Word32.toString o Obj.toWord32),
	  (BT.word64Tycon,	wordPrefx o Word64.toString o Obj.toWord64),
	  (BT.charTycon,	char2str),
	  (BT.realTycon,	Real64.toString o Obj.toReal64),
	  (BT.exnTycon,		exn2str),
	  (BT.pointerTycon,	fn _ => "cptr"),
	  (BT.stringTycon,	PrintUtil.formatString o Obj.toString),
(* FIXME: actually print the values *)
	  (BT.chararrayTycon,	fn _ => "-"),
	  (BT.word8vectorTycon,	fn _ => "-"),
	  (BT.word8arrayTycon,	fn _ => "-"),
	  (BT.real64arrayTycon,	fn _ => "-"),
	  (BT.arrowTycon,	fn _ => "fn"),	(* perhaps "<fn>" or "-fn-" instead? *)
	  (BT.contTycon,	fn _ => "cont") (* perhaps "<cont>" or "-cont-" instead? *)
	]
in
  fun primToString tyc = List.find (fn (tyc', _) => TU.eqTycon(tyc, tyc')) toStringTbl
end (* local *)

val nopull = 0

(* Main Function: 
 * fmtObj: staticEnv -> (object * ty * int) -> PP.format *)
fun fmtObj env (obj: object, ty: T.ty, depth: int) : PP.format =
let fun fmtClosed (obj:object, ty:T.ty, tycontextOp: tycContext option, senv: sharingEnv, depth:int) =
        fmtObj' (obj, ty, tycontextOp, senv, depth, nopull, nopull)

    and fmtObj' (_, _, _, _, 0, _, _) : PP.format = PP.text  "#"
      | fmtObj' (obj: object, ty: T.ty, tycontextOp: tycContext option,
                 senv: sharingEnv, depth: int, lpull: int, rpull: int) =
	((case ty
	   of T.VARty (ref(T.INSTANTIATED t)) =>
		fmtObj' (obj, t, tycontextOp, senv, depth, rpull, lpull)
	    | T.POLYty {tyfun=T.TYFUN{body,arity},...} =>
		if arity=0
	        then  fmtObj' (obj, body, tycontextOp, senv, depth, lpull, rpull)
	        else let val args = Obj.mkTuple (List.tabulate(arity, fn i => Obj.toObject 0))
			 val tobj : object -> object = Unsafe.cast obj   (* DBM: dummy runtime types? *)
			 val res = tobj args
		      in fmtObj' (res, body, tycontextOp, senv, depth, lpull, rpull)
		     end
	    | T.CONty (tyc as T.GENtyc { kind, stamp, eq, ... }, argtys) =>
		(case (kind, !eq)
		   of (T.PRIMITIVE, _) =>
			(case primToString tyc
			   of SOME(_, fmt) => PP.text (fmt obj)
			    | NONE => (* check for vector/array type constructors *)
				if TU.eqTycon(tyc,BT.vectorTycon)
				then fmtVector (Obj.toVector obj, hd argtys, tycontextOp, senv, depth,
					       !Control.Print.printLength)
				     handle Obj.Representation => PP.text  "<primvec?>"
				else if TU.eqTycon (tyc, BT.arrayTycon)
				then let fun formatter (obj, _) =
					     (case Obj.rep obj
						of Obj.PolyArray =>
						     fmtArray (Obj.toArray obj, hd argtys, tycontextOp, senv,
							       depth, !Control.Print.printLength)
						 | Obj.RealArray =>
						     fmtRealArray (Obj.toRealArray obj, !Control.Print.printLength)
						 | _ => bug "array (neither Real nor Poly)")
				      in formatWithSharing (obj, formatter, senv)
				     end
				     handle Obj.Representation => PP.text  "<primarray?>"
				else PP.text  "<prim?>")
		    | (T.DATATYPE _, T.ABS) =>  (* [DBM, 09.20.22] !eq = T.ABS does not imply kind = ABSTRACT? *)
	                (PPTable.formatObject stamp obj
			 handle PPTable.NO_FORMATTER => PP.text  "-" )
		    | (T.DATATYPE{index,stamps, family as {members,...}, freetycs, root, stripped}, _) =>
			 if TU.eqTycon(tyc,BT.ulistTycon)
			 then fmtUrList (obj, hd argtys, tycontextOp, depth, !Control.Print.printLength)
			 else if TU.eqTycon (tyc,BT.suspTycon)
			 then PP.text  "$$"  (* LAZY *)
			 else if TU.eqTycon (tyc,BT.listTycon)
			 then fmtList (obj, hd argtys, tycontextOp, senv, depth, !Control.Print.printLength)
			 else if TU.eqTycon (tyc,BT.refTycon)
			 then let val argtys' = interpArgs(argtys,tycontextOp)
				  fun formatter (obj, senv) =
				      fmtDcon (obj, (Vector.sub(stamps,index), Vector.sub(members,index)),
					       SOME([BT.refTycon],[]), argtys', senv, depth, lpull, rpull)
			       in formatWithSharing (obj, formatter, senv)
			      end
			 else let val argtys' = interpArgs(argtys,tycontextOp)
			       in fmtDcon (obj, (Vector.sub(stamps,index), Vector.sub(members,index)),
					   SOME(transMembers (stamps, freetycs, root, family)),
					   argtys', senv, depth, lpull, rpull)
			      end
		    | _ => PP.text "-"
		(* end case *))
	    | T.CONty(tyc as T.RECORDtyc [], _) => PP.text "()"
	    | T.CONty(tyc as T.RECORDtyc labels, argtys) =>
	        if Tuples.isTUPLEtyc tyc
		then fmtTuple (Obj.toTuple obj, argtys, tycontextOp, senv, depth)
		else fmtRecord (Obj.toTuple obj, labels, argtys, tycontextOp, senv, depth)
	    | T.CONty(tyc as T.DEFtyc _, _) =>
		fmtObj'(obj, TU.reduceType ty, tycontextOp, senv, depth, lpull, rpull)
	    | T.CONty(tyc as T.RECtyc i,argtys) =>
		(case tycontextOp
		   of SOME (memberTycs,_) =>
			let val tyc' =
				List.nth(memberTycs,i)
				handle Subscript =>
				 (print "#fmtObj':  ";
				  print (Int.toString i);
				  print " "; print(Int.toString(length memberTycs));
				  print "\n";
				  bug "fmtObj': bad index for RECtyc")
			 in case tyc'
			      of T.GENtyc {kind = T.DATATYPE{index,stamps, family={members,...},...}, ...} =>
				 fmtDcon (obj, (Vector.sub(stamps,index), Vector.sub(members,index)),
					 tycontextOp, argtys, senv, depth, lpull, rpull)
			       | _ => bug "fmtObj': bad tycon in members"
			end
		    | NONE => bug "fmtObj': RECtyc with no members"
		  (* end case *))
	    | T.CONty(tyc as T.FREEtyc i,argtys) =>
		(case tycontextOp
		    of SOME (_, freeTycs) =>
			 let val tyc' =
				 List.nth(freeTycs,i)
				 handle Subscript =>
				  (print "#fmtObj': ";
				   print (Int.toString i);
				   print " ";
				   print(Int.toString(length freeTycs));
				   print "\n";
				   bug "fmtObj': bad index for FREEtyc")
			  in fmtObj'(obj, T.CONty(tyc', argtys), tycontextOp, senv, depth, lpull, rpull)
			 end
		     | NONE => bug "fmtObj': RECtyc with no members"
		   (* end case *))
	    | _ => PP.text  "-"
	  (* end case *))
	  handle e => raise e)

    and fmtDcon (_, _, _, _, _, 0, _, _) = PP.text  "#"
      | fmtDcon (obj:object, (stamp, {tycname,dcons,...}), tycontextOp : tycContext option,
		 argtys, senv: sharingEnv, depth:int, lpull: int, rpull: int) =
	 PPTable.formatObject stamp obj (* attempt to find and apply user-defined formatter for obj *)
	 handle PPTable.NO_FORMATTER =>
	    if length dcons = 0 then PP.text "-" else
	    let val dcon as {name,domain,...} = switch(obj,dcons)
		val dname = S.name name
	     in case domain
		  of NONE => PP.text dname
		   | SOME dom =>
		      let val fixity = Lookup.lookFix(env,S.fixSymbol dname) (* may be inaccurate? *)
			  val dom = TU.applyTyfun(T.TYFUN{arity=length argtys,body=dom}, argtys)
			  val dom = TU.headReduceType dom (* unnecessary *)
			  fun prdcon () =
			      case (fixity, dom)
				of (F.INfix (leftPull, rightPull), T.CONty(domTyc as T.RECORDtyc _, [leftTy,rightTy])) =>
				   let val (leftArg, rightArg) =
					   case Obj.toTuple (decon(obj,dcon))
					     of [a, b] => (a, b)
					      | _ => bug "fmtDcon [a, b]"
				    in if Tuples.isTUPLEtyc domTyc
				       then PP.pblock
					     [fmtObj'(leftArg, leftTy, tycontextOp, senv, depth-1, 0, leftPull),
					      PP.text  dname,
					      fmtObj'(rightArg, rightTy, tycontextOp, senv, depth-1, rightPull, 0)]
				       else PP.pcat
					      (PP.text dname,
					       PP.softIndent 2 (fmtObj' (decon(obj,dcon), dom, tycontextOp, senv,
									 depth-1, 0, 0)))
				   end
				 | _ =>
				   PP.pcat (PP.text dname,
					    fmtObj'(decon(obj,dcon), dom, tycontextOp, senv, depth-1, 0, 0))
		       in case fixity
			    of F.NONfix => prdcon ()
			     | F.INfix (myleft, myright) => 
				 if (lpull = 0 andalso rpull = 0) orelse lpull >= myleft orelse rpull > myright
				 then PP.parens (prdcon())
				 else prdcon()
		      end
	    end

    and fmtList (obj: object, ty: T.ty, tycontextOp, senv: sharingEnv, depth:int, length: int) =
	let fun listDestruct (p: object) =
		case switch (p, listDcons)
		  of {domain=NONE,...} => NONE                    (* nil case *)
		   | dcon => (case Obj.toTuple (decon (p, dcon))  (* cons case *)
				of [a, b] => SOME(a, b)
				 | _ => bug "fmtList [a, b]")

	    val (elems, more) =
		let fun gather (p, len, elems) =
			case listDestruct p
			  of NONE => (rev elems, false)  (* obj = nil, ran out of elements *)
			   | SOME (x, xs) =>             (* obj = x :: xs *)
			       if len <= 0
			       then (rev elems, true)    (* more elements, not printed *)
			       else gather (xs, len-1, x::elems)
		 in gather (obj, length, nil)
		end

	    val elementsFormats =
		let val fmts = map (fn e => fmtClosed (e, ty, tycontextOp, senv, depth - 1)) elems
		 in if more
		    then fmts @ [PP.text "..."]
		    else fmts
		end

	 in PP.listFormats elementsFormats
	end

    and fmtUrList (obj:object, ty:T.ty, tycontextOp, depth:int, length: int) =
	PP.brackets (PP.text  "unrolled list")

    and fmtTuple (objs: object list, tys: T.ty list, tycontextOp, senv: sharingEnv, depth:int) : PP.format =
	PP.tupleFormats (map (fn (obj, ty) => fmtClosed (obj, ty, tycontextOp, senv, depth - 1))
			(ListPair.zipEq (objs, tys)))

    and fmtRecord (objs: object list, labels: T.label list, tys: T.ty list,
		   tycontextOp, senv: sharingEnv, depth: int) =
	let fun fmtField (f,l,ty) =
		  PP.hblock [PP.text (S.name l), PP.equal, fmtClosed (f, ty, tycontextOp, senv, depth-1)]
	 in PP.braces (PP.psequence PP.comma (map fmtField (List3.zip3Eq (objs, labels, tys))))
	end

    and fmtVector (vectorObj: object vector, ty: T.ty, tycontextOp,
		  senv: sharingEnv, depth:int, length: int) =
	let val vectorLength  = Vector.length vectorObj
	    val elems =
		let val minLength = Int.min (vectorLength, length)
		    fun gather (index, elems) =
			if index < minLength
			then gather (index+1, Vector.sub (vectorObj, index) :: elems)
			else (rev elems)  (* more elements than printed because of length limit *)
		 in gather (0, nil)
		end

	    val elementFormats =
		let val fmts = map (fn elem => fmtClosed (elem, ty, tycontextOp, senv, depth - 1)) elems
		 in if length < vectorLength
		    then fmts @ [PP.text "..."]  (* printing incomplete -- more elements not printed *)
		    else fmts
		end

	 in PP.brackets (PP.psequence PP.comma elementFormats)
	end

    and fmtArray (arrayObj: object array, ty: T.ty, tycontextOp, senv: sharingEnv, depth: int, length: int) =
	let val arrayLength  = Array.length arrayObj

        fun fmtElem elem = fmtClosed (elem, ty, tycontextOp, senv, depth - 1)

	val fmts =
	    let val minLength = Int.min (arrayLength, length)
		fun gather (index, elemFmts) =
		    if index < minLength
		    then gather (index+1, fmtElem (Array.sub (arrayObj, index)) :: elemFmts)
		    else (rev elemFmts)
	     in gather (0, nil)
	    end

	val elementFormats =
	    if length < arrayLength
	    then fmts @ [PP.text "..."]
	    else fmts

     in PP.formatClosedSeq {alignment = PP.P, front = PP.text "[|", back = PP.text "|]",
			    sep = PP.comma, formatter = (fn x => x)}
			   elementFormats
    end

    and fmtRealArray (arrayObj : Real64Array.array, length: int) =
	let val arrayLength  = Real64Array.length arrayObj

	    val fmts =
		let val minLength = Int.min (arrayLength, length)
		    fun gather (index, elemFmts) =
			if index < minLength
			then gather (index+1, PP.text (Real.toString (Real64Array.sub (arrayObj, index))) :: elemFmts)
			else (rev elemFmts)
		in gather (0, nil)
		end

	    val elementFormats =
		if length < arrayLength
		then fmts @ [PP.text "..."]
		else fmts

	 in PP.formatClosedSeq {alignment = PP.P, front = PP.text "[|", back = PP.text "|]",
				sep = PP.comma, formatter = (fn x => x)}
			       elementFormats
	end

 in resetSharing ();
    fmtObj' (obj, ty, NONE, emptySharingEnv, depth, nopull, nopull)
end (* fun ppObj *)

end (* top local *)
end (* structure PPObj *)



