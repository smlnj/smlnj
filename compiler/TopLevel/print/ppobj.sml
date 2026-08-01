(* ppobj.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Support for printing SML values in the REPL
 *)

signature PPOBJ =
  sig
    type object
    val ppObj : StaticEnv.staticEnv
                -> PrettyPrint.stream
                   -> object * Types.ty * int
                      -> unit
    val debugging : bool ref
  end


structure PPObj : PPOBJ =
  struct

    structure PP = PrettyPrint
    structure PU = PPUtil
    structure V = Vector
    structure A = Access
    structure T = Types
    structure TU = TypesUtil
    structure BT = BasicTypes
    structure F = Fixity
    structure Obj = Unsafe.Object

    (* debugging *)
    val say = Control.Print.say
    val debugging = ref false
    fun debugmsg msg = if !debugging then (say msg; say "\n") else ()

    fun bug msg = ErrorMsg.impossible("PPObj: "^msg)

    type object = Obj.object

    fun gettag obj = Obj.toInt (Obj.nth(obj, 0))

    fun switch (obj, dcons) = let
          fun chk (f, tag : int) =
                (f obj = tag) handle Obj.Representation => false
          fun try ((d as {name,rep,domain})::r) = (case rep
                 of A.TAGGED i =>
                      if chk(gettag, i) then d else try r
                  | A.CONSTANT i =>
                      if chk(Obj.toInt, i) then d else try r
                  | A.TRANSPARENT => d
                  | A.UNTAGGED => if Obj.boxed obj then d else try r
                  | A.REF => d
                  | A.LISTCONS => if (Obj.boxed obj) then d else try r
                  | A.LISTNIL => if chk(Obj.toInt, 0) then d else try r
                  | A.SUSP _ => d  (* LAZY *)
                  | _ => bug "switch: funny datacon"
                (* end case *))
            | try [] = bug "switch: none of the datacons matched"
          in
            try dcons
          end

    (** a temporary hack for printing UNTAGGEDREC objects *)
    fun isRecordTy (T.VARty(ref (T.INSTANTIATED t))) = isRecordTy t
      | isRecordTy (T.CONty(T.RECORDtyc _, _::_)) = true
      | isRecordTy _ = false

    (* FIXME: I think that this function is needed because the "TRANSPARENT"
     * representation was disabled (see ElabData/types/conreps.sml)
     *)
    fun isUbxTy (T.VARty(ref (T.INSTANTIATED t))) = isUbxTy t
      | isUbxTy (T.CONty(tc as T.GENtyc _, [])) =
          (Target.is64 andalso
            (TU.eqTycon(tc, BT.int64Tycon) orelse TU.eqTycon(tc, BT.word64Tycon)))
          orelse
            (TU.eqTycon(tc, BT.int32Tycon) orelse TU.eqTycon(tc, BT.word32Tycon))
      | isUbxTy _ = false

    fun decon (obj, {rep, name, domain}) = (case rep
           of A.UNTAGGED => (case domain
                 of SOME t => if (isRecordTy t) orelse (isUbxTy t)
                      then obj
                      else (Obj.nth(obj, 0) handle e => raise e)
                  | _ => bug "decon -- unexpected conrep-domain"
                (* end case *))
            | A.TAGGED _ => (Obj.nth(obj,1) handle e => raise e)
(*
            | A.TAGGEDREC _ => let
                (* skip first element, i.e. discard tag *)
                val a = tuple obj
                fun f i = if i < V.length a
                      then V.sub(a,i) :: f(i+1)
                      else []
                in
                  U.cast (V.fromList (f(1)))
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

    val noparen = F.INfix(0,0)

    local
      fun dconsOf (T.GENtyc{
              kind = T.DATATYPE{family = { members = #[{dcons, ... }], ... }, ... },
              ...
            }) = dcons
        | dconsOf _ = bug "(u)listDcons"
    in
    val listDcons = dconsOf BT.listTycon
    val ulistDcons = dconsOf BT.ulistTycon
    end (* local *)

  local
    (* counter to generate identifier *)
    val cpt = ref 0

    (* test membership in an association list and gives back
     * the second element *)
    fun mem (a: unit ref) = let
          fun m [] = NONE
            | m ((x,r)::l) = if a = x then SOME r else m l
          in
            m
          end

    (* verifies if an object has been seen and if yes, gives back its
     * identification number, creating a new one if necessary *)
    fun isSeen obj l = let
          val obj' = Unsafe.cast obj : unit ref
          in
            case mem obj' l
             of NONE => (false,0)
              | SOME (r as ref NONE) => let
                val id = !cpt
                in
                  cpt := id+1; r := SOME id; (true,id)
                end
              | SOME (ref (SOME id)) => (true,id)
            (* end case *)
          end
    in

    (* reset the identifier counter *)
    fun initCpt () = cpt := 0

    (* print with sharing if necessary. The "printer" already knows the ppstream. *)
    fun printWithSharing ppstrm (obj, accu, printer) =
          if !Control.Print.printLoop
            then let
              val (seen,nb) = isSeen obj accu
              in
                if seen
                  then (PP.string ppstrm "%"; PP.string ppstrm (Int.toString nb))
                  else let
                    val modif = ref NONE
                    val nlAccu = (Unsafe.cast obj : unit ref,modif) :: accu
                    in
                      printer (obj,nlAccu);
                      case !modif
                       of NONE => ()
                        | SOME i => (
                            PP.string ppstrm " as %"; PP.string ppstrm (Int.toString i))
                      (* end case *)
                    end
              end
          else printer (obj,accu)

    end (* local *)

    fun interpArgs (tys, NONE) = tys
      | interpArgs (tys, SOME(members, freetycs)) = let
          fun subst (T.CONty(T.RECtyc n,args)) = let
                val tyc' = (List.nth(members,n) handle Subscript => bug "interpArgs 1")
                in
                  T.CONty(tyc', map subst args)
                end
            | subst (T.CONty(T.FREEtyc n, args)) = let
                val tyc' = (List.nth(freetycs,n) handle Subscript => bug "interpArgs 2")
                in
                  T.CONty(tyc', map subst args)
                end
            | subst (T.CONty(tyc, args)) = T.CONty(tyc, map subst args)
            | subst (T.VARty(ref(T.INSTANTIATED ty))) = subst ty
            | subst ty = ty
          in
            map subst tys
          end

    fun transMembers (
          stamps: Stamps.stamp vector,
          freetycs: T.tycon list, root,
          family as {members,...} : T.dtypeFamily
        ) = let
          fun dtmemberToTycon(n, {tycname,arity,dcons,eq,sign,lazyp}, l) =
                T.GENtyc{
                    stamp=Vector.sub(stamps,n), arity=arity, eq=ref T.YES,
                    path=InvPath.IPATH[tycname],
                    kind=T.DATATYPE{index=n,
                    stamps=stamps, freetycs=freetycs,
                    root=root, family=family, stripped=false},
                    stub = NONE
                  } :: l
          in
            (Vector.foldri dtmemberToTycon [] members, freetycs)
          end

    (* convert an `IEEEReal.decimal_approx` to a string using the format
     * `StringCvt.GEN(SOME 12)`, with the exception that the result should be a
     * valid SML real literal (i.e., the decimal point should not be removed in
     * decimal notation).
     *)
    local
      (* return the number of characters required to represent the exponent *)
      fun expSize 0 = 0
        | expSize e = let
            (* count the "E" and sign *)
            val (n, e) = if (e < 0) then (2, ~e) else (1, e)
            in
              if (n < 10) then n+1
              else if (n < 100) then n+2
              else if (n < 1000) then n+3
              else if (n < 10000) then n+4
              else n+5 (* covers max exponent for 128-bit floats *)
            end
      val digits = #["0","1","2","3","4","5","6","7","8","9"]
      fun getDigit d = Vector.sub(digits, d)
      fun prependDigits ([], suffix) = suffix
        | prependDigits (d::dr, suffix) = getDigit d :: prependDigits(dr, suffix)
      (* produce a string of `n` zeros *)
      fun zeros n = CharVector.tabulate(n, fn _ => #"0")
      fun prependZeros (0, frags) = frags
        | prependZeros (n, frags) = zeros n :: frags
    in
    (* `raToString prec approx` converts the decimal approximation to a string.
     * The `prec` argument is the max number of significant digits to print
     *)
    fun raToString prec ({class, digits, exp, sign} : IEEEReal.decimal_approx) = let
          fun withSign s = if sign then "~" ^ s else s
          in
            case class
             of IEEEReal.INF => withSign "inf"
              | IEEEReal.NAN => "nan"
              | IEEEReal.ZERO => withSign "0.0"
              | _ => let
                  val nDigits = List.length digits
                  val (nDigits, digits, exp) = if (List.length digits > prec)
                        then let (* round to `prec` significant digits *)
                          fun round (0, _) = (false, 0, [])
                            | round (_, []) = (false, 0, [])
                            | round (n, d::ds) = (case round (n-1, ds)
                                 of (true, _, _) => let
                                      val d' = d+1
                                      in
                                        if (d' > 9) then (true, 0, []) else (false, 1, [d'])
                                      end
                                  | (false, nd, ds') => (false, nd+1, d::ds')
                                (* end case *))
                          in
                            case round(prec, digits)
                             of (true, _, _) => (1, [1], exp+1)
                              | (_, nd, ds) => (nd, ds, exp)
                            (* end case *)
                          end
                        else (nDigits, digits, exp)
                  (* compute the size (w/o sign) when formatting in fixed-point notation *)
                  val fixSize = if (exp >= nDigits)
                          (* <whole> <zeros> ".0" *)
                          then exp + 2
                        else if (exp > 0)
                          (* <whole> "." <frac> *)
                          then nDigits + 1
                          (* "0." <zeros> <digits> *)
                          else nDigits + 2 - exp
                  (* compute the size (w/o sign) when formatting in scientific notation
                   * "0." <digits> <exp>
                   *)
                  val sciSize = nDigits + 2 + expSize exp
                  in
                    if (fixSize <= sciSize)
                      then let (* fixed-point notation *)
                        val frags = if (exp >= nDigits)
                              then prependDigits(digits, prependZeros(exp-nDigits, [".0"]))
                            else if (exp > 0)
                              then let
                                val (w, f) = List.splitAt (digits, exp)
                                in
                                  prependDigits(w, "." :: prependDigits(f, []))
                                end
                              else "0." :: prependZeros(~exp, prependDigits(digits, []))
                        in
                          String.concat (if sign then "~" :: frags else frags)
                        end
                      else let (* scientific notation *)
                        val frags = "0." :: prependDigits(digits, ["e", Int.toString exp])
                        in
                          String.concat (if sign then "~" :: frags else frags)
                        end
                  end
            (* end case *)
          end

    val real64ToString = (raToString 12) o Real64.toDecimal
    end (* local *)

    (* printing for monomorphic primitive types (i.e., the types in BasicTypes) *)
    local
      fun wordPrefx s = "0wx" ^ s
      fun char2str obj = concat["#\"", Char.toString(Char.chr(Obj.toInt obj)), "\""]
      fun exn2str obj = General.exnName(Obj.toExn obj) ^ "(-)"
      val toStringTbl = [
              (BT.intTycon,             Int.toString o Obj.toInt),
              (BT.int32Tycon,           Int32.toString o Obj.toInt32),
              (BT.int64Tycon,           Int64.toString o Obj.toInt64),
              (BT.intinfTycon,          PrintUtil.formatIntInf o Unsafe.cast),
              (BT.wordTycon,            wordPrefx o Word.toString o Obj.toWord),
              (BT.word8Tycon,           wordPrefx o Word8.toString o Obj.toWord8),
              (BT.word32Tycon,          wordPrefx o Word32.toString o Obj.toWord32),
              (BT.word64Tycon,          wordPrefx o Word64.toString o Obj.toWord64),
              (BT.charTycon,            char2str),
(* FIXME: REAL32 *)
              (BT.realTycon,            real64ToString o Obj.toReal64),
              (BT.exnTycon,             exn2str),
              (BT.pointerTycon,         fn _ => "<cptr>"),
              (BT.stringTycon,          PrintUtil.formatString o Obj.toString),
(* FIXME: actually print the values *)
              (BT.chararrayTycon,       fn _ => "<chararray>"),
              (BT.word8vectorTycon,     fn _ => "<word8vector>"),
              (BT.word8arrayTycon,      fn _ => "<word8array>"),
              (BT.real64arrayTycon,     fn _ => "<real64array>"),
              (BT.arrowTycon,           fn _ => "<fn>"),
              (BT.contTycon,            fn _ => "<cont>")
            ]
    in
    fun primToString tyc = List.find (fn (tyc', _) => TU.eqTycon(tyc, tyc')) toStringTbl
    end (* local *)

    (* main function: ppObj: staticEnv -> ppstream -> (object * ty * int) -> unit *)
    fun ppObj env ppstrm = let
        fun ppValue (obj: object, ty: T.ty, depth: int) : unit =
            ppVal' (obj, ty, NONE, depth, noparen, noparen, [])

        and ppValShare (
              obj:object, ty:T.ty,
              membersOp: (T.tycon list * T.tycon list) option,
              depth:int, accu
            ) = ppVal' (obj, ty, membersOp, depth, noparen, noparen, accu)

        and ppVal' (_,_,_,0,_,_,_) = PP.string ppstrm  "#"
          | ppVal' (
              obj: object, ty: T.ty, membersOp: (T.tycon list * T.tycon list) option,
              depth: int, l: F.fixity, r: F.fixity, accu
            ) = ((case ty
               of T.VARty(ref(T.INSTANTIATED t)) =>
                    ppVal'(obj,t,membersOp,depth,r,l,accu)
                | T.POLYty{tyfun=T.TYFUN{body,arity},...} =>
                    if arity=0
                      then  ppVal'(obj, body,membersOp,depth,l,r,accu)
                      else let
                        val args = Obj.mkTuple (List.tabulate(arity, fn i => Obj.toObject 0))
                        val tobj : object -> object = Unsafe.cast obj
                        val res = tobj args
                        in
                          ppVal'(res, body, membersOp, depth, l, r, accu)
                        end
                | T.CONty(tyc as T.GENtyc { kind, stamp, eq, ... }, argtys) => (
                    case (kind, !eq)
                     of (T.PRIMITIVE, _) => (case primToString tyc
                           of SOME(_, fmt) => PP.string ppstrm (fmt obj)
                            | NONE => (* check for vector/array type constructors *)
                                if TU.eqTycon(tyc,BT.vectorTycon)
                                  then ppVector (
                                    Obj.toVector obj, hd argtys,
                                    membersOp, depth,
                                    !Control.Print.printLength, accu)
                                    handle Obj.Representation => PP.string ppstrm  "<primvec?>"
                                else if TU.eqTycon(tyc,BT.arrayTycon)
                                  then (printWithSharing ppstrm
                                    (obj,accu,
                                     fn (obj,accu) =>
                                        (case Obj.rep obj
                                          of Obj.PolyArray =>
                                             ppArray(Obj.toArray obj, hd argtys,
                                                     membersOp, depth,
                                                     !Control.Print.printLength, accu)
                                           | Obj.RealArray =>
                                             ppRealArray(Obj.toRealArray obj,
                                                         !Control.Print.printLength)
                                           | _ => bug "array (neither Real nor Poly)"
                                             ))
                                    handle Obj.Representation => PP.string ppstrm  "<primarray?>")
                                else PP.string ppstrm  "<prim?>"
                          (* end case *))
                      | (T.DATATYPE _, T.ABS) =>
                        (PPTable.pp_object ppstrm stamp obj
                         handle PP_NOT_INSTALLED => PP.string ppstrm  "-" )
                      | (T.DATATYPE{index,stamps,
                                    family as {members,...}, freetycs, root, stripped}, _) =>
                        if TU.eqTycon(tyc,BT.ulistTycon) then
                            ppUrList(obj,hd argtys,membersOp,depth,
                                     !Control.Print.printLength,accu)
                        else if TU.eqTycon(tyc,BT.suspTycon) then
                            PP.string ppstrm  "$$"  (* LAZY *)
                        else if TU.eqTycon(tyc,BT.listTycon) then
                            ppList(obj,hd argtys,membersOp,depth,
                                   !Control.Print.printLength,accu)
                        else if TU.eqTycon(tyc,BT.refTycon) then
                            (printWithSharing ppstrm
                             (obj,accu,
                              let val argtys' = interpArgs(argtys,membersOp)
                              in fn (obj,accu) =>
                                    ppDcon(obj,
                                           (Vector.sub(stamps,index),
                                            Vector.sub(members,index)),
                                           SOME([BT.refTycon],[]),argtys',
                                           depth,l,r,accu)
                              end))
                        else let val argtys' = interpArgs(argtys,membersOp)
                             in
                                 ppDcon(obj,(Vector.sub(stamps,index),
                                             Vector.sub(members,index)),
                                        SOME(transMembers (stamps, freetycs,
                                                           root, family)),
                                        argtys',depth,l,r,accu)
                             end
                      | _ => PP.string ppstrm "-"
                    (* end case *))
                | T.CONty(tyc as T.RECORDtyc [], _) => PP.string ppstrm  "()"
                | T.CONty(tyc as T.RECORDtyc labels, argtys) => if Tuples.isTUPLEtyc tyc
                    then ppTuple(Obj.toTuple obj, argtys, membersOp, depth, accu)
                    else ppRecord(Obj.toTuple obj, labels, argtys, membersOp, depth, accu)
                | T.CONty(tyc as T.DEFtyc _, _) =>
                    ppVal'(obj, TU.reduceType ty, membersOp, depth, l, r,accu)
                | T.CONty(tyc as T.RECtyc i,argtys) => (case membersOp
                      of SOME (memberTycs,_) =>
                           let val tyc' =
                                   List.nth(memberTycs,i)
                                   handle Subscript =>
                                    (PP.flushStream ppstrm;
                                     print "#ppVal':  ";
                                     print (Int.toString i);
                                     print " "; print(Int.toString(length memberTycs));
                                     print "\n";
                                     bug "ppVal': bad index for RECtyc")
                            in case tyc'
                                 of T.GENtyc { kind =
                                               T.DATATYPE{index,stamps,
                                                          family={members,...},...},
                                               ... } =>
                                    ppDcon(obj,(Vector.sub(stamps,index),
                                                Vector.sub(members,index)),
                                           membersOp, argtys,
                                           depth,l,r,accu)
                                  | _ => bug "ppVal': bad tycon in members"
                           end
                       | NONE => bug "ppVal': RECtyc with no members"
                     (* end case *))
                | T.CONty(tyc as T.FREEtyc i,argtys) => (case membersOp
                      of SOME (_, freeTycs) =>
                           let val tyc' =
                                   List.nth(freeTycs,i)
                                   handle Subscript =>
                                    (PP.flushStream ppstrm;
                                     print "#ppVal':  ";
                                     print (Int.toString i);
                                     print " ";
                                     print(Int.toString(length freeTycs));
                                     print "\n";
                                     bug "ppVal': bad index for FREEtyc")
                            in ppVal'(obj, T.CONty(tyc', argtys), membersOp,
                                      depth, l, r, accu)
                           end
                       | NONE => bug "ppVal': RECtyc with no members"
                     (* end case *))

                | _ => PP.string ppstrm  "-"
              (* end case *))
                handle e => raise e)

        and ppDcon(_,_,_,_,0,_,_,_) = PP.string ppstrm  "#"
          | ppDcon(obj:object, (stamp, {tycname,dcons,...}), membersOp : (T.tycon list * T.tycon list) option,
                   argtys, depth:int, l:F.fixity, r:F.fixity, accu) =
             PPTable.pp_object ppstrm stamp obj
                   (* attempt to find and apply user-defined pp on obj *)
             handle PP_NOT_INSTALLED =>
               if length dcons = 0 then PP.string ppstrm "-"
               else
                let val dcon as {name,domain,...} = switch(obj,dcons)
                    val dname = Symbol.name name
                 in case domain
                      of NONE => PP.string ppstrm dname
                       | SOME dom =>
                          let val fixity =
                                  Lookup.lookFix(env,Symbol.fixSymbol dname)
                              (* (??) may be inaccurate *)
                          val dom = TU.applyTyfun(T.TYFUN{arity=length argtys,body=dom},
                                                  argtys)
                          val dom = TU.headReduceType dom (* unnecessary *)
                          fun prdcon() = (case (fixity,dom)
                                 of (F.INfix _,T.CONty(domTyc as T.RECORDtyc _, [tyL,tyR])) =>
                                    let val (a, b) =
                                            case Obj.toTuple(decon(obj,dcon))
                                             of [a, b] => (a, b)
                                              | _ => bug "ppDcon [a, b]"
                                     in if Tuples.isTUPLEtyc domTyc
                                        then (PP.openHOVBox ppstrm (PP.Rel 0);
                                              ppVal'(a,tyL,
                                                     membersOp,
                                                     depth-1,F.NONfix,fixity,accu);
                                              PP.break ppstrm {nsp=1,offset=0};
                                              PP.string ppstrm  dname;
                                              PP.break ppstrm {nsp=1,offset=0};
                                              ppVal'(b,tyR,
                                                     membersOp,
                                                     depth-1,fixity, F.NONfix,accu);
                                              PP.closeBox ppstrm)
                                        else (PP.openHOVBox ppstrm (PP.Rel 2);
                                              PP.string ppstrm  dname;
                                              PP.break ppstrm {nsp=1,offset=0};
                                              ppVal'(decon(obj,dcon),dom,
                                                     membersOp, depth-1,
                                                     F.NONfix,F.NONfix,accu);
                                              PP.closeBox ppstrm)
                                    end
                                  | _ => (PP.openHOVBox ppstrm (PP.Rel 2);
                                          PP.string ppstrm  dname;
                                          PP.break ppstrm {nsp=1,offset=0};
                                          ppVal'(decon(obj,dcon),dom,membersOp,depth-1,
                                                 F.NONfix,F.NONfix,accu);
                                          PP.closeBox ppstrm)
                                (* end case *))
                          fun prpardcon() =
                              (PP.openHOVBox ppstrm (PP.Rel 0);
                               PP.string ppstrm  "("; prdcon(); PP.string ppstrm  ")";
                               PP.closeBox ppstrm)
                       in case(l,r,fixity)
                            of (F.NONfix,F.NONfix,_) => prpardcon()
                             | (F.INfix _,F.INfix _,_) => prdcon()
                               (* special case: only on first iteration, for no parens *)
                             | (_,_,F.NONfix) => prdcon()
                             | (F.INfix(_,p1),_,F.INfix(p2,_)) =>
                                 if p1 >= p2 then prpardcon()
                                 else prdcon()
                             | (_,F.INfix(p1,_),F.INfix(_,p2)) =>
                                 if p1 > p2 then prpardcon()
                                 else prdcon()
                      end
              end

        and ppList(obj:object, ty:T.ty, membersOp, depth:int, length: int,accu) =
            let fun list_case p =
                    case switch(p, listDcons)
                      of {domain=NONE,...} => NONE
                       | dcon => (case Obj.toTuple(decon(p, dcon)) of
                                      [a, b] => SOME(a, b)
                                    | _ => bug "ppList [a, b]")

               fun ppTail(p, len) =
                   case list_case p
                     of NONE => ()
                      | SOME(hd,tl) =>
                          if len <= 0 then (PP.string ppstrm  "...")
                          else (case list_case tl
                                 of NONE =>
                                      ppValShare (hd, ty, membersOp, depth-1,accu)
                                  | _ =>
                                      (ppValShare (hd, ty, membersOp, depth-1,accu);
                                       PP.string ppstrm  ",";
                                       PP.break ppstrm {nsp=0,offset=0};
                                       ppTail(tl,len-1)))

             in PP.openHOVBox ppstrm (PP.Rel 1);
                PP.string ppstrm  "[";
                ppTail(obj,length);
                PP.string ppstrm  "]";
                PP.closeBox ppstrm
            end

        and ppUrList(obj:object, ty:T.ty, membersOp, depth:int, length: int,accu) =
            let fun list_case p =
                    case switch(p, ulistDcons)
                      of {domain=NONE,...} => NONE
                       | dcon => (case Obj.toTuple(decon(p, dcon)) of
                                      [a, b] => SOME (a, b)
                                    | _ => bug "ppUrList [a, b]")

                fun ppTail(p, len) =
                   case list_case p
                     of NONE => ()
                      | SOME(hd,tl) =>
                          if len <= 0 then (PP.string ppstrm  "...")
                          else (case list_case tl
                                 of NONE =>
                                      ppValShare (hd, ty, membersOp, depth-1,accu)
                                  | _ =>
                                      (ppValShare (hd, ty, membersOp, depth-1,accu);
                                       PP.string ppstrm  ",";
                                       PP.break ppstrm {nsp=0,offset=0};
                                       ppTail(tl,len-1)))

             in PP.openHOVBox ppstrm (PP.Rel 1);
                PP.string ppstrm  "[ unrolled list ";
                (* ppTail(obj,length); *)
                PP.string ppstrm  "]";
                PP.closeBox ppstrm
            end

        and ppTuple (objs: object list, tys: T.ty list, membersOp, depth:int, accu) : unit =
            let fun ppFields ([f],[ty]) = ppValShare (f, ty, membersOp, depth-1, accu)
                  | ppFields (f::restf, ty::restty) =
                      (ppValShare (f, ty, membersOp, depth-1, accu);
                       PP.string ppstrm (",");
                       PP.break ppstrm {nsp=0,offset=0};
                       ppFields(restf,restty))
                  | ppFields ([], []) = ()
                  | ppFields _ = bug(concat[
                        "ppTuple.ppFields: arity mismatch; ", Int.toString(length objs),
                        " objects vs. ", Int.toString(length tys), " fields"
                      ])
             in PP.openHOVBox ppstrm (PP.Rel 1);
                PP.string ppstrm ("(");
                ppFields(objs, tys);
                PP.string ppstrm (")");
                PP.closeBox ppstrm
            end

        and ppRecord (objs: object list, labels: T.label list,
                     tys: T.ty list, membersOp, depth: int, accu) =
            let fun ppFields([f],[l],[ty]) =
                      (PP.openHVBox ppstrm (PP.Rel 2);
                       PP.string ppstrm (Symbol.name l);
                       PP.string ppstrm ("=");
                       ppValShare (f, ty, membersOp, depth-1, accu);
                       PP.closeBox ppstrm)
                  | ppFields(f::restf, l::restl, ty::restty) =
                      (PP.openHVBox ppstrm (PP.Rel 2);
                       PP.string ppstrm (Symbol.name l);
                       PP.string ppstrm ("=");
                       ppValShare (f,ty,membersOp,depth-1,accu);
                       PP.closeBox ppstrm;
                       PP.string ppstrm (",");
                       PP.break ppstrm {nsp=0,offset=0};
                       ppFields(restf,restl,restty))
                  | ppFields([],[],[]) = ()
                  | ppFields _ = bug "ppRecord.ppFields in ppval.sml"
             in PP.openHOVBox ppstrm (PP.Rel 1);
                PP.string ppstrm ("{");
                ppFields(objs,labels,tys);
                PP.string ppstrm ("}");
                PP.closeBox ppstrm
            end

        and ppVector (objs:object vector, ty:T.ty, membersOp, depth:int, length,accu) =
              let val vectorLength  = V.length objs
                  val (len, closing) =
                        if length >= vectorLength then
                          (vectorLength,fn _ => PP.string ppstrm "]")
                        else (length,fn sep => (PP.string ppstrm sep;
                                                PP.string ppstrm "...]"))
                  fun printRest(sep,breaker, index) =
                        if index >= len then closing sep
                        else (PP.string ppstrm  sep; breaker ();
                              ppValShare (V.sub (objs,index),ty,membersOp,
                                          depth-1,accu);
                              printRest (",",fn () => PP.break ppstrm {nsp=0,offset=0}, index + 1))
               in PP.openHOVBox ppstrm (PP.Rel 1);
                  PP.string ppstrm "#["; printRest("",fn () => (), 0);
                  PP.closeBox ppstrm
              end

        and ppArray (objs: object array, ty: T.ty, membersOp, depth: int, length, accu) =
              let val vectorLength  = Array.length objs
                  val (len, closing) =
                        if length >= vectorLength then
                          (vectorLength,fn _ => PP.string ppstrm "|]")
                        else (length,fn sep => (PP.string ppstrm sep;
                                                PP.string ppstrm "...|]"))
                  fun printRest(sep,breaker, index) =
                        if index >= len then closing sep
                        else (PP.string ppstrm  sep; breaker ();
                              ppValShare (Array.sub (objs,index),ty,membersOp,
                                          depth-1,accu);
                              printRest (",",fn () => PP.break ppstrm {nsp=0,offset=0}, index + 1))
               in PP.openHOVBox ppstrm (PP.Rel 1);
                  PP.string ppstrm "[|"; printRest("",fn () => (), 0);
                  PP.closeBox ppstrm
              end
        and ppRealArray (objs : Real64Array.array, length: int) = let
              val vectorLength  = Real64Array.length objs
              val (len, closing) =
                    if length >= vectorLength then
                      (vectorLength,fn _ => PP.string ppstrm "|]")
                    else (length,fn sep => (PP.string ppstrm sep;
                                            PP.string ppstrm "...|]"))
              fun printRest(sep,breaker, index) = if index >= len
                    then closing sep
                    else (
                      PP.string ppstrm  sep; breaker ();
                      PP.string ppstrm (Real.toString(Real64Array.sub(objs,index)));
                      printRest (",", fn () => PP.break ppstrm {nsp=0,offset=0}, index + 1))
              in
                PP.openHOVBox ppstrm (PP.Rel 1);
                PP.string ppstrm "[|"; printRest("",fn () => (), 0);
                PP.closeBox ppstrm
              end
        in
          ppValue
        end (* fun ppObj *)

  end (* structure PPObj *)
