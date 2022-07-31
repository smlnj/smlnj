(* ltykindchk.sml *)

(* Kind checker *)

signature LTYKINDCHK =
sig

  exception KindChk of string

  val tkAssertSubkind : Lty.tkind * Lty.tkind -> unit
  (* tkAssertSubkind(k1,k2): assert that k1 is a subkind of k2 *)

  val tkAssertIsMono : Lty.tkind * string -> unit
  (* assert that a kind is monomorphic *)

  val tkSel : Lty.tkind * int -> Lty.tkind
  (* select the ith element (0 based) from a kind sequence *)

  val tkApp : Lty.tkind * Lty.tkind list -> Lty.tkind
  (* tkApp(tk,tks): check the validity of an application of a
   * type function of kind `tk' to a list of arguments of kinds `tks'.
   * Returns the result kind if valid, raises KindChk otherwise.
   *)

  val tcKindCheckGen :   unit -> (Lty.tkindEnv -> Lty.tyc -> Lty.tkind)
  val tcKindVerifyGen :  unit -> (Lty.tkindEnv -> (Lty.tkind * Lty.tyc) -> unit)
  val teKindCheckGen :   unit -> (Lty.tkindEnv -> (Lty.tycEnv * int) -> unit)
  val ltKindCheckGen :   unit -> (Lty.tkindEnv -> Lty.lty -> Lty.tkind)
  val tcteKindCheckGen : unit -> (Lty.tkindEnv -> Lty.tyc -> Lty.tkind) *
                                 (Lty.tkindEnv -> (Lty.tkind * Lty.tyc) -> unit) *
                                 (Lty.tkindEnv -> (Lty.tycEnv * int) -> unit)

end (* signature LTYKINDCHK *)

structure LtyKindChk : LTYKINDCHK =
struct

structure LD = LtyDef
structure PP = PrettyPrint
structure PU = PPUtil
open Lty

fun bug s = ErrorMsg.impossible ("LtyKindChk:" ^ s)

val pd = ref 10
val with_pp = PP.with_default_pp

(********************************************************************
 *                      KIND-CHECKING ROUTINES                      *
 ********************************************************************)
exception KindChk of string

(* assert that k1 is a subkind of k2 *)
fun tkAssertSubkind (k1, k2) =
    if LD.tkSubkind (k1, k2) then ()
    else raise KindChk "Subkind assertion failed!"

(* assert that a kind is monomorphic *)
fun tkAssertIsMono (k,msg) =
    if LD.tkIsMono k then ()
    else raise KindChk ("Mono assertion failed! "^msg)

(* select the ith element (0 based) from a kind sequence *)
fun tkSel (tk, i) = 
  (case (tk_out tk)
    of (TK_SEQ ks) => 
       (List.nth(ks, i)
        handle Subscript => raise KindChk "Invalid TC_SEQ index")
     | _ => raise KindChk "Projecting out of sequence")


(* tkApp: tkind * tkind list
 * check the application of a type function of
 * kind `tk' to a list of arguments of kinds `tks'
 *)
fun tkApp (tk, tks) = 
  (case (tk_out tk)
    of TK_FUN(a, b) =>
         if LD.tksSubkind(tks, a) then b
         else raise KindChk "Param/Arg Tyc Kind mismatch"
     | _ => raise KindChk "Application of non-TK_FUN") 


(* Kind checking **************************************************)

(* Kind-checking naturally requires traversing type graphs.  to avoid
 * re-traversing bits of the dag, we use a dictionary to memoize the
 * kind of each tyc we process.
 *
 * The problem is that a tyc can have different kinds, depending on
 * the valuations of its free variables.  So this dictionary maps a
 * tyc to an association list that maps the kinds of the free
 * variables in the tyc (represented as a TK_SEQ) to the tyc's kind.
 *)
(* structure TcDict = BinaryMapFn
                     (struct
                        type ord_key = tyc
                        val compare = tc_cmp
		      end) *)
                       
(* lookupFreeVars eliminated *)

structure Memo :> sig
  type dict 
  val newDict         : unit -> dict
  val recallOrCompute : dict * tkindEnv * tyc * (unit -> tkind) -> tkind
end =
struct
    structure TcDict = RedBlackMapFn
                         (struct
                            type ord_key = tyc
                            val compare = tc_cmp
                          end)

    type dict = tkind TcDict.map ref
    val newDict : unit -> dict = ref o (fn () => TcDict.empty)

    fun recallOrCompute (dict, kenv, tyc, doit) =
        (* only cashe kinds of closed tycs, to avoid possibility
         * of free tvs that are not bound in kenv *)
        case tc_vs tyc
          of SOME [] =>  (* tyc is closed *)
             (case TcDict.find(!dict, tyc)
               of SOME tk => tk
                | NONE => 
                   let val tk = doit()
                    in dict := TcDict.insert(!dict, tyc, tk);
                       tk
                   end)
           | _ => (* not known to be closed. Have to compute,
                   * and can't cashe. *)
             doit()

end (* Memo *)

(* return the kind of a given tyc in the given kind environment *)
fun tcteKindCheckGen() =
let val dict = Memo.newDict()

    fun tcKindChk (kenv : tkindEnv) t = let
        (* default recursive invocation *)    
        val g = tcKindChk kenv
        (* how to compute the kind of a tyc *)
	fun mkI tycI =
            case tycI
             of TC_VAR (i, j) =>
                (tkLookup (kenv, i, j)
                 handle tkUnbound =>
                  (with_pp (fn s =>
                     (PU.pps s "KindChk: unbound tv: ";
                      PPLty.ppTyc (!pd) s (tc_inj tycI);
                      PP.newline s;
                      PU.pps s "kenv: ";
                      PP.openHOVBox s (PP.Rel 0);
                      PPLty.ppKindEnv (!pd) s kenv;
                      PP.newline s;
                      PP.closeBox s));
                   raise KindChk "unbound tv"))
              | TC_NVAR _ => 
                bug "TC_NVAR not supported yet in tcKindChk"
              | TC_PRIM pt =>
                LD.tkc_int (PrimTyc.pt_arity pt)
              | TC_FN(ks, tc) =>
                LD.tkc_fun(ks, tcKindChk (tkInsert (kenv,ks)) tc)
              | TC_APP (tc, tcs) =>
                tkApp (g tc, map g tcs)
              | TC_SEQ tcs =>
                LD.tkc_seq (map g tcs)
              | TC_PROJ(tc, i) =>
                tkSel(g tc, i)
              | TC_SUM tcs =>
                (List.app (fn tc => (tkAssertIsMono(g tc,"TC_SUM"))) tcs;
                 LD.tkc_mono)
              | TC_FIX {family={size=n, gen=tc, params=ts,...},index=i} =>
                let (* Kind check generator tyc *)
		    val k = g tc
		    (* Kind check freetycs *)
                    val nk =
                        case ts
                          of [] => k 
                           | _ => tkApp(k, map g ts)
                 in case (tk_out nk)
                     of TK_FUN(argk, resk) => 
                        let val argk' =
                                case argk
                                  of [x] => x
                                   | _ => LD.tkc_seq argk
                              (* "sequencize" the domain to make it comparable
                               * to resk *)
                        in
			    (* Kind check recursive tyc app ??*)
                            (* [KM ???] seems bogus if arg is a proper subkind,
                             * but probably ok if tkSubkind is really equivalence *)
                            if LD.tkSubkind(argk', resk) then (* order? *)
                                (if n = 1 then resk else tkSel(resk, i))
                            else raise KindChk "Recursive app mismatch"
                        end
                      | _ => raise KindChk "FIX with bad generator"
                end
              | TC_BOX tc =>
                (tkAssertIsMono (g tc, "TC_BOX");
                 LD.tkc_mono)
              | TC_TUPLE tcs =>
                (List.app (fn tc => (tkAssertIsMono(g tc, "TC_TUPLE"))) tcs;
                 LD.tkc_mono)
              | TC_ARROW (_, tcs1, tcs2) =>
                (List.app (fn tc => (tkAssertIsMono(g tc, "TC_ARROW domain"))) tcs1;
                 List.app (fn tc => (tkAssertIsMono(g tc, "TC_ARROW range"))) tcs2;
                 LD.tkc_mono)
              | TC_WRAP tc =>
                (tkAssertIsMono (g tc, "TC_WRAP");
                 LD.tkc_mono)
              | TC_PARROW _ => bug "unexpected TC_PARROW in tcKindChk"
           (* | TC_ENV _ => bug "unexpected TC_ENV in tcKindChk" *)
	      | TC_ENV(body, 0, j, teEmpty) => 
		  (tcKindChk (List.drop(kenv,j)) body 
		   handle Subscript => 
                     (if j < 0 then print ("KindChk: negative j: "^Int.toString j^"\n")
                      else if j >= (length kenv) then
                          (print ("KindChk: drop to large: "^Int.toString j^
                                  ", |kenv| = "^Int.toString(length kenv)^"\n"))
                      else ();
		      bug "KindChk: TC_ENV: dropping frames"))
	      | TC_ENV(body, i, j, env) =>
		  (let val kenv' = 
			   List.drop(kenv, j)
			   handle Subscript => 
                               (if j < 0 then (print "j = "; print(Int.toString j);
                                               print "\n")
                                else ();
                                bug "[TC_ENV]: dropping too many frames")
		       fun bindToKinds(Lamb(_,ks)) = ks
			 | bindToKinds(Beta(_,_,ks)) = ks
		       fun addBindToKEnv(b,ke) = 
			   bindToKinds b :: ke
		       val bodyKenv = 
			   foldr addBindToKEnv kenv' (teToBinders env)
		   in teKindChk kenv (env,j);
		      tcKindChk bodyKenv body
		   end) 
            (*  | TC_IND _ =>  bug "unexpected TC_IND in tcKindChk" *)
	      | TC_IND(newtyc, oldtycI) =>
		  let val newtycknd = g newtyc
		  in   
		      if tk_eq(newtycknd, mkI oldtycI) 
		      then newtycknd
		      else bug "tcKindChk[IND]: new and old kind mismatch"
		  end 
              | TC_CONT _ => bug "unexpected TC_CONT in tcKindChk"

        fun mk () =
	    mkI (tc_out t)
    in
        Memo.recallOrCompute (dict, kenv, t, mk)
        handle tkUnbound => raise KindChk "tkUnbound"
    end (* function tcKindChk *)

    and teKindChk(kenv: tkindEnv) (env: tycEnv, j: int) : unit =
	let 
	    fun chkBinder(Lamb _) = ()
	      | chkBinder(Beta(j',args,ks)) = 
		let 
		    val kenv' = List.drop(kenv, j-j')
		    val argks = map (fn t => tcKindChk kenv' t) args
		in if LD.tksSubkind(ks, argks)
		   then ()
		   else bug "teKindChk: Beta binder kinds mismatch"
		end
		handle Subscript => 
		       bug "tcKindChk[Env]: dropping too many frames"
	in app chkBinder (teToBinders env)
	end (* function teKindChk *)

    (* assert that the kind of `tc' is a subkind of `k' in `kenv' *)
    fun tcKindVer kenv (k, tc) =
        tkAssertSubkind (tcKindChk kenv tc, k)
in
    (tcKindChk, tcKindVer, teKindChk)
end (* function tcteKindCheckGen *)

fun tcKindCheckGen() = 
    #1(tcteKindCheckGen())
      
fun tcKindVerifyGen() =
    #2(tcteKindCheckGen())
 
fun teKindCheckGen() =
    #3(tcteKindCheckGen())
 
(* ltKindCheckGen : unit -> tkindEnv -> lty -> tkind *)
fun ltKindCheckGen () = 
let val (tcKindChk, _, teKindChk) = tcteKindCheckGen()
    fun ltyIChk (kenv : tkindEnv) (ltyI : ltyI) =
        (case ltyI 
          of LT_TYC(tyc) => tcKindChk kenv tyc
           | LT_STR(ltys) => LD.tkc_seq(map (ltyChk' kenv) ltys)
           | LT_FCT(paramLtys, rngLtys) => 
               let val paramks = map (ltyChk' kenv) paramLtys
                in LD.tkc_fun(paramks, LD.tkc_seq(map (ltyChk' kenv) rngLtys))
               end
           | LT_POLY(ks, ltys) => 
               LD.tkc_seq(map (ltyChk' (tkInsert(kenv,ks))) ltys)
               (* ??? *)
           | LT_CONT(ltys) => 
               LD.tkc_seq(map (ltyChk' kenv) ltys)
           | LT_IND(newLty, oldLtyI) =>
               let val newLtyKnd = (ltyChk' kenv) newLty
               in if tk_eq(newLtyKnd, ltyIChk kenv oldLtyI)
                  then newLtyKnd
                  else bug "ltyChk[IND]: kind mismatch"
               end
           | LT_ENV(body, i, j, env) =>
               (* Should be the same as checking TC_ENV and 
                * therefore the two cases should probably just
                * call the same helper function *)
               (let val kenv' = 
                        List.drop(kenv, j)
                        handle Subscript => 
                               (if j < 0 then (print "j = "; print(Int.toString j);
                                               print "\n")
                                else ();
                                bug "[LT_ENV]: dropping too many frames")
                    fun bindToKinds(Lamb(_,ks)) = ks
                      | bindToKinds(Beta(_,_,ks)) = ks
                    fun addBindToKEnv(b,ke) = 
                        tkInsert(ke,bindToKinds b)
                    val bodyKenv = 
                        foldr addBindToKEnv kenv' (teToBinders env)
                in teKindChk kenv (env,j);
                   ltyChk' bodyKenv body
                end))
    and ltyChk' kenv lty =
         ltyIChk kenv (lt_out lty)
         handle x => 
           (with_pp (fn ppstrm => (PPLty.ppLty (!pd) ppstrm lty;
                                   PP.newline ppstrm));
            raise x)
 in ltyChk'
end (* function ltKindCheckGen *)	   

end (* structure LtyKindChk *)
