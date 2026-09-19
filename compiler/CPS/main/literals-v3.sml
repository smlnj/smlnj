(* literals-v3.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This file implements support for heap-allocated literals.  Our approach
 * is to split out the literals from the CPS representation and create a
 * bytecode program that the runtime execures to allocate the literals.
 * This process involves several steps:
 *
 *      1) the CPS code is analyzed to identify literals that need to be lifted
 *      2) a literal-bytecode program is generated that will be used to construct
 *         the literal vector when the module is instantiated
 *      3) the CPS code is rewritten to replace references to literal values
 *         with selects from the literal vector
 *
 * The implementation of the bytecode interpreter for the literal language
 * is in base/runtime/gc/build-literals-v3.c.  A description of the bytecode
 * language is at https://github.com/smlnj/.github/wiki/Literals-v3
 *)

signature LITERALS =
  sig

    (** `litsplit f` takes a CPS function and splits out the heap-allocated
     * literal values from it.  At runtime, these literals will be accessed via
     * a record of literals that is allocated by the runtime system.  This
     * function returns a rewriten version of its argument that accesses
     * literals from the record and a byte-vector that encodes the program
     * for generating the literals.
     *)
    val split : CPS.function -> CPS.function * Word8Vector.vector

  end

structure Literals : LITERALS =
  struct

    structure LV = LambdaVar
    structure LVTbl = LV.Tbl
    structure WordTbl = WordHashTable
    structure C = CPS
    structure P = C.P
    structure BC = LiteralBytecode

    fun bug msg = ErrorMsg.impossible ("Literals: "^msg)

    val debugFlg = Control.CG.debugLits
    val say = Control.Print.say

    fun hashRConst {ty=32, rval} = RealLit.hash rval + 0w179
      | hashRConst {ty=64, rval} = RealLit.hash rval + 0w283
      | hashRConst _ = bug "bogus real-const size"

    fun sameRConst ({ty=t1, rval=rv1} : int RealConst.t, {ty=t2, rval=rv2}) =
          (t1 = t2) andalso RealLit.same(rv1, rv2)

  (****************************************************************************
   *                    LIFTING LITERALS ON CPS                               *
   ****************************************************************************)

    datatype obj
      (* records, raw records, mixed records, and vectors *)
      = RECORD of C.record_kind * literal list
      | STRING of string

    and literal
      = OBJ of {                        (* heap-allocated literal values *)
            refCnt : int ref,           (* count of uses of this literal value from
                                         * other literals; when > 1, then we have
                                         * shared structure.
                                         *)
            useCnt : int ref,           (* count of all uses of this literal.  When
                                         * this count is > refCnt, then the literal
                                         * will need to be bound to a variable in the
                                         * residual program.
                                         *)
            id : word,                  (* unique ID *)
            value : obj,                (* the representation of the object *)
            ty : C.cty                  (* the object's type *)
          }
      | IMMED of C.intty IntConst.t     (* immediate integer/word literal *)
      | ENUM of int                     (* data-constructor tags *)
      | REAL of int RealConst.t

    (* count a use of a literal *)
    fun useLit (lit as OBJ{useCnt, ...}) = (useCnt := !useCnt + 1)
      | useLit _ = ()

    (* count a reference to a literal from another literal record *)
    fun refUseLit (lit as OBJ{useCnt, refCnt, ...}) = (
          useCnt := !useCnt + 1;
          refCnt := !refCnt + 1)
      | refUseLit _ = ()

    (* is a literal used as value outside of being part of another literal? *)
    fun litIsUsed (OBJ{refCnt, useCnt, ...}) = (!refCnt < !useCnt)
      | litIsUsed _ = bug "impossible"

    (* is a literal shared?  This happens when its refCnt is > 1
     * or when its refCnt = 1 and its useCnt > 1.
     *)
    fun litIsShared (OBJ{refCnt=ref rc, useCnt, ...}) =
          (rc > 1) orelse ((rc = 1) andalso (!useCnt > 1))
      | litIsShared _ = bug "impossible"

    (* print a list of "top-level" literals (for debugging purposes) *)
    fun printLits (lits : literal list) = let
          val id2s = Word.fmt StringCvt.DEC
          fun prIndent 0 = ()
            | prIndent n = (say "  "; prIndent(n-1))
          fun prObj indent (RECORD(rk, args), suffix) = (
                case rk
                 of C.RK_VECTOR => say(concat["VECTOR ", suffix, "\n"])
                  | C.RK_RECORD => say(concat["RECORD ", suffix, "\n"])
                  | C.RK_MIXED{ptrLen, rawLen} => say(concat["MIXED ", suffix, "\n"])
                  | C.RK_RAWBLOCK => say(concat["RAWBLOCK ", suffix, "\n"])
                  | _ => raise Fail "bogus record kind"
                (* end case *);
                List.app (prLiteral (indent+1)) lits)
(* TODO: trim large strings *)
            | prObj _ (STRING s, suffix) = say (concat[
                  "STRING ", suffix, " \"", String.toString s, "\" ", suffix, "\n"
                ])
          and prLiteral indent lit = (
                prIndent indent;
                case lit
                 of OBJ{id, refCnt, useCnt, value, ...} => prObj indent (value, concat[
                        "#", id2s id, " ", Int.toString(!refCnt), "/",
                        Int.toString(!useCnt)
                      ])
                  | IMMED{ty={sz, tag=true}, ival} => say(concat[
                        "INT63 ", IntInf.toString ival, "\n"
                      ])
                  | IMMED{ty={sz, ...}, ival} => say(concat[
                        "RAWINT", Int.toString sz, " ", IntInf.toString ival, "\n"
                      ])
                  | ENUM n => say(concat["ENUM ", Int.toString n, "\n"])
                  | REAL{rval, ...} => say(concat["REAL ", RealLit.toString rval, "\n"])
                (* end case *))
          fun prSlot (i, OBJ{value, ...}) = (
                say (StringCvt.padLeft #" " 4 (Int.toString i) ^ ": ");
                prObj 3 (value, ""))
            | prSlot (i, _) = bug "expected top-level OBJ"
          in
            List.appi prSlot lits
          end (* printLits *)
    fun printReals (reals : int RealConst.t list) = let
          fun prSlot (i, {ty, rval}) = say (concat[
                  StringCvt.padLeft #" " 4 (Int.toString i), ": REAL",
                  Int.toString ty, " ", RealLit.toString rval, "\n"
                ])
          in
            List.appi prSlot reals
          end

    (* an environment for tracking literals *)
    structure LitEnv : sig

        type t

        (* a variable that is bound to a literal is either used to build a literal
         * record, in which case the bool is false, or is used as an argument to
         * some other operation (including non-literal records).
         *)
        type var_info = bool * literal

        (* create a new environment *)
        val new : unit -> t
        (* add a literal record value to the environment *)
        val addRecord : t -> C.record_kind * C.value list * C.lvar -> unit
        (* `isConst lenv v` returns true when either `v` is a variable bound to a
         * literal or `v` is a constant value.
         *)
        val isConst : t -> C.value -> bool
        (* record the use of a value in a non-literal context *)
        val useValue : t -> C.value -> unit

        (* return true if there are no literals defined in the environment *)
        val isEmpty : t -> bool
        (* get the literal objects and reals that are used outside of constructing
         * literal records.
         *)
        val getLiterals : t -> {
                usedLits : literal list,
                realLits : int RealConst.t list
              }
        (* return the literal that a variable is bound to *)
        val findVar : t -> C.lvar -> var_info option
        (* get the index of a real literal *)
        val lookupReal : t -> int RealConst.t -> int
        (* find a string literal *)
        val lookupString : t -> string -> literal

      (* return a list of the variables that are bound to top-level literalsn paired
       * with their binding (for debugging).
       *)
	val boundVars : t -> (C.lvar * literal) list

      end = struct

        (* hash keys are the heap-allocated literals *)
        type key = obj

(* TODO: use the Hash module from the SML/NJ Library *)
        fun hashObj (RECORD(rk, lits)) = let
              fun hashArg (OBJ{id, ...}, h) = 0w3 * id + 0w293 + h
                | hashArg (IMMED ic, h) = hashIConst ic + h
                | hashArg (ENUM n, h) = 0w3 * Word.fromInt n + 0w157 + h
                | hashArg (REAL rc, h) = hashRConst rc + h
              val h0 = (case rk
                     of C.RK_VECTOR => 0w197
                      | C.RK_RECORD => 0w313
                      | C.RK_MIXED{ptrLen, rawLen} => 0w439
                      | C.RK_RAWBLOCK => 0w571
                      | _ => bug("unexpected record kind " ^ PPCps.rkToString rk)
                    (* end case *))
              in
                List.foldl hashArg h0 lits
              end
          | hashObj (STRING s) = HashString.hashString s + 0w419
        and hashIConst {ty={tag=true, ...}, ival} =
              Word.fromLargeInt ival + 0w157
          | hashIConst {ty={sz, ...}, ival} =
              Word.fromLargeInt ival + Word.fromInt sz + 0w257

        fun sameObj (RECORD(rk1, args1), RECORD(rk2, args2)) = let
              fun sameLit (OBJ{useCnt=u1, ...}, OBJ{useCnt=u2, ...}) = (u1 = u2)
                | sameLit (IMMED ic1, IMMED ic2) = sameIConst (ic1, ic2)
                | sameLit (ENUM n1, ENUM n2) = (n1 = n2)
                | sameLit (REAL rc1, REAL rc2) = sameRConst (rc1, rc2)
                | sameLit _ = false
              in
                (rk1 = rk2) andalso ListPair.allEq sameLit (args1, args2)
              end
          | sameObj (STRING s1, STRING s2) = (s1 = s2)
          | sameObj _ = false
        and sameIConst ({ty={sz=s1, ...}, ival=iv1}, {ty={sz=s2, ...}, ival=iv2}) =
              (s1 = s2) andalso (iv1 = iv2)

        structure LTbl = HashTableFn(
          struct
            type hash_key = obj
            val hashVal = hashObj
            val sameKey = sameObj
          end)

        structure RTbl = HashTableFn(
          struct
            type hash_key = int RealConst.t
            val hashVal = hashRConst
            val sameKey = sameRConst
          end)

        type var_info = bool * literal

        datatype t = LE of {
            lits : literal LTbl.hash_table,     (* table of unique heap-allocated
                                                 * literals in the module
                                                 *)
            reals : int RTbl.hash_table,        (* a mapping from real literals that
                                                 * appear outside of a heap-allocated
                                                 * literal to unique IDs
                                                 *)
            vMap : var_info LV.Tbl.hash_table   (* map from variables to the literals
                                                 * that they are bound to
                                                 *)
          }

        fun new () = LE{
                lits = LTbl.mkTable(32, Fail "LitTbl"),
                reals = RTbl.mkTable(32, Fail "RealTbl"),
                vMap = LV.Tbl.mkTable(32, Fail "VarTbl")
              }

        fun litExists (LE{lits, ...}) = LTbl.inDomain lits

        fun findLit (LE{lits, ...}) = LTbl.find lits

        (* insert a heap-allocated object into the literal environemt.  We assume
         * that the object is not already in the table and we return the literal
         * value.
         *)
        fun newLit (LE{lits, ...}) = let
              val insert = LTbl.insert lits
              in
                fn (obj : obj, cty) => let
                    val lit = OBJ{
                            useCnt = ref 0, refCnt = ref 0,
                            id = Word.fromInt(LTbl.numItems lits),
                            value = obj,
                            ty = cty
                          }
                    in
                      insert (obj, lit);
                      lit
                    end
              end (* newLit *)

        fun insertString env = let
              val find = findLit env
              val newLit = newLit env
              in
                fn s => (case find (STRING s)
                     of SOME lit => lit
                      | NONE => newLit (STRING s, C.ptrTy)
                    (* end case *))
              end

        fun lookupReal (LE{reals, ...}) = RTbl.lookup reals

        fun insertReal (LE{reals, ...}) = let
              val find = RTbl.find reals
              val insert = RTbl.insert reals
              in
                fn rc => (case find rc
                     of NONE => insert (rc, RTbl.numItems reals)
                      | SOME _ => ()
                    (* end case *))
              end

        fun lookupString env = let
              val find = findLit env
              in
                fn s => (case find (STRING s)
                   of SOME lit => lit
                    | NONE => bug "string not found"
                  (* end case *))
              end

        fun findVar (LE{vMap, ...}) = LV.Tbl.find vMap

        fun insertVar (LE{vMap, ...}) = LV.Tbl.insert vMap

        fun isConst (LE{vMap, ...}) = let
              val inDomain = LV.Tbl.inDomain vMap
              in
                fn (C.VAR x) => inDomain x
                 | (C.LABEL _) => bug "unexpected LABEL"
                 | (C.NUM n) => true
                 | (C.ENUM _) => true
                 | (C.REAL r) => true
                 | (C.STRING s) => true
                 | C.VOID => false
              end

        fun addRecord env = let
              val findLit = findLit env
              val findVar = findVar env
              val insVar = insertVar env
              val insReal = insertReal env
              val insStr = insertString env
              val newLit = newLit env
              (* resolve the record fields to their corresponding literal reps *)
              fun resolveField (C.VAR x) = (case findVar x
                      of SOME(_, lit) => lit
                       | NONE => bug "expected literal"
                     (* end case *))
                | resolveField (C.LABEL _) = bug "unexpected LABEL value"
                | resolveField (C.NUM n) = IMMED n
                | resolveField (C.ENUM n) = ENUM n
                | resolveField (C.REAL rc) = (insReal rc; REAL rc)
                | resolveField (C.STRING s) = insStr s
                | resolveField C.VOID = bug "unexpected VOID value"
              in
                fn (rk, fields, v) => let
                    val flds = List.map resolveField fields
                    val obj = RECORD(rk, flds)
                    val lit = (case findLit obj
                           of SOME lit => lit
                            | NONE => let
                                val cty = (case rk
                                       of C.RK_VECTOR => C.ptrTy
                                        | C.RK_RECORD => C.rPtrTy(length flds)
                                        | C.RK_MIXED rep => C.PTRt(C.RPT rep)
                                        | C.RK_RAWBLOCK => C.fPtrTy(length flds)
                                        | _ => bug "unexpected record kind"
                                      (* end case *))
                                val lit = newLit (obj, cty)
                                in
                                  (* record the references and uses of the fields *)
                                  List.app refUseLit flds;
                                  lit
                                end
                          (* end case *))
                    in
                      insVar (v, (false, lit))
                    end
              end

        fun useValue env = let
              val findVar = findVar env
              val insReal = insertReal env
              val insStr = insertString env
              val insVar = insertVar env
              in
                fn (C.VAR x) => (case findVar x
                       of SOME(flg, lit) => (
                            useLit lit;
                            if flg then () else insVar (x, (true, lit)))
                        | NONE => ()
                      (* end case *))
                 | (C.LABEL _) => bug "unexpected LABEL"
                 | (C.NUM n) => ()
                 | (C.ENUM _) => ()
                 | (C.REAL rc) => insReal rc
                 | (C.STRING s) => useLit (insStr s)
                 | C.VOID => ()
              end

        fun isEmpty (LE{lits, reals, ...}) =
              (LTbl.numItems lits + RTbl.numItems reals = 0)

        fun getLiterals (LE{lits, reals, ...}) = {
                usedLits = List.filter litIsUsed (LTbl.listItems lits),
                realLits = RTbl.listKeys reals
              }

	fun boundVars (LE{vMap, ...}) =
	      LV.Tbl.foldi
		(fn (x, (true, lit), acc) => (x, lit)::acc | (_, _, acc) => acc)
		  [] vMap

      end (* structure LitEnv *)

    (****************************************************************************
     *                                FIRST PASS                                *
     ****************************************************************************)

    (* The first pass initializes the literal table by walking the CPS module.  After
     * this pass, we have identified any literal value that needs to be included in the
     * literal section.  Furthermore, we have identified which literal values are used
     * in non-literal contexts.
     *)
    fun identifyLiterals body = let
          val env = LitEnv.new()
          val isConst = LitEnv.isConst env
          val useValue = LitEnv.useValue env
          val useValues = List.app useValue
          val addRecord = LitEnv.addRecord env
          fun addWrap (nk, u, v) = let
                val rk = (case nk
                       of P.INT 64 => C.RK_RAWBLOCK
                        | P.UINT 64 => C.RK_RAWBLOCK
                        | P.FLOAT _ => C.RK_RAWBLOCK
                        | _ => raise Fail("unexpected wrap of " ^ NumKind.toString nk)
                      (* end case *))
                in
                  addRecord (rk, [u], v)
                end
          fun fieldToValue (u, C.OFFp 0) = u
            | fieldToValue _ = bug "unexpected access in field"
        (* process a CPS function *)
          fun doFun (fk, f, vl, cl, e) = doExp e
        (* process a CPS expression *)
          and doExp ce = (case ce
                 of C.RECORD(rk, fields, v, e) => let
                      val ul = List.map fieldToValue fields
                      in
                        if List.all isConst ul
                          then addRecord (rk, ul, v)
                          else useValues ul;
                        doExp e
                      end
                  | C.SELECT(i, u, v, t, e) => (useValue u; doExp e)
                  | C.OFFSET _ => bug "unexpected OFFSET in doExp"
                  | C.APP(u, ul) => useValues ul
                  | C.FIX(fns, e) => (List.app doFun fns; doExp e)
                  | C.SWITCH(u, v, es) => List.app doExp es
                  | C.BRANCH(p, ul, v, e1, e2) => (useValues ul; doExp e1; doExp e2)
                  | C.SETTER(p, ul, e) => (useValues ul; doExp e)
                  | C.LOOKER(p, ul, v, t, e) => (useValues ul; doExp e)
                  | C.ARITH(p, ul, v, t, e) => (useValues ul; doExp e)
                  | C.PURE(P.WRAP nk, [u], v, t, e) => if isConst u
                      then (addWrap(nk, u, v); doExp e)
                      else doExp e
                  | C.PURE (p, ul, v, t, e) => (useValues ul; doExp e)
                  | C.RCC (k, l, p, ul, vtl, e) => (useValues ul; doExp e)
                (* end case *))
          in
            doExp body;
            env
          end

    (****************************************************************************
     *                               SECOND PASS                                *
     ****************************************************************************)

    (* build the representation of the literals; return a table mapping literal IDs
     * to their locations, the bytecode for building the literal vector, and a boolean
     * that is true if there is a real-literal vector.
     *)
    fun buildLiterals env = let
          (* table to track shared literals (indexed by CPS value) *)
          val sharedLitTbl = WordTbl.mkTable (32, Fail "sharedLitTbl")
          val insertSharedLit = let
                val insert = WordTbl.insert sharedLitTbl
                in
                  fn id => let val loc = WordTbl.numItems sharedLitTbl
                      in
                        insert (id, loc); loc
                      end
                end
          val findSharedLit = WordTbl.find sharedLitTbl
          (* a table to map object IDs to slots in the top-level literal record *)
          val slotTbl : (int * C.cty) WordTbl.hash_table =
                WordTbl.mkTable (16, Fail "slot table")
          val assignSlot = let
                val insert = WordTbl.insert slotTbl
                in
                  fn (id, ty) => insert (id, (WordTbl.numItems slotTbl, ty))
                end
          (* a function for mapping CPS values to slots in the top-level literal
           * vector
           *)
          val slotForValue : C.value -> (int * C.cty) option = let
                val findVar = LitEnv.findVar env
                val lookupReal = LitEnv.lookupReal env
                val lookupString = LitEnv.lookupString env
                val findSlot = WordTbl.find slotTbl
                fun getSlot (OBJ{id, ty, ...}) = (case findSlot id
                       of NONE => bug ("no slot for " ^ Word.fmt StringCvt.DEC id)
                        | someSlot => someSlot
                      (* end case *))
                  | getSlot _ = bug "getSlot for non-object"
                in
                  fn (C.VAR x) => (case findVar x
                       of SOME(true, lit) => getSlot lit
                        | SOME _ => bug "impossible: bad variable"
                        | NONE => NONE
                      (* end case *))
                   | (C.REAL rc) => SOME(lookupReal rc, C.FLTt(#ty rc))
                   | (C.STRING s) => getSlot (lookupString s)
                   | _ => NONE
                end
          (* generate code to create a record *)
          fun genRecord (rk, lits, code) = let
                val code = List.foldl genLiteral code lits
                in
                  case rk
                   of C.RK_VECTOR => BC.VEC(List.length lits) :: code
                    | C.RK_RECORD => BC.RECORD(List.length lits) :: code
                    | C.RK_MIXED rep => BC.MIXED rep :: code
                    | C.RK_RAWBLOCK => BC.RAWBLOCK(List.length lits) :: code
                    | _ => bug "unexpected record kind"
                  (* end case *)
                end
          and genLiteral (OBJ{refCnt, useCnt, value, id, ...}, code) = if (!refCnt > 1)
                then (case findSharedLit id
                   of SOME slot => BC.LOAD slot :: code
                    | NONE => let
                        val slot = insertSharedLit id
                        in
                          case value
                           of RECORD(rk, lits) =>
                                BC.SAVE slot :: genRecord(rk, lits, code)
                            | STRING s => BC.SAVE slot :: BC.STR8 s :: code
                          (* end case *)
                        end
                  (* end case *))
                else (case value
                   of RECORD(rk, lits) => genRecord(rk, lits, code)
                    | STRING s => BC.STR8 s :: code
                  (* end case *))
            | genLiteral (IMMED{ty={sz=63, ...}, ival}, code) =
                BC.INT63 ival :: code
            | genLiteral (IMMED{ty={sz=64, ...}, ival}, code) =
                BC.RAWINT64 ival :: code
            | genLiteral (ENUM n, code) =
                BC.INT63(IntInf.fromInt n) :: code
            | genLiteral (REAL{ty=64, rval}, code) =
                BC.REAL64 rval :: code
            | genLiteral _ = bug "bogus literal"
          fun genTopLiteral (lit as OBJ{id, ty, ...}, code) = (
                assignSlot (id, ty);
                genLiteral (lit, code))
            | genTopLiteral _ = bug "bogus top-level literal"
          (* get the literal values from the environment *)
          val {usedLits, realLits} = LitEnv.getLiterals env
          (* sort the used literals by order of their definition *)
          val usedLits = let
                fun gt (OBJ{id=a, ...}, OBJ{id=b, ...}) = (a > b)
                  | gt _ = bug "unexpected immediate literal"
                in
                  ListMergeSort.sort gt usedLits
                end
          val numLits = List.length usedLits
          val numReals = List.length realLits
          (* generate the code for the top-level literals *)
          val code = List.foldl genTopLiteral [] usedLits
          (* add code to generate the real literals (if any) *)
          val code = let
(* REAL32: FIXME *)
                fun genReal ({ty=64, rval}, code) = BC.REAL64 rval :: code
                  | genReal _ = bug "bogus real literal"
                in
                  List.foldl genReal code realLits
                end
          (* create the top-level literal record *)
          val (code, litPtrTy) = (case (numLits, numReals)
                 of (0, 0) => bug "unexpected empty literal environment"
                  | (0, _) => (BC.RAWBLOCK numReals :: code, C.fPtrTy numReals)
                  | (_, 0) => (BC.RECORD numLits :: code, C.rPtrTy numLits)
                  | (n, m) => (
                      BC.MIXED{ptrLen=n, rawLen=m} :: code,
                      C.PTRt(C.RPT{ptrLen=n, rawLen=m})
                    )
                (* end case *))
          val code = List.rev (BC.RETURN :: code)
          in
            if !debugFlg
              then let
                fun prBV (x, OBJ{id, ...}) = (
                      say(concat["LET ", LV.lvarName x, " : "]);
                      case WordTbl.find slotTbl id
                       of NONE => say "? = <no slot>\n"
                        | SOME(n, ty) => say(concat[
                              CPSUtil.ctyToString ty, " = slot-", Int.toString n, "\n"
                            ])
                      (* end case *))
                fun prByte (i, w) = (
                      say(StringCvt.padLeft #"0" 2 (Word8.toString w));
                      if (i mod 16 = 15)
                        then say "\n"
                        else say " ")
                in
                  say "==========\n";
                  printLits usedLits;
                  say "==========\n";
                  printReals realLits;
                  say "==========\n";
                  List.app prBV (LitEnv.boundVars env);
                  say "==========\n";
                  LiteralBytecode.dump code;
                  say "==========\n"
                end
              else ();
            (code, slotForValue, litPtrTy)
          end

    (****************************************************************************
     *                               THIRD PASS                                 *
     ****************************************************************************)

(* TODO: keep an environment of available literal bindings to avoid redundant SELECTs *)
  (* rewrite the program, removing unused variables *)
    fun liftLiterals (env, slotForValue, litVec, body) = let
          val findVar = LitEnv.findVar env
        (* rewrite a value *)
          fun rewriteValue (u, k : C.value -> C.cexp) = (case slotForValue u
                 of NONE => k u
                  | SOME(slot, ty) => let
                      val v = LambdaVar.mkLvar()
                      in
                        C.SELECT(slot, litVec, v, ty, k(C.VAR v))
                      end
                (* end case *))
handle ex => (say(concat["rewriteValue (", PPCps.value2str u, ", -): error\n"]); raise ex)
        (* rewrite a list of values *)
          fun rewriteValues (ul, k : C.value list -> C.cexp) = let
                fun rewrite ([], ul') = k(List.rev ul')
                  | rewrite (u::ul, ul') = rewriteValue (u, fn u' => rewrite(ul, u'::ul'))
                in
                  rewrite (ul, [])
                end
        (* rewrite a list of record fields *)
          fun rewriteFields (ul, k : (C.value * C.accesspath) list -> C.cexp) = let
                fun rewrite ([], ul') = k(List.rev ul')
                  | rewrite ((u, acc)::ul, ul') =
                      rewriteValue (u, fn u' => rewrite(ul, (u',acc)::ul'))
                in
                  rewrite (ul, [])
                end
handle ex => (say "rewriteFields\n"; raise ex)
        (* rewrite a variable that might be bound to a record literal *)
          fun rewriteVar (x, mkOrig, k) = (case findVar x
                 of SOME _ => k()
                  | _ => mkOrig()
                (* end case *))
handle ex => (say(concat["rewriteVar (", LV.lvarName x, ", -, -): error\n"]); raise ex)
        (* process a CPS function *)
          fun doFun (fk, f, vl, cl, e) = (fk, f, vl, cl, doExp e)
        (* process a CPS expression *)
          and doExp ce = (case ce
                 of C.RECORD(rk, ul, v, e) =>
                      rewriteVar (v,
                        fn () => rewriteFields (ul, fn ul' => C.RECORD(rk, ul', v, doExp e)),
                        fn () => doExp e)
                  | C.SELECT(i, u, v, t, e) =>
                      rewriteValue(u, fn u' => C.SELECT(i, u', v, t, doExp e))
                  | C.OFFSET _ => bug "unexpected OFFSET in doExp"
                  | C.APP(u, ul) =>
                      rewriteValues (ul, fn ul' => C.APP(u, ul'))
                  | C.FIX(fns, e) => C.FIX(map doFun fns, doExp e)
                  | C.SWITCH(u, v, es) =>
                      rewriteValue (u, fn u' => C.SWITCH(u', v, List.map doExp es))
                  | C.BRANCH(p, ul, v, e1, e2) =>
                      rewriteValues (ul, fn ul' => C.BRANCH(p, ul', v, doExp e1, doExp e2))
                  | C.SETTER(p, ul, e) =>
                      rewriteValues (ul, fn ul' => C.SETTER(p, ul', doExp e))
                  | C.LOOKER(p, ul, v, t, e) =>
                      rewriteValues (ul, fn ul' => C.LOOKER(p, ul', v, t, doExp e))
                  | C.ARITH(p, ul, v, t, e) =>
                      rewriteValues (ul, fn ul' => C.ARITH(p, ul', v, t, doExp e))
                  | C.PURE(P.WRAP nk, [u], v, t, e) =>
                      rewriteVar (v,
                        fn () => rewriteValue (u, fn u' => C.PURE(P.WRAP nk, [u'], v, t, doExp e)),
                        fn () => doExp e)
                  | C.PURE(p, ul, v, t, e) =>
                      rewriteValues (ul, fn ul' => C.PURE(p, ul', v, t, doExp e))
                  | C.RCC(k, l, p, ul, vtl, e) =>
                      rewriteValues (ul, fn ul' => C.RCC(k, l, p, ul', vtl, doExp e))
                (* end case *))
          in
          (* process the module *)
            doExp body
          end

    (* the main function *)
    fun split (
          func as (fk, f, vl as [_,x], [kontTy, t as C.PTRt(C.RPT{ptrLen=n, rawLen=0})], body)
        ) = let
          (* new argument type has an additional argument for the literals *)
          val nt = C.rPtrTy(n+1)
          val _ = if !debugFlg
                then (
                  say (concat["\n==== Before Literals.liftLiterals\n"]);
                  PPCps.printcps0 func)
                else ()
          val env = identifyLiterals body
          val (nbody, code) = if LitEnv.isEmpty env
                then (body, [BC.INT63 0, BC.RETURN])
                else let
(* REAL32: FIXME *)
                  val (code, slotForValue, litPtrTy) = buildLiterals env
                  val lvv = LambdaVar.mkLvar()
                  val nbody = liftLiterals (env, slotForValue, C.VAR lvv, body)
                  (* add code to bind the literal vector *)
                  val nbody = C.SELECT(n, C.VAR x, lvv, litPtrTy, nbody)
                  in
                    (nbody, code)
                  end
          val bytes = LiteralBytecode.encode code
          val nfunc = (fk, f, vl, [kontTy, nt], nbody)
          in
            if !debugFlg
              then (
                say (concat["==== After Literals.liftLiterals\n"]);
                PPCps.printcps0 nfunc)
              else ();
            (nfunc, bytes)
          end
      | split _ = bug "unexpected CPS header in split"

  end (* Literals *)
