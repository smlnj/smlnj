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
    fun useLit (OBJ{useCnt, ...}) = useCnt := !useCnt + 1
      | useLit _ = ()
    (* count a reference to a literal from another literal record *)
    fun refUseLit (OBJ{useCnt, refCnt, ...}) = (
          useCnt := !useCnt + 1;
          refCnt := !refCnt + 1)
      | refUseLit _ = ()

  (* return the CPS type for a literal *)
    fun cpsTypeOf (OBJ{ty, ...}) = ty
      | cpsTypeOf (IMMED{ty, ...}) = C.NUMT ty
      | cpsTypeOf (ENUM _) = C.ENUMt
      | cpsTypeOf (REAL{ty, ...}) = C.FLTt ty

  (* is a literal used as value outside of being part of another literal? *)
    fun litIsUsed (OBJ{refCnt, useCnt, ...}) = (!refCnt < !useCnt)
      | litIsUsed _ = bug "impossible"

  (* is a literal shared?  This happens when its refCnt is > 1
   * or when its refCnt = 1 and its useCnt > 1.
   *)
    fun litIsShared (OBJ{refCnt=ref rc, useCnt, ...}) =
          (rc > 1) orelse ((rc = 1) andalso (!useCnt > 1))
      | litIsShared _ = bug "impossible"

  (* print the list of "top-level" literals (for debugging purposes) *)
    fun printLits lits = let
          val id2s = Word.fmt StringCvt.DEC
          fun prIndent 0 = ()
            | prIndent n = (say "  "; prIndent(n-1))
          fun prObj indent (RECORD{rk, args}, suffix) = (
                case rk
                 of C.RK_VECTOR => say(concat["VECTOR ", suffix, "\n"])
                  | C.RK_RECORD => say(concat["RECORD ", suffix, "\n"])
                  | C.RK_MIXED{ptrLen, rawLen} => say(concat["MIXED ", suffix, "\n"])
                  | C.RK_RAWBLOCK => say(concat["RAW ", suffix, "\n"])
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
          fun prSlot (i, OBJ arg) = (
                say (StringCvt.padLeft #" " 4 (Int.toString i) ^ ": ");
                prOBJ 3 arg)
            | prSlot (i, _) = bug "expected top-level OBJ"
          in
            List.appi prSlot lits
          end (* printLits *)

  (* an environment for tracking literals *)
    structure LitEnv : sig

        type t

        (* a variable that is bound to a literal is either used to build a literal
         * record, in which case the bool is false, or is used as an argument to
         * some other operation (including non-literal records).
         *)
        type var_info = bool * literal

        datatype value
          = NoLit
          | Lit of literal
          | Real of int

        (* create a new environment *)
        val new : unit -> t
        (* add a literal record value to the environment *)
        val addRecord : t -> C.record_kind * literal list * C.lvar -> unit
        (* return the literal that a variable is bound to *)
        val findVar : t -> C.lvar -> var_info option
        (* `isConst lenv v` returns true when either `v` is a variable bound to a
         * literal or `v` is a constant value.
         *)
        val isConst : t -> C.value -> bool
        (* find the literal value for the given value.  Note that for NUM and REAL
         * values, we return NONE, since they are represented as IMMED literals.
         *)
        val findValue : t -> C.value -> value
        (* record the use of a value in a non-literal context *)
        val useValue : t -> C.value -> unit
        (* like useValue, but for constant values embedded in literal records.  This
         * function returns the literal that the value maps to.
         *)
        val useLitValue : t -> C.value -> literal
        (* return the number of literals in the environment *)
        val numLits : t -> int
        (* return true if there are no literals defined in the environment *)
        val isEmpty : t -> bool
        (* return true if the environment has unbound 64-bit real literals (e.g.,
         * the arguments to arithmetic operations).
         *)
        val hasReal64 : t -> bool
        (* get the literal objects and reals that are used outside of constructing
         * literal records.
         *)
        val getLiterals : t -> {
                usedLits : literal list,
                realLits : int RealConst.t list
              }
        (* return a list of the variables that are bound to top-level literals paired
         * with their binding.
         *)
        val boundVars : t -> (C.lvar * literal) list

      end = struct

        (* hash keys are the heap-allocated literals *)
        type key = obj

(* TODO: use the Hash module from the SML/NJ Library *)
        fun hashObj (RECORD(rk, args)) = let
              fun hashArg (OBJ{id, ...}, h) = 0w3 * id + 0w293 + h
                | hashArg (IMMED ic, h) => hashIConst ic + h
                | hashArg (ENUM n, h) = 0w3 * Word.fromInt n + 0w157 + h
                | hashArg (REAL rc, h) => hashRConst rc + h
              val h0 = (case rk
                     of C.RK_VECTOR => 0w197
                      | C.RK_RECORD => 0w313
                      | C.RK_MIXED{ptrLen, rawLen} => 0w439
                      | C.RK_RAWBLOCK => 0w571
                      | _ => bug("unexpected record kind " ^ PPCps.rkToString rk)
                    (* end case *))
              in
                List.foldl f h0 lits
              end
          | hashOBJ (STRING s) = HashString.hashString s + 0w419
        and hashIConst {ty={tag=true, ...}, ival}) =
              Word.fromLargeInt ival + 0w157
          | hashIConst {ty={sz, ...}, ival}) =
              Word.fromLargeInt ival + Word.fromInt sz + 0w257
        and hashRConst {ty=32, rval} = RealLit.hash rval + 0w179
          | hashRConst {ty=64, rval} = RealLit.hash rval + 0w283

        fun sameObj (RECORD(rk1, args1), RECORD(rk2, args2)) = let
              fun sameLit (OBJ{useCnt=u1, ...}, OBJ{useCnt=u2, ...}) = (u1 = u2)
                | sameLit (IMMED ic1, IMMED ic2) = sameIConst (ic1, ic2)
                | sameLit (ENUM n1, ENUM n2) = (n1 = n2)
                | sameLit (REAL rc1, REAL rc2) = sameRConst (rc1, tc2)
                | sameLit _ = false
              in
                (rk1 = rk2) andalso ListPair.allEq sameLit (args1, args2)
              end
          | sameKey (STRING s1, STRING s2) = (s1 = s2)
          | sameKey _ = false
        and sameIConst ({ty={sz=s1, ...}, ival=iv1}, {ty={sz=s2, ...}, ival=iv2}) =
              (s1 = s2) andalso (iv1 = iv2)
        and sameRConst ({ty=t1, rval=rv1}, {ty=t2, rval=rv2}) =
              (t1 = t2) andalso RealLit.same(rv1, rv2)

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
                reals = RSet.mkEmpty 32,
                vMap = LV.Tbl.mkTable(32, Fail "VarTbl")
              }

        fun hasReal64 (LE{reals, ...}) = RSet.isEmpty reals

        local
          fun addLit mkKey (LE{lits, ...}) = let
                val find = LTbl.find lits
                val insert = LTbl.insert lits
                in
                  fn arg => let
                      val key = mkKey arg
                      in
                        case find key
                         of SOME lit => lit
                          | NONE => let
                              val lit = OBJ{
                                      useCnt = ref 0, refCnt = ref 0,
                                      id = Word.fromInt(LTbl.numItems lits),
                                      value = key
                                    }
                              in
                                insert (key, lit);
                                lit
                              end
                        (* end case *)
                      end
                end
        in
        val addRecord = addLit RECORD
        val addString addLit STRING
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

        datatype value
          = NoLit
          | Lit of literal
          | Real of int

        fun findValue (LE{lits, reals, vMap, ...}) = let
              val findLit = LTbl.find lits
              val findVar = LV.Tbl.find vMap
              in
                fn (C.VAR x) => (case findVar x
                      of SOME(_, lit) => Lit lit
                       | NONE => NoLit
                     (* end case *))
                 | (C.LABEL _) => bug "unexpected LABEL"
                 | (C.NUM n) => NoLit
                 | (C.ENUM _) => NoLit
                 | (C.REAL r) => (case findReal r
                      of SOME i => Real i
                       | NONE => NoLit
                     (* end case *))
                 | (C.STRING s) => (case findLit (STRING s)
                      of SOME lit => Lit lit
                       | NONE => NoLit
                     (* end case *))
                 | C.VOID => bug "unexpected VOID"
              end

        fun useValue env = let
              val findVar = findVar env
              val addReal = addReal env
              val addString = addString env
              val insert = insertVar env
              in
                fn (C.VAR x) => (case findVar x
                       of SOME(flg, lit) => (
                            useLit lit;
                            if flg then () else insert (x, (true, lit)))
                        | NONE => ()
                      (* end case *))
                 | (C.LABEL _) => bug "unexpected LABEL"
                 | (C.NUM n) => ()
                 | (C.ENUM _) => ()
                 | (C.REAL r) => (setHasReal64 env; useLit(addReal r))
                 | (C.STRING s) => useLit(addString s)
                 | C.VOID => ()
              end

        fun useLitValue env = let
              val findVar = findVar env
              val addReal = addReal env
              val addString = addString env
              fun use lit = (refUseLit lit; lit)
              in
                fn (C.VAR x) => (case findVar x
                      of SOME(_, lit) => use lit
                       | NONE => bug "expected literal"
                     (* end case *))
                 | (C.LABEL _) => bug "unexpected LABEL"
                 | (C.NUM n) => IMMED n
                 | (C.ENUM n) => ENUM n
                 | (C.REAL r) => addReal r
                 | (C.STRING s) => use(addString s)
                 | C.VOID => bug "unexpected VOID"
              end

        fun numLits (LE{lits, ...}) = LTbl.numItems lits

        fun isEmpty (LE{lits, reals, ...}) =
              (LTbl.numItems lits = 0) andalso (RSet.isEmpty reals)

        fun getLiterals (LE{lits, reals, ...}) = {
                usedLiterals = List.filter litIsUsed (LTbl.listItems lits),
                realLiterals = RSet.listItems reals
              }

        fun boundVars (LE{vMap, ...}) =
              LV.Tbl.foldi
                (fn (x, (true, lit), acc) => (x, lit)::acc | (_, _, acc) => acc)
                  [] vMap

      end (* structure LitEnv *)

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
          val useLitValue = LitEnv.useLitValue env
          val addRecord = LitEnv.addRecord env
          fun addWrap (nk, lit) = let
                val rk = (case nk
                       of P.INT 64 => C.RK_RAWBLOCK
                        | P.UINT 64 => C.RK_RAWBLOCK
                        | P.FLOAT _ => C.RK_RAWBLOCK
                        | _ => raise Fail("unexpected wrap of " ^ NumKind.toString nk)
                      (* end case *))
                in
                  addRecord (rk, [lit])
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
                          then addRecord (rk, List.map useLitValue ul, v)
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
                      then (addWrap(nk, useLitValue u); doExp e)
                      else doExp e
                  | C.PURE (p, ul, v, t, e) => (useValues ul; doExp e)
                  | C.RCC (k, l, p, ul, vtl, e) => (useValues ul; doExp e)
                (* end case *))
          in
            doExp body;
            env
          end

  (* literal values are either in the main literal vector or in the vector
   * of real literals.
   *)
    datatype lit_loc = LitSlot of int | Real64Slot of int

    (* build the representation of the literals; return a table mapping literal IDs
     * to their locations, the bytecode for building the literal vector, and a boolean
     * that is true if there is a real-literal vector.
     *)
    fun buildLiterals env = let
          val {usedLits, realLits} = LitEnv.getLiterals env
          (* get a list of the literals that are bound to variables in order of their
           * definition.
           *)
          val lits = let
                fun gt (OBJ{id=a, ...}, OBJ{id=b, ...}) = (a > b)
                  | gt _ = bug "unexpected immediate literal"
                in
                  ListMergeSort.sort gt usedLits
                end
          val numLits = List.length lits
          val numReals = List.length realLits
          val litIdTbl = WordTbl.mkTable(numLits, Fail "litIdTbl")
          val insertLit = let
                val insert = WordTbl.insert litIdTbl
                in
                  fn id => let val slot = !nLits
                      in
                        nLits := slot + 1;
                        insert (id, LitSlot slot)
                      end
                end
          val insertReal64 = let
                val insert = WordTbl.insert litIdTbl
                in
                  fn (id, rval) => let val slot = !nReal64Lits
                      in
                        nReal64Lits := slot + 1;
                        insert (id, Real64Slot slot);
                        real64Lits := real64ToBytes rval :: !real64Lits
                      end
                end
          (* table to track shared literals (indexed by literal ID) *)
          val sharedLitTbl = WordTbl.mkTable(numNamedLits, Fail "sharedLitTbl")
          val insertSharedLit = let
                val insert = WordTbl.insert sharedLitTbl
                in
                  fn id => let val loc = WordTbl.numItems sharedLitTbl
                      in
                        insert (id, loc); loc
                      end
                end
          val findSharedLit = WordTbl.find sharedLitTbl
          (* generate code to create a record *)
          fun genRecord (rk, lits, code) = let
                val code = List.foldl genLiteral code lits
                in
                  case rk
                   of C.RK_VECTOR => BC.VEC(List.length lits) :: code
                    | C.RK_RECORD => BC.RECORD(List.length lits) :: code
                    | C.RK_MIXED rep => BC.MIXED rep :: code
                    | C.RK_RAWBLOCK => BC.RAW(List.length lits) :: code
                    | _ => bug "unexpected record kind"
                  (* end case *)
                end
          fun genLiteral (OBJ{refCnt, useCnt, value, id, ...}, code) = if (!refCnt > 1)
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
                  (* end case *)
                else (case value
                   of RECORD(rk, lits) => enRecord(rk, lits, code)
                    | STRING s => BC.STR8 s :: code
                  (* end case *))
            | genLiteral (IMMED{ty={sz=63, ...}, ival}, code) =
                BC.INT63 ival :: code
            | genLiteral (IMMED{ty={sz=64, ...}, ival}, code) =
                BC.RAWINT64 ival :: code
            | genLiteral (ENUM n, code) =
                BC.INT63(IntInf.fromInt n) :: code
            | genLiteral (REAL{ty=64, rval}, code) =
                BC.REAL rval :: code
            | genLiteral _ = bug "bogus literal"
          val code = List.foldl genLiteral [] lits
          (* add code to generate the real literals (if any) *)
          val code = ??
          (* create the top-level literal record *)
          val code = (case (numLits, numReal)
                 of (0, 0) => [BC.ENUM 0]
                  | (0, _) => BC.RAW numReal :: code
                  | (_, 0) => BC.RECORD numLits :: code
                  | _ => BC.MIXED{ptrLen=numLits, rawLen=numReal}
                (* end case *))
          val code = List.rev (BC.RETURN :: code)
          in
            if !debugFlg
              then let
                fun prBV (x, LIT{id, ...}) = (
                      say(concat["LET ", LV.lvarName x, " = "]);
                      case WordTbl.find litIdTbl id
                       of NONE => say "<no slot>\n"
                        | SOME(LitSlot n) => say(concat["literal-", Int.toString n, "\n"])
                        | SOME(Real64Slot n) => say(concat["real64-", Int.toString n, "\n"])
                      (* end case *))
                fun prByte (i, w) = (
                      say(StringCvt.padLeft #"0" 2 (Word8.toString w));
                      if (i mod 16 = 15)
                        then say "\n"
                        else say " ")
                in
                  say "==========\n";
                  say(concat["== bytecode size: ", Int.toString(W8V.length code), "\n"]);
                  printLits lits;
                  say "==========\n";
                  List.app prBV (LitEnv.boundVars env);
                  say "==========\n";
                  W8V.appi prByte code;
                  if (W8V.length code mod 16 <> 15) then say "\n" else ();
                  say "==========\n"
                end
              else ();
            (litIdTbl, code, litVecSz, !nReal64Lits)
          end

(* TODO: keep an environment of available literal bindings to avoid redundant SELECTs *)
  (* rewrite the program, removing unused variables *)
    fun liftLiterals (env, idTbl, litVec, fltVec, body) = let
          val findValue = LitEnv.findValue env
          val findVar = LitEnv.findVar env
          fun getSlot id = (case WordTbl.find idTbl id
                 of SOME slot => slot
                  | NONE => bug("no slot for literal " ^ Word.fmt StringCvt.DEC id)
                (* end case *))
        (* rewrite a value *)
          fun rewriteValue (u, k : C.value -> C.cexp) = (case findValue u
                 of SOME(LIT{id, value, ...}) => let
                      val v = LambdaVar.mkLvar()
                      val ty = cpsTypeOf value
                      in
                        case getSlot id
                         of LitSlot n => C.SELECT(n, litVec, v, ty, k(C.VAR v))
                          | Real64Slot n => C.SELECT(n, fltVec, v, ty, k(C.VAR v))
                        (* end case *)
                      end
                  | _ => k u
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
                then (body, [BC.ENUM 0, BC.RETURN])
                else let
(* REAL32: FIXME *)
                  val (idTbl, code, nLits, nReal64Lits) = buildLiterals env
                  val lvv = LambdaVar.mkLvar()
                  val rvv = LambdaVar.mkLvar()
                  val nbody = liftLiterals (env, idTbl, C.VAR lvv, C.VAR rvv, body)
                (* add code to bind the real-literal vector (if necessary) *)
                  val nbody = if nReal64Lits > 0
                        then C.SELECT(0, C.VAR lvv, rvv, C.fPtrTy nReal64Lits, nbody)
                        else nbody
                (* add code to bind the literal vector *)
                  val nbody = C.SELECT(n, C.VAR x, lvv, C.rPtrTy nLits, nbody)
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
