(* literal-bytecode.sml
 *
 * COPYRIGHT (c) 2026 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * Literals are encoded as instructions for a "literal machine."  This module
 * defines a symbolic representation of the instructions for this machine as
 * well as encoding support.  See https://github.com/smlnj/.github/wiki/Literals-v3
 * for a detailed description.
 *)

structure LiteralBytecode : sig

    datatype t
      = INT63 of IntInf.int
      | RAWINT64 of IntInf.int
      | STR8 of string
      | REAL64 of RealLit.t
      | RECORD of int           (* record of uniform values *)
      | RAWBLOCK of int         (* record of raw values *)
      | MIXED of CPS.record_rep (* mixed record *)
      | VEC of int              (* vector literal *)
      | SAVE of int
      | LOAD of int
      | RETURN

    val encode : t list -> Word8Vector.vector

    val dump : t list -> unit

  end = struct

    structure W8V = Word8Vector
    structure W8B = Word8Buffer

    val say = Control.Print.say

    fun bug msg = ErrorMsg.impossible ("Literals: "^msg)

    datatype t
      = INT63 of IntInf.int
      | RAWINT64 of IntInf.int
      | STR8 of string
      | REAL64 of RealLit.t
      | RECORD of int           (* record of uniform values *)
      | RAWBLOCK of int         (* record of raw values *)
      | MIXED of CPS.record_rep (* mixed record *)
      | VEC of int              (* vector literal *)
      | SAVE of int
      | LOAD of int
      | RETURN

    fun opToString (INT63 n) = concat ["INT63(", IntInf.toString n, ")"]
      | opToString (RAWINT64 n) = concat ["RAWINT(64, ", IntInf.toString n, ")"]
      | opToString (STR8 s) = concat ["STR8(", Int.toString(size s), ")"]
      | opToString (REAL64 r) = concat ["REAL64(", RealLit.toString r, ")"]
      | opToString (RECORD n) = concat ["RECORD(", Int.toString n, ")"]
      | opToString (RAWBLOCK n) = concat ["RAWBLOCK(", Int.toString n, ")"]
      | opToString (MIXED{ptrLen, rawLen}) = concat [
            "MIXED(", Int.toString ptrLen, ", ", Int.toString rawLen, ")"
          ]
      | opToString (VEC n) = concat ["VEC(", Int.toString n, ")"]
      | opToString (SAVE i) = concat ["SAVE(", Int.toString i, ")"]
      | opToString (LOAD i) = concat ["LOAD(", Int.toString i, ")"]
      | opToString RETURN = "RETURN"

    fun dump code = let
          fun pr opc = (say(opToString opc); say "\n")
          in
            List.app pr code
          end

  (****************************************************************************
   *                          ENCODING THE BYTECODE                           *
   ****************************************************************************)

    (* analyze a bytecode program to determine the maximum stack depth and size
     * of the save area.  We also do some rudimentary type checking and slot
     * allocation.
     *)
    local
      datatype ty = TAG | RAW | PTR
      fun tyToString TAG = "I"
        | tyToString RAW = "R"
        | tyToString PTR = "P"
      (* return the string representation of the stack with the top on the right *)
      fun stkToString stk = concat[
              "[", String.concatWithMap "," tyToString (List.rev stk), "]"
            ]
      fun isUniform RAW = false
        | isUniform _ = true
      fun isRaw PTR = false
        | isRaw _ = true
      structure Tbl = IntHashTable
      type slot_info = {first : int, last : int ref, ty : ty}
    in
    fun analyse (code as [INT63 0, RETURN]) =
          ({maxDepth=1, maxSaved=0, nInstrs=2}, code)
      | analyse code = let
          val tbl : slot_info Tbl.hash_table = Tbl.mkTable (16, Fail "slot table")
          val insert = Tbl.insert tbl
          val find = Tbl.find tbl
          val lookup = Tbl.lookup tbl
          (* pass1 determines the maximum stack depth and initializes the mapping
           * from pseudo-slots to intervals.  It also typechecks the code.
           *)
          fun pass1 (_, [], _, _, _) = bug "missing RETURN"
            | pass1 (pc, [RETURN], [PTR], _, maxD) = (pc+1, maxD)
            | pass1 (_, [RETURN], _, _, _) = bug "invalid stack on return"
            | pass1 (pc, opc::code, stk, d, maxD) = let
                fun bad msg = bug(concat[
                        msg, " at ", Int.toString pc, " (", opToString opc,
                        "); stk = ", stkToString stk
                      ])
                fun top () = (case stk
                       of [] => bad "empty stack"
                        | ty::_ => ty
                      (* end case *))
                (* pop elements off of the stack while checking that
                 * they satisfy the type predicate.
                 *)
                fun popn pred (0, stk, d) = (stk, d)
                  | popn pred (n, ty::stk, d) = if pred ty
                      then popn pred (n-1, stk, d-1)
                      else bad "insufficient arguments"
                  | popn _ _ = bad "empty stack"
                fun push ty = pass1 (pc+1, code, ty::stk, d+1, Int.max(d+1, maxD))
                fun continue (stk', d') = pass1 (pc+1, code, stk', d', maxD)
                in
                  case opc
                   of INT63 _ => push TAG
                    | RAWINT64 _ => push RAW
                    | STR8 _ => push PTR
                    | REAL64 _ => push RAW
                    | RECORD n => let
                        val (stk', d') = popn isUniform (n, stk, d)
                        in
                          continue (PTR::stk', d'+1)
                        end
                    | RAWBLOCK n => let
                        val (stk', d') = popn isRaw (n, stk, d)
                        in
                          continue (PTR::stk', d'+1)
                        end
                    | MIXED{ptrLen, rawLen} => let
                        val (stk', d') = popn isRaw (rawLen, stk, d)
                        val (stk'', d'') = popn isUniform (ptrLen, stk', d')
                        in
                          continue (PTR::stk'', d''+1)
                        end
                    | VEC n => let
                        val (stk', d') = popn isUniform (n, stk, d)
                        in
                          continue (PTR::stk', d'+1)
                        end
                    | SAVE i => (case find i
                         of SOME _ => bad "store to full slot"
                          | NONE => (
                              insert (i, {first=pc, last=ref pc, ty=top()});
                              continue (stk, d))
                        (* end case *))
                    | LOAD i => (case find i
                         of NONE => bad "load from empty slot"
                          | SOME{last, ty, ...} => (
                              last := pc;
                              push ty)
                        (* end case *))
                    | RETURN => bad "unexpected return"
                  (* end case *)
                end
          val (nInstrs, maxDepth) = pass1 (0, code, [], 0, 0)
          (* the number of pseudo-slots used in the program *)
          val nPseudoSlots = Tbl.numItems tbl
          (* do slot assignment for the SAVE/LOAD instructions; we assume that
           * pseudo-slots are assigned starting at 0 in increasing order (i.e.,
           * slot 0 is stored to before slot 1.
           *)
          val (code, maxSaved) = if nPseudoSlots <= 1
                then (code, nPseudoSlots)
                else let
                  (* the assignment of pseudo-slots to slots *)
                  val assign = Array.tabulate (Tbl.numItems tbl, Fn.id)
                  (* construct the initial worklist *)
                  val wl = List.tabulate(nPseudoSlots, fn i => let
                        val {first, last, ty} = lookup i
                        in
                          {first=first, last=last, id=i}
                        end)
                  (* insert an interval into the active list, which is ordered
                   * by inceasing `last` fields.
                   *)
                  fun addActive (active, last, slot) = let
                        val newItem = {last = last, slot = slot}
                        fun add [] = [newItem]
                          | add (item::rest) = if (last < #last item)
                              then newItem :: item :: rest
                              else item :: add rest
                        in
                          add active
                        end
                  (* allocate slots using a linear-scan allocator; the arguments
                   * are:
                   *    wl      -- list of intervals to process (the work list)
                   *    active  -- active intervals ordered by increasing last use
                   *    avail   -- available slots
                   *    next    -- the next slot to assign when avail is empty
                   *)
                  fun go ([], _, _, next) = next
                    | go ({first, last, id}::wl, active, avail, next) = let
                        (* free any slots whose last use preceeds `first` *)
                        fun free (active as {last, slot}::rest, avail) =
                              if last < first
                                then free(rest, slot::avail)
                                else (active, avail)
                          | free arg = arg
                        val (active, avail) = free (active, avail)
                        in
                          case avail
                           of [] => (
                                Array.update(assign, id, next);
                                go (wl, addActive(active, !last, next), [], next+1))
                            | slot::avail => (
                                Array.update(assign, id, slot);
                                go (wl, addActive(active, !last, slot), avail, next))
                          (* end case *)
                        end
                  val maxSaved = go (wl, [], [], 0)
                  (* rewrite an instruction to use the slot assignments *)
                  fun rewrite (SAVE i) = SAVE(Array.sub(assign, i))
                    | rewrite (LOAD i) = LOAD(Array.sub(assign, i))
                    | rewrite opcode = opcode
                  in
                    (List.map rewrite code, maxSaved)
                  end
          in
            ({maxDepth=maxDepth, maxSaved=maxSaved, nInstrs=nInstrs}, code)
          end (* analyse *)
    end (* local *)


  (****************************************************************************
   *                          ENCODING THE BYTECODE                           *
   ****************************************************************************)

  (* magic number for V3 literal bytecodes.  This needs to agree with the runtime
   * constant `V3_MAGIC` in `runtime/gc/build-literals-v3.c`
   *)
    val magicV3 : int = 0x20260912

  (* `INT63` opcodes *)
    fun opINT63_0_31 n = Word8.fromLargeInt n
    fun opINT63_m32_m1 n = Word8.fromLargeInt(0x40 + n)
    val opINT63b : Word8.word = 0wx80
    val opINT63h : Word8.word = 0wx81
    val opINT63w : Word8.word = 0wx82
    val opINT63l : Word8.word = 0wx83
  (* `INT64` opcodes *)
    fun opINT64_0_31 n = Word8.fromLargeInt(n + 0x40)
    fun opINT64_m32_m1 n = Word8.fromLargeInt(0x80 + n)
  (* `RAWINT(64,-)` opcodes *)
    val opRAWINT64b : Word8.word = 0wx9C
    val opRAWINT64h : Word8.word = 0wx9D
    val opRAWINT64w : Word8.word = 0wx9E
    val opRAWINT64l : Word8.word = 0wx9F
  (* `REAL` opcodes *)
    val opREAL32 : Word8.word = 0wx84
    val opREAL64 : Word8.word = 0wx85
  (* `STR8` opcodes *)
    val opSTR8_0 : Word8.word = 0wx88
    val opSTR8b : Word8.word = 0wx89
    val opSTR8h : Word8.word = 0wx8A
    val opSTR8w : Word8.word = 0wx8B
  (* record opcodes *)
    fun opRECORD_1_14 len = Word8.fromInt(0x9F + len)
    val opRECORDb: Word8.word = 0wxAE
    val opRECORDh: Word8.word = 0wxAF
  (* raw records *)
    fun opRAW_1_14 n = Word8.fromInt(0xAF + n)
    val opRAWb : Word8.word = 0wxBE
    val opRAWh : Word8.word = 0wxBF
  (* mixed records *)
    val opMIXEDbb : Word8.word = 0wxC0
    val opMIXEDhh : Word8.word = 0wxC1
  (* vector opcodes *)
    val opVEC_0 : Word8.word = 0wxC8
    val opVECb : Word8.word = 0wxC9
    val opVECh : Word8.word = 0wxCA
  (* save/load opcodes *)
    fun opSAVE_0_6 slot = Word8.fromInt(0xE8 + slot)
    val opSAVEh : Word8.word = 0wxEF
    fun opLOAD_0_6 slot = Word8.fromInt(0xF0 + slot)
    val opLOADh : Word8.word = 0wxF7
  (* return *)
    val opRETURN : Word8.word = 0wxff

    fun ~>> (n : int, w : word) = Word.toIntX(Word.~>>(Word.fromInt n, w))
    fun >> (n : int, w : word) = Word.toIntX(Word.>>(Word.fromInt n, w))

  (* encode an 8-bit signed value as a byte list *)
    fun addInt8 (buf, n) = W8B.add1(buf, Word8.fromInt n)
    fun addLargeInt8 (buf, n) = W8B.add1(buf, Word8.fromLargeInt n)
  (* encode an 8-bit unsigned value as a byte list *)
    val addUInt8 = addInt8
  (* encode a 16-bit signed value as a byte list *)
    fun addInt16 (buf, n) = (
          W8B.add1(buf, Word8.fromInt(~>>(n, 0w8)));
          W8B.add1(buf, Word8.fromInt n))
    fun addLargeInt16 (buf, n) = (
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w8)));
          W8B.add1(buf, Word8.fromLargeInt n))
  (* encode a 16-bit unsigned value as a byte list *)
    fun addUInt16 (buf, n) = (
          W8B.add1(buf, Word8.fromInt(>>(n, 0w8)));
          W8B.add1(buf, Word8.fromInt n))
  (* encode a 32-bit signed value as a byte list *)
    fun addInt32 (buf, n) = (
          W8B.add1(buf, Word8.fromInt(~>>(n, 0w24)));
          W8B.add1(buf, Word8.fromInt(~>>(n, 0w16)));
          W8B.add1(buf, Word8.fromInt(~>>(n, 0w8)));
          W8B.add1(buf, Word8.fromInt n))
    fun addLargeInt32 (buf, n) = (
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w24)));
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w16)));
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w8)));
          W8B.add1(buf, Word8.fromLargeInt n))
  (* encode a 32-bit unsigned value as a byte list *)
    fun addUInt32 (buf, n) = (
          W8B.add1(buf, Word8.fromInt(>>(n, 0w24)));
          W8B.add1(buf, Word8.fromInt(>>(n, 0w16)));
          W8B.add1(buf, Word8.fromInt(>>(n, 0w8)));
          W8B.add1(buf, Word8.fromInt n))
  (* encode a 64-bit signed value as a byte list *)
    fun addLargeInt64 (buf, n) = (
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w56)));
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w48)));
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w40)));
          W8B.add1(buf, Word8.fromLargeInt(IntInf.~>>(n, 0w32)));
          addLargeInt32 (buf, n))
    fun addLargeInt64' (buf, n) = addLargeInt64 (buf, IntInf.fromInt n)

    fun intToBytes32 n = W8V.fromList[
            Word8.fromInt(~>>(n, 0w24)),
            Word8.fromInt(~>>(n, 0w16)),
            Word8.fromInt(~>>(n, 0w8)),
            Word8.fromInt n
          ]

    fun largeIntToBytes32 n = W8V.fromList[
            Word8.fromLargeInt(IntInf.~>>(n, 0w24)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w16)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w8)),
            Word8.fromLargeInt n
          ]

    fun largeIntToBytes64 n = W8V.fromList[
            Word8.fromLargeInt(IntInf.~>>(n, 0w56)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w48)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w40)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w32)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w24)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w16)),
            Word8.fromLargeInt(IntInf.~>>(n, 0w8)),
            Word8.fromLargeInt n
          ]

    fun largeIntToBytes (32, n) = largeIntToBytes32 n
      | largeIntToBytes (64, n) = largeIntToBytes64 n
      | largeIntToBytes (sz, _) = bug ("bogus integer size " ^ Int.toString sz)

    fun real64ToBytes r = #1(Real64ToBits.toBits r)

    (* bounds *)
    val minInt8 : IntInf.int = ~128
    val maxInt8 : IntInf.int = 127
    val minInt16 : IntInf.int = ~32768
    val maxInt16 : IntInf.int = 32767
    val minInt32 : IntInf.int = ~2147483648
    val maxInt32 : IntInf.int = 2147483647

    (* encode tagged integers *)
    fun encINT63 (buf, n) = if (0 <= n) andalso (n <= 31)
            then W8B.add1(buf, opINT63_0_31 n)
          else if (n < 0) andalso (n >= ~32)
            then W8B.add1(buf, opINT63_m32_m1 n)
          else if (minInt8 <= n) andalso (n <= maxInt8)
            then (W8B.add1(buf, opINT63b); addLargeInt8(buf, n))
          else if (minInt16 <= n) andalso (n <= maxInt16)
            then (W8B.add1(buf, opINT63h); addLargeInt16(buf, n))
          else if (minInt32 <= n) andalso (n <= maxInt32)
            then (W8B.add1(buf, opINT63w); addLargeInt32(buf, n))
            else (W8B.add1(buf, opINT63l); addLargeInt64(buf, n))

    (* encode 64-bit raw integers *)
    fun encINT64 (buf, n) = if (0 <= n) andalso (n <= 31)
            then W8B.add1(buf, opINT64_0_31 n)
          else if (n < 0) andalso (n >= ~32)
            then W8B.add1(buf, opINT64_m32_m1 n)
          else if (minInt8 <= n) andalso (n <= maxInt8)
            then (W8B.add1(buf, opRAWINT64b); addLargeInt8(buf, n))
          else if (minInt16 <= n) andalso (n <= maxInt16)
            then (W8B.add1(buf, opRAWINT64h); addLargeInt16(buf, n))
          else if (minInt32 <= n) andalso (n <= maxInt32)
            then (W8B.add1(buf, opRAWINT64w); addLargeInt32(buf, n))
            else (W8B.add1(buf, opRAWINT64l); addLargeInt64(buf, n))

(* REAL32: FIXME *)
    (* endcode a 64-bit real literal *)
    fun encREAL64 (buf, r) = (
          W8B.add1(buf, opREAL64);
          W8B.addVec(buf, #1(Real64ToBits.toBits r)))

    (* encode a STR8 opcode *)
    fun encSTR8 (buf, "") = W8B.add1(buf, opSTR8_0)
      | encSTR8 (buf, s) = let
          val len = size s
          in
            if (len <= 255)
              then (W8B.add1(buf, opSTR8b); addUInt8(buf, len))
            else if (len < 65535)
              then (W8B.add1(buf, opSTR8h); addUInt16(buf, len))
              else (W8B.add1(buf, opSTR8w); addUInt32(buf, len));
            W8B.addVec(buf, Byte.stringToBytes s)
          end

    (* encode a RECORD opcode and length *)
    fun encRECORD (buf, len) = if (len <= 14)
            then W8B.add1(buf, opRECORD_1_14 len)
          else if (len <= 255)
            then (W8B.add1(buf, opRECORDb); addUInt8(buf, len))
          else if (len <= 65535)
            then (W8B.add1(buf, opRECORDh); addUInt16(buf, len))
            else bug "record too big"

    (* encode a RAW record opcode *)
    fun encRAW (buf, len) = if (len <= 14)
            then W8B.add1(buf, opRAW_1_14 len)
          else if (len <= 255)
            then (W8B.add1(buf, opRAWb); addUInt8(buf, len))
          else if (len <= 65535)
            then (W8B.add1(buf, opRAWh); addUInt16(buf, len))
            else bug "raw record too big"

    (* encode a MIXED record opcode *)
    fun encMIXED (buf, {ptrLen, rawLen}) =
          if (ptrLen <= 255) andalso (rawLen <= 255)
            then (W8B.add1(buf, opMIXEDbb); addUInt8(buf, ptrLen); addUInt8(buf, rawLen))
          else if (ptrLen <= 65535) andalso (rawLen <= 65535)
            then (
              W8B.add1(buf, opMIXEDhh);
              addUInt16(buf, ptrLen);
              addUInt16(buf, rawLen))
            else bug "mixed record too big"

  (* encode a VECTOR opcode and length *)
    fun encVEC (buf, 0) = W8B.add1(buf, opVEC_0)
      | encVEC (buf, len) = if (len <= 255)
            then (W8B.add1(buf, opVECb); addUInt8(buf, len))
          else if (len <= 65535)
            then (W8B.add1(buf, opVECh); addUInt16(buf, len))
            else bug "vector too big"

    (* encode a SAVE/LOAD opcode *)
    local
      fun enc (op_0_6, oph) (buf, slot) = if (slot <= 6)
          then W8B.add1(buf, op_0_6 slot)
          else (W8B.add1(buf, oph); addUInt16(buf, slot))
    in
    val encSAVE = enc (opSAVE_0_6, opSAVEh)
    val encLOAD = enc (opLOAD_0_6, opLOADh)
    end (* local *)

    (* encode a return *)
    fun encRETURN buf = W8B.add1(buf, opRETURN)

    (* encode the literal header block *)
    fun headerToBytes {maxDepth, maxSaved} = W8V.concat[
          intToBytes32 magicV3,
          intToBytes32 maxDepth,
          intToBytes32 Target.mlValueSz,
          intToBytes32 maxSaved]

    fun encode code = let
          val ({maxDepth, maxSaved, nInstrs}, code) = analyse code
          val buf = W8B.new (4 * nInstrs)
          (* encode an instruction *)
          fun enc (INT63 n) = encINT63 (buf, n)
            | enc (RAWINT64 n) = encINT64 (buf, n)
            | enc (REAL64 r) = encREAL64 (buf, r)
            | enc (STR8 s) = encSTR8 (buf, s)
            | enc (RECORD n) = encRECORD (buf, n)
            | enc (RAWBLOCK n) = encRAW (buf, n)
            | enc (MIXED rep) = encMIXED (buf, rep)
            | enc (VEC n) = encVEC (buf, n)
            | enc (SAVE i) = encSAVE (buf, i)
            | enc (LOAD i) = encLOAD (buf, i)
            | enc RETURN = encRETURN buf
          in
            List.app enc code;
            W8V.concat[
                headerToBytes {maxDepth = maxDepth, maxSaved = maxSaved},
                W8B.contents buf
              ]
          end
(*+DEBUG*)
            handle ex => (
              say("### Bytecode ###\n");
              dump code;
              say "###\n";
              raise ex)
(*-DEBUG*)

  end (* LiteralBytecode *)
