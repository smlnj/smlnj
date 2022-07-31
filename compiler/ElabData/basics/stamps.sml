(* Copyright 1996 by AT&T Bell Laboratories *)
(* Re-written by Matthias Blume (3/2000) *)
(* stamps.sml *)

structure Stamps :> STAMPS =
struct

  type pid = PersStamps.persstamp	(* for global stamps *)

  datatype stamp
    = Special of string
    | Global of { pid: pid, cnt: int }
    | Fresh of int

  type ord_key = stamp

  fun compare (Fresh i, Fresh i') = Int.compare (i, i')
    | compare (Fresh _, _) = GREATER
    | compare (_, Fresh _) = LESS
    | compare (Special s, Special s') = String.compare (s, s')
    | compare (Special _, _) = GREATER
    | compare (_, Special _) = LESS
    | compare (Global g, Global g') =
      (case Int.compare (#cnt g, #cnt g')
         of EQUAL => PersStamps.compare (#pid g, #pid g')
          | unequal => unequal)

  fun eq (s, s') = compare (s, s') = EQUAL

  type generator = int ref
  fun newGenerator () = ref 0
  fun fresh r = let val i = !r in r := i + 1; Fresh i end
  val special = Special
  val global = Global

  local
      structure M = IntRedBlackMap
  in
      type converter = int M.map ref * int ref
      fun newConverter () = (ref M.empty, ref 0)
      fun Case _ (Special s) { fresh, global, special } = special s
        | Case _ (Global g) { global, ... } = global g
        | Case (m, n) (Fresh i) { fresh, ... } =
          (case M.find (!m, i) of
               SOME i' => fresh i'
             | NONE => let val i' = !n
                       in
                           n := i' + 1; m := M.insert (!m, i, i');
                           fresh i'
                       end)
  end

  fun isFresh (Fresh _) = true
    | isFresh _ = false

  fun toString (Fresh i) = concat ["FSTAMP(", Int.toString i, ")"]
    | toString (Global { pid, cnt }) =
      concat ["GSTAMP(", PersStamps.toHex pid, ",", Int.toString cnt, ")"]
    | toString (Special s) = concat ["SSTAMP(", s, ")"]

  fun toShortString (Fresh i) = "#F" ^ Int.toString i
    | toShortString (Special s) = "#S:" ^ s
    | toShortString (Global { pid, cnt }) = let
          fun trim3 s = substring (s, size s - 3, 3)
      in
          concat ["#G", trim3 (PersStamps.toHex pid), ".", Int.toString cnt]
      end

end (* structure Stamps *)
