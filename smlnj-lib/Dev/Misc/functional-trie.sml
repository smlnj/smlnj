(* trie.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of tries for strings.  The interface supports using
 * the trie as a finite map with insertion and find operations, or as a
 * finite-state-machine recognizer.
 *
 * TODO:
 *    Some possible data-structure improvements.
 *      1) We can compress sequences of characters when there is only one choice
 *      2) Use a vector of kids when the out degree exceeds some limit
 *
 *)

structure Trie :> sig

    (* the abstract type of tries mapping strings to values of type `'a` *)
    type 'a t

    (* make a trie from a list of string-value pairs.  This function raises
     * the `Fail` exception if there are duplicate keys in the argument list.
     *)
    val make : (string * 'a) list -> 'a t

    (* extend the trie; if the key is already present, then the old value
     * is replaced by the new value.
     *)
    val insert : 'a t * string * 'a -> 'a t

    (* attempt to match the string/substring *)
    val find : 'a t * string -> 'a option
    val findSubstr : 'a t * substring -> 'a option

    (** FSM operations **)

    (* step one character *)
    val step : 'a t * char -> 'a t

    (* step one character *)
    val steps : 'a t * string -> 'a t

    (* is the trie node empty? *)
    val isEmpty : 'a t -> bool

    (* get the value at the node *)
    val valueOf : 'a t -> 'a option

    (* returns the list of key-value pairs in the trie *)
    val completionsOf : 'a t -> (string * 'a) list

  end = struct

    (* integer addition w/o overflow checking *)
    fun ++ (a, b) = Word.toIntX(Word.fromInt a + Word.fromInt b)

    infix ++

    (* string subscript w/o bounds check *)
    val sub = Unsafe.CharVector.sub

    datatype 'a t
      (* The empty trie *)
      = Empty
      (* An internal node.  The kids are in sorted order by character. *)
      | Nd of {kids : (char * 'a t) list, value : 'a option}

    fun insert (tri, key, value) = let
          fun mkPath [] = Nd{kids=[], value=SOME value}
            | mkPath (c::cs) = Nd{kids=[(c, mkPath cs)], value=NONE}
          fun ins (Empty, cs) = mkPath cs
            | ins (Nd{kids, value}, []) = Nd{kids=kids, value=value}
            | ins (Nd{kids, value}, c::cs) = let
                fun lp [] = [(c, mkPath cs)]
                  | lp (kids' as (c', tr)::rkids) = if (c < c')
                        then (c, mkPath cs)::kids'
                      else if (c = c')
                        then (c, ins(tr, cs)) :: rkids
                        else (c', tr) :: lp rkids
                in
                  Nd{kids=lp kids, value=value}
                end
          in
            ins (tri, String.explode key)
          end

    fun make items = let
          (* first sort the items by the strings *)
          val items = ListMergeSort.sort
                (fn ((k1, _), (k2, _)) => String.> (k1, k2))
                  items
          (* check for duplicates *)
          val () = let
                fun chk ((k1, _)::(r as (k2, _)::_)) = if (k1 = k2)
                      then raise Fail (concat[
                          "duplicate key \"", String.toString k1, "\""
                        ])
                      else chk r
                  | chk _ = ()
                in
                  chk items
                end
          in
            List.foldl
              (fn ((key, value), tri) => insert (tri, key, value))
                Empty
                  items
          end (* make *)

    fun find (tri, key) = let
          val n = size key
          fun find' (Empty, _) = NONE
            | find' (Nd{kids, value}, i) = if (i < n)
                then let
                  val c = sub(key, i)
                  fun lp [] = NONE
                    | lp ((c', tri)::kids) = if (c < c')
                          then NONE
                        else if (c = c')
                          then find' (tri, i++1)
                          else lp kids
                  in
                    lp kids
                  end
                else value
          in
            find' (tri, 0)
          end

    fun findSubstr (tri, key) = let
          val n = Substring.size key
          fun find' (Empty, _) = NONE
            | find' (Nd{kids, value}, ss) = (case Substring.getc ss
                 of SOME(c, ss') => let
                      fun lp [] = NONE
                        | lp ((c', tri)::kids) = if (c < c')
                              then NONE
                            else if (c = c')
                              then find' (tri, ss')
                              else lp kids
                      in
                        lp kids
                      end
                  | NONE => value
                (* end case *))
          in
            find' (tri, key)
          end

    (* step one character *)
    fun step (Empty, _) = Empty
      | step (Nd{kids, ...}, c) = let
          fun lp [] = Empty
            | lp ((c', tri)::kids) = if (c < c')
                  then Empty
                else if (c = c')
                  then tri
                  else lp kids
          in
            lp kids
          end

    fun steps (tri, s) = let
          val n = size s
          fun step1 (Empty, _) = Empty
            | step1 (Nd{kids, ...}, i) = if (i < n)
                then let
                  val c = sub(s, i)
                  fun lp [] = Empty
                    | lp ((c', tri)::kids) = if (c < c')
                          then Empty
                        else if (c = c')
                          then step1 (tri, i++1)
                          else lp kids
                  in
                    lp kids
                  end
                else Empty
          in
            step1 (tri, 0)
          end

    (* is the trie node empty? *)
    fun isEmpty Empty = true
      | isEmpty _ = false

    (* get the value at the node *)
    fun valueOf (Nd{value, ...}) = value
      | valueOf _ = NONE

    (* returns the list of key-value pairs in the trie *)
    fun completionsOf trie = let
          fun walk (_, Empty, items) = items
            | walk (prefix, Nd{kids, value}, items) = let
                val items' = List.foldr
                      (fn ((c, tr), items) => walk(c::prefix, tr, items))
                        items
                          kids
                in
                  case value
                   of SOME v => (String.implodeRev prefix, v)::items'
                    | NONE => items'
                  (* end case *)
                end
          in
            walk ([], trie, [])
          end

  end
