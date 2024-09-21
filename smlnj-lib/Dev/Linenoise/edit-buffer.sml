(* edit-buffer.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Base definitions and utilities for managing the editor state.
 *)

structure EditBuffer : sig

  (* the editor buffer *)
    type t = {
        contents : CharArray.array ref, (* current contents of the buffer *)
        last : int ref                  (* index of position following last character *)
      }

  (* create the initial buffer *)
    val new : unit -> t

  (* is the buffer empty? *)
    val isEmpty : t -> bool

  (* clear the buffer *)
    val clear : t -> unit

  (* get the contents as an array slice *)
    val getContents : t -> CharArraySlice.slice

  (* add a character/string to the end of the buffer *)
    val append1 : t * char -> unit
    val append  : t * string -> unit

  (* insert a character at the specified position *)
    val insertAt : t * int * char -> unit

  (* delete the character at the specified position *)
    val deleteAt : t * int -> unit

  (* `deleteSlice (buf, ix, n)` deletes the `n` characters in the range `[ix..ix+n-1]`. *)
    val deleteSlice : t * int * int -> unit

  end = struct

    structure Arr = CharArray
    structure Slice = CharArraySlice

    type t = {
        contents : CharArray.array ref, (* current contents of the buffer *)
        last : int ref                  (* index of position following last character *)
      }

    fun new () = {
            contents = ref(Arr.array(128, #"\000")),
            last = ref 0
          }

    fun isEmpty (buf : t) = (!(#last buf) = 0)

    fun clear {contents, last} = (last := 0)

    fun getContents ({contents, last} : t) = Slice.slice(!contents, 0, SOME(!last))

    fun grow ({contents, last} : t, n) = let
          val contents' = !contents
          val len = Arr.length contents'
          val capacity = Arr.length(!contents) - !last
          in
            if capacity > n
              then ()
              else let
                val newSz = Int.max(len+len, len+n)
                val newContents = Arr.array(newSz, #"\000")
                in
                  Slice.copy {
                      src = Slice.slice(contents', 0, SOME(!last)),
                      di = 0, dst = newContents
                    };
                  contents := newContents
                end
          end

    fun append1 (buf as {contents, last}, c) = (
          grow (buf, 1);
          Arr.update(!contents, !last, c);
          last := !last + 1)

    fun append (buf as {contents, last}, s) = let
          val n = size s
          in
            grow (buf, n);
            Arr.copyVec {src = s, di = !last, dst = !contents};
            last := !last + n
          end

    fun insertAt (buf as {contents, last}, pos, c) = let
          val _ = grow (buf, 1)
          val len = !last
          val arr = !contents
          in
            if (pos >= len)
              then (
                Arr.update(arr, len, c);
                last := len + 1)
              else let
                val pos = if (pos < 0) then 0 else pos
                in
                  Slice.copy {
                      src = Slice.slice(arr, pos, SOME(len - pos)),
                      di = pos + 1, dst = arr
                    };
                  Arr.update(arr, pos, c);
                  last := len + 1
                end
          end (* insertAt *)

    fun deleteAt (buf as {contents, last}, pos) = let
          val len = !last
          val arr = !contents
          in
            if (pos < len - 1)
              then let
                val pos = if (pos < 0) then 0 else pos
                in
                  Slice.copy {
                      src = Slice.slice(arr, pos+1, SOME(len - pos - 1)),
                      di = pos, dst = arr
                    };
                  last := len - 1
                end
              else last := len - 1
          end (* deleteAt *)

    fun deleteSlice (buf as {contents, last}, start, n) = let
          val len = !last
          val arr = !contents
          val after = start + n (* index of first char after deletion *)
          in
            if (after < len)
              then (
                Slice.copy {
                    src = Slice.slice(arr, after, NONE),
                    di = start, dst = arr
                  };
                last := len - n)
              else last := Int.max(0, start)
          end

  end
