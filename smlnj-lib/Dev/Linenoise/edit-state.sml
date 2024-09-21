(* edit-state.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure EditState : sig

    type t

  (* create the initial edit state *)
    val new : unit -> t

  (* clear the input buffer *)
    val clear : t -> unit

  (* is the edit buffer empty? *)
    val isEmpty : t -> bool

  (* get the current edit cursor position *)
    val getPos : t -> int

  (* get the current contents of the buffer as a string *)
    val getContents : t -> string

  (* return the current contents of the edit buffer as a record of three strings:
   *
   *    prefix          -- the characters before the current cursor position
   *    charAtPos       -- the character at the current position
   *    suffix          -- the characters after the current position
   *
   * The concatenation of these strings is the buffer's contents.  Note that when
   * the current position is at the end of the buffer, then `charAtPos` and `suffix`
   * will be the empty strings.
   *)
    val getContentsInParts : t -> {prefix : string, chrAtPos : string, suffix : string}

  (* functions to update the state; these are used to implement the commands;
   * the functions that have a `bool` return type return `false` on error.
   *)
    val insert : t * char -> unit       (* insert character at current position *)
    val moveL : t -> bool               (* move cursor left one character *)
    val moveR : t -> bool               (* move cursor right one character *)
    val moveHome : t -> bool            (* move cursor to beginning *)
    val moveEnd : t -> bool             (* move cursor to end *)
    val deleteL : t -> bool             (* delete character to left, moving position *)
    val deleteR : t -> bool             (* delete character to right *)
    val delPrevWord : t -> bool         (* delete word to left (incl trailing space) *)
    val delToEOL : t -> unit            (* delete to end of line *)
    val delLine : t -> unit             (* delete the entire line *)

  end = struct

    structure EB = EditBuffer
    structure Slice = CharArraySlice

  (* return the last position (also number of characters) in a buffer *)
    fun last (buf : EB.t) = !(#last buf)

    datatype t = Ed of {
        buf : EB.t,
        pos : int ref                   (* cursor position *)
       }

    fun new () = Ed{
            buf = EB.new(),
            pos = ref 0
          }

    fun isEmpty (Ed{buf, ...}) = EB.isEmpty buf

    fun clear (Ed{buf, pos, ...}) = (EB.clear buf; pos := 0)

    fun getPos (Ed{pos, ...}) = !pos

    fun getContents (Ed{buf, ...}) = Slice.vector(EB.getContents buf)

    fun getContentsInParts (Ed{buf, pos as ref ix, ...}) = let
          val contents = EB.getContents buf
          val prefix = Slice.vector(Slice.subslice(contents, 0, SOME ix))
          in
            if (ix < last buf)
              then {
                  prefix = prefix,
                  chrAtPos = Slice.vector(Slice.subslice(contents, ix, SOME 1)),
                  suffix = Slice.vector(Slice.subslice(contents, ix+1, NONE))
                }
              else {prefix = prefix, chrAtPos = "", suffix = ""}
          end

    fun insert (Ed{buf, pos as ref ix, ...}, c) = (
          EB.insertAt(buf, ix, c);
          pos := ix + 1)

    fun moveL (Ed{buf, pos as ref ix, ...}) = if (ix > 0)
          then (pos := ix - 1; true)
          else false

    fun moveR (Ed{buf, pos as ref ix, ...}) = if (ix < last buf)
          then (pos := ix + 1; true)
          else false

    fun moveHome (Ed{buf, pos as ref ix, ...}) = if (ix > 0)
          then (pos := 0; true)
          else false

    fun moveEnd (Ed{buf, pos as ref ix, ...}) = if (ix < last buf)
          then (pos := last buf; true)
          else false

    fun deleteL (Ed{buf, pos as ref ix, ...}) = if (ix > 0)
          then (
            EB.deleteAt (buf, ix - 1);
            pos := ix - 1;
            true)
          else false

    fun deleteR (Ed{buf, pos as ref ix, ...}) = if (ix < last buf)
          then (
            EB.deleteAt (buf, ix);
            true)
          else false

    fun delPrevWord (Ed{buf, pos as ref ix, ...}) = if (ix > 0)
          then let
            val contents = !(#contents buf)
            fun isSP #" " = true | isSP _ = false
            fun notSP #" " = false | notSP _ = true
          (* scan left to find the index of the last character that satisfies the
           * predicate.
           *)
            fun skip pred jx = if (jx > 0)
                  then if pred(CharArray.sub(contents, jx - 1))
                    then skip pred (jx - 1)
                    else jx
                  else 0
          (* first skip trailing whitespace and then non-space *)
            val start = skip notSP (skip isSP ix)
            val len = ix - start
            in
              EB.deleteSlice (buf, start, len);
              pos := start;
              true
            end
          else false

    fun delToEOL (Ed{buf, pos as ref ix, ...}) = (#last buf := ix)

    fun delLine (Ed{buf, pos as ref ix, ...}) = (
          #last buf := 0;
          pos := 0)

  end
