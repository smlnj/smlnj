(* cmd-strings.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Terminal command strings
 *)

structure CmdStrings : sig

  (* move cursor position to beginning of the line *)
    val moveToBOL : string

  (* clear the line to the end *)
    val clearToEOL : string

  (* move right the specified number of characters *)
    val moveRight : int -> string

  (* move to the specified position (which we do by moving
   * to tg=he beginning and then right.
   *)
    val moveTo : int -> string

  (* clear the screen *)
    val clearScreen : string

  (* display a string in reverse video mode (to highlight the cursor position) *)
    val reverse : string  -> string

  end = struct

    val moveToBOL = "\r"

    val clearToEOL = "\027[0K"

    fun moveRight n = if n <= 0
          then ""
          else concat["\027[", Int.toString n, "C"]

    fun moveTo n = if n <= 0
          then "\r"
          else concat["\r\027[", Int.toString n, "C"]

    val clearScreen = "\027[2J"

    val revOn = ANSITerm.toString [ANSITerm.REV]
    val revOff = ANSITerm.toString [ANSITerm.REV_OFF]

    fun reverse s = String.concat[revOn, s, revOff]

  end
