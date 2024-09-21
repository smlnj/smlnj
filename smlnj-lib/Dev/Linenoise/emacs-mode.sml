(* emacs-mode.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * Implementation of the default "emacs" mode for the console
 * editor.  The commands are:
 *
 *      ^A      -- move to beginning of line
 *      ^E      -- move to end of line
 *      ^B      -- move left one space
 *      ^F      -- move right one space
 *      ^D      -- delete character to right
 *      ^H      -- delete character to left (also "delete" key)
 *      ^K      -- delete to end of line
 *      ^U      -- delete entire line
 *      ^W      -- delete previous word
 *      <ret>   -- finish editing
 *      ^C      -- clear buffer and return the empty string
 *      ^L      -- clear buffer
 *)

structure EmacsMode : sig

    val editor : Editor.t

  end = struct

    datatype cmd = datatype Editor.cmd

    fun editor getc (state : EditState.t) = (case getc()
           of #"\000" => EOF
            | #"\^A" => MV_TO_HOME
            | #"\^B" => MV_LEFT
            | #"\^C" => INTERRUPT
            | #"\^D" => if EditState.isEmpty state
                then EOF (* ^D when the buffer is empty *)
                else DEL_RIGHT
            | #"\^E" => MV_TO_END
            | #"\^F" => MV_RIGHT
            | #"\^K" => DEL_TO_EOL
            | #"\^L" => CLEAR
            | #"\^N" => NEXT
            | #"\^P" => PREV
            | #"\^T" => SWAP
            | #"\^U" => DEL_LINE
            | #"\^W" => DEL_PREV_WORD
            | #"\b" => DEL_LEFT         (* backspace (a.k.a. ^H) *)
            | #"\127" => DEL_LEFT       (* "delete" *)
            | #"\r" => ENTER
            | #"\t" => COMPLETE
            | #"\^[" => (case getc()
                 of #"[" => (case getc()
                       of #"A" => PREV (* ESC [ A == Up arrow *)
                        | #"B" => NEXT (* ESC [ B == Down arrow *)
                        | #"C" => MV_RIGHT (* ESC [ C == Right arrow *)
                        | #"D" => MV_LEFT (* ESC [ D == Left arrow *)
                        | #"H" => MV_TO_HOME (* ESC [ H == Home *)
                        | #"F" => MV_TO_END (* ESC [ F == End *)
                        | c => if Char.isDigit c
                            then ( (* extended escape *)
                              case getc()
                               of #"~" => if (c = #"3")
                                    then DEL_RIGHT (* ESC [ 3 ~ == Delete *)
                                    else SKIP
                                | _ => SKIP
                              (* end case *))
                            else SKIP
                      (* end case *))
                  | #"O" => (case getc()
                       of #"H" => MV_TO_HOME (* ESC O H == Home *)
                        | #"F" => MV_TO_END (* ESC O F == End *)
                        | _ => SKIP
                      (* end case *))
                  | _ => SKIP
                (* end case *))
            | c => INSERT c
          (* end case *))

  end
