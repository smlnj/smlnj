(* editor.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Editor =
  struct

  (* abstract edit commands *)
    datatype cmd
      = ENTER           (* return the current contents of the edit buffer *)
      | MV_TO_HOME      (* move cursor to upper-left corner *)
      | MV_TO_END       (* move cursor to end of buffer *)
      | MV_LEFT         (* move cursor one position left *)
      | MV_RIGHT        (* move cursor one position right *)
      | PREV            (* move to previous history entry *)
      | NEXT            (* move to next history entry *)
      | INTERRUPT       (* interrupt the editing (i.e., ^C) *)
      | DEL_LEFT        (* delete character to left of cursor *)
      | DEL_RIGHT       (* delete character to right of cursor *)
      | CLEAR           (* clear the and refresh the screen *)
      | SWAP            (* swap current character with previous *)
      | DEL_TO_EOL
      | DEL_LINE
      | DEL_PREV_WORD
      | COMPLETE        (* command completion *)
      | EOF
      | SKIP            (* used for unrecognized escape sequences *)
      | INSERT of char

  (* an "editor" determines how character input is interpreted *)
    type t = (unit -> char) -> EditState.t -> cmd

  end
