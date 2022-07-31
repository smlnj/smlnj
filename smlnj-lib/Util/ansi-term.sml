(* ansi-term.sml
 *
 * COPYRIGHT (c) 2020 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Support for ANSI terminal control codes.  Currently, this support
 * is just for display attributes.
 *)

structure ANSITerm : sig

    datatype color
      = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

    datatype style
      = FG of color	(* foreground color *)
      | BG of color	(* background color *)
      | BF		(* bold/bright *)
      | DIM		(* dim *)
      | NORMAL		(* normal intensity/brightness *)
      | UL		(* underline *)
      | UL_OFF		(* underline off *)
      | BLINK		(* blinking text *)
      | BLINK_OFF	(* blinking off *)
      | REV		(* reverse video *)
      | REV_OFF		(* reverse video off *)
      | INVIS		(* invisible *)
      | INVIS_OFF	(* invisible off *)
      | RESET

  (* return the command string for the given styles; the empty list is "normal" *)
    val toString : style list -> string

  (* output commands to set the given styles; the empty list is "normal" *)
    val setStyle : (TextIO.outstream * style list) -> unit

  end = struct

    datatype color
      = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default

    datatype style
      = FG of color	(* foreground color *)
      | BG of color	(* background color *)
      | BF		(* bold/bright *)
      | DIM		(* dim *)
      | NORMAL		(* normal intensity/brightness *)
      | UL		(* underline *)
      | UL_OFF		(* underline off *)
      | BLINK		(* blinking text *)
      | BLINK_OFF	(* blinking off *)
      | REV		(* reverse video *)
      | REV_OFF		(* reverse video off *)
      | INVIS		(* invisible *)
      | INVIS_OFF	(* invisible off *)
      | RESET

  (* basic color codes *)
    fun colorToCmd Black = 0
      | colorToCmd Red = 1
      | colorToCmd Green = 2
      | colorToCmd Yellow = 3
      | colorToCmd Blue = 4
      | colorToCmd Magenta = 5
      | colorToCmd Cyan = 6
      | colorToCmd White = 7
      | colorToCmd Default = 9

  (* convert style to integer command *)
    fun styleToCmd (FG c) = 30 + colorToCmd c
      | styleToCmd (BG c) = 40 + colorToCmd c
      | styleToCmd BF = 1
      | styleToCmd DIM = 2
      | styleToCmd NORMAL = 22
      | styleToCmd UL = 4
      | styleToCmd UL_OFF = 24
      | styleToCmd BLINK = 5
      | styleToCmd BLINK_OFF = 25
      | styleToCmd REV = 7
      | styleToCmd REV_OFF = 27
      | styleToCmd INVIS = 8
      | styleToCmd INVIS_OFF = 28
      | styleToCmd RESET = 0

    fun cmdStr [] = ""
      | cmdStr (cmd :: r) = let
	  fun f (cmd, l) = ";" :: Int.toString cmd :: l
	  in
	    concat ("\027[" :: Int.toString cmd :: List.foldr f ["m"] r)
	  end

    fun toString [] = cmdStr[0]
      | toString stys = cmdStr(List.map styleToCmd stys)

    fun setStyle (outStrm, stys) = TextIO.output(outStrm, toString stys)

  end
