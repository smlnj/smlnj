(* smlnj-lib/Dev/PrettyPrint/src91/ansiterm-device.sml *)

(* The structure ANSITermDevice itself represents an ANSI terminal as output device.
 * Initially, we take the output stream to be TextIO.stdOut, but we could make
 * this a settable attribute of the device. There is no device "type" in this representation
 * -- just a particular device represented as a structure. *)

structure ANSITermDevice : DEVICE where style = ANSITermStyle.style =
struct

local

  structure T = ANSITerm
  structure S = ANSITermStyle

in

(* a single, fixed outstream for this device *)
val outstream = TextIO.stdOut
val lineWidth : int ref = ref 90

(* terminal mode state *)
val fg : color ref = ref Black  (* foreground color, default black *)
val bg : color ref = ref White  (* background color, default white *)
val bf : bool ref = ref false   (* boldface? default false *)
val ul : bool ref = ref false   (* underlining? default false *)
val bl : bool ref = ref false   (* blinkiing? default false *)
val dm : bool ref = ref false   (* dim? default false *)
val iv : bool ref = ref false   (* invisible? default false *)

(* setTermMode : T.style -> unit *)
fun setTermMode cmd =
    TextIO.output (outstream, T.toString [cmd])

(* applyStyle : style -> (unit -> unit) option *)
fun applyStyle (style: S.style)  : (unit -> unit) option =
    case style
      of S.FG new_fg =>
	   if not (!fg = new_fg)
	   then (* changing foreground color *)
	        let val old_fg = !fg
		 in fg := new_fg; setTermMode (T.FG c);
		    SOME (fn () => (fg := old_fg; setTermMode (T.FG old_fg)))
		end
	   else NONE
       | S.BG new_bg =>
	   if not (!bg = new_bg)
	   then (* changing background color *)
	        let val old_bg = !bg
		 in bg := new_bg; setTermMode (T.BG c);
		    SOME (fn () => (fg := old_bg; setTermMode (T.BG old_bg)))
		end
	   else NONE
       | S.BF => (* not orthogonal, cancels DIM as well as BF *)
	   if not bf
	   then (bf := true; setTermMode T.BF;
		 SOME (fn () => (bf := false; dm := false; setTermMode T.NORMAL)))
	   else NONE
       | S.DM => (* not orthogonal, cancels BF as well as DM *)
	   if not dm
	   then (dm := true; setTermMode T.DIM;
		 SOME (fn () => (dm := false; bf := false; setTermMode T.NORMAL)))
	   else NONE
       | S.UL =>
	   if not ul
	   then (ul := true; setTermMode T.UL;
		 SOME (fn () => (bf := false; setTermMode T.UL_OFF)))
	   else NONE
       | S.BL =>
	   if not bl
	   then (bl := true; setTermMode T.BLINK;
		 SOME (fn () => (bf := false; setTermMode T.BLINK_OFF)))
	   else NONE
       | S.IV =>
	   if not iv
	   then (iv := true; setTermMode T.INVIS;
		 SOME (fn () => (bf := false; setTermMode T.INVIS_OFF)))
	   else NONE

(* renderStyled : S.style * (unit -> unit) -> unit *)
fun renderStyled (style: S.style, renderFormat : unit -> unit) =
    let val post = applyStyle style
     in renderFormat ();
	case post
	  of NONE => ()
	   | SOME reset => reset ()
    end

(* getLineWidth : unit -> int *)
fun getLineWidth () = !lineWidth
(* setLineWidth : int -> unit *)
fun setLineWidth (n: int) = (lineWidth := n)

(* space : int -> unit *)
(* output some number of spaces to the device *)
fun space (n: int) = TextIO.output (outstream, StringCvt.padLeft #" " n "")

(* indent : int -> unit *)
(* output an indentation of the given width to the device *)
val indent = space

(* newline : unit -> unit *)
(* output a new-line to the device *)
fun newline () = TextIO.output1 (outStream, #"\n")

(* string : string -> unit *)
(* output a string/character in the current style to the device *)
fun string (s: string) = TextIO.output (dst, s)

(* token : string -> unit *)
(* output a string/character in the current style to the device *)
val token = string

(* flush : unit -> unit *)
(* if the device is buffered, then flush any buffered output *)
fun flush () = TextIO.flushOut outstream


end (* top local *)
end (* structure ANSITermDevice *)
