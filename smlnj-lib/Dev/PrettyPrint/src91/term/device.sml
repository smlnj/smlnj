(* smlnj-lib/Dev/PrettyPrint/src91/term/device.sml *)

(* The structure ANSITermDevice itself represents an ANSI terminal as output device.
 * Initially, we take the output stream to be TextIO.stdOut, but we could make
 * this a settable attribute of the device. There is no device "type" in this representation
 * -- just a particular device represented as a structure. *)

structure ANSITermDevice : DEVICE =
struct

local

  structure T = ANSITerm
  structure S = Style

in

(* a single, fixed outstream for this device, used for both output and for setting
 * terminal modes *)
val outstream = TextIO.stdOut  (* FIXED. Should this be a parameter somewhere? *)
val lineWidth : int = 90  (* not used -- should this become "maximum line width for device"? *)


(* ====== output functions ====== *)

(* space : int -> unit *)
(* output some number of spaces to the device *)
fun space (n: int) = TextIO.output (outstream, StringCvt.padLeft #" " n "")

(* indent : int -> unit *)
(* output an indentation of the given width to the device *)
val indent = space

(* newline : unit -> unit *)
(* output a new-line to the device *)
fun newline () = TextIO.output1 (outstream, #"\n")

(* string : string -> unit *)
(* output a string/character in the current style to the device *)
fun string (s: string) = TextIO.output (outstream, s)

(* token : string -> unit *)
(* output a string/character in the current style to the device *)
val token = string

(* flush : unit -> unit *)
(* if the device is buffered, then flush any buffered output *)
fun flush () = TextIO.flushOut outstream


(* ====== terminal modes and styles ====== *)

(* setTermMode : T.style -> unit *)
fun setTermMode cmd =
    TextIO.output (outstream, T.toString [cmd])

(* terminal mode state *)
val fg : T.color ref = ref T.Black  (* foreground color, default black *)
val bg : T.color ref = ref T.White  (* background color, default white *)
val bf : bool ref = ref false   (* boldface? default false *)
val ul : bool ref = ref false   (* underlining? default false *)
val bl : bool ref = ref false   (* blinkiing? default false *)
val dm : bool ref = ref false   (* dim? default false *)
val iv : bool ref = ref false   (* invisible? default false *)
val rv : bool ref = ref false   (* fg/bg reversal *)

(* backup terminal mode state: used to revert the terminal after a resetTerm *)
val bu_fg : T.color ref = ref T.Black  (* foreground color, default black *)
val bu_bg : T.color ref = ref T.White  (* background color, default white *)
val bu_bf : bool ref = ref false   (* boldface? default false *)
val bu_ul : bool ref = ref false   (* underlining? default false *)
val bu_bl : bool ref = ref false   (* blinkiing? default false *)
val bu_dm : bool ref = ref false   (* dim? default false *)
val bu_iv : bool ref = ref false   (* invisible? default false *)
val bu_rv : bool ref = ref false   (* fg/bg reversed? default false *)

(* defaultTerm : unit -> bool *)
(* output stream is in the default terminal mode *)
fun defaultTerm () =
    (!fg = T.Black andalso !bg = T.White andalso
     not (!bf) andalso not (!ul) andalso not (!bl) andalso
     not (!dm) andalso not (!iv) andalso not (!rv))

(* resetTerm : unit -> unit *)
(* resets terminal modes to defaults while backing up the existing mode settings *)			   
fun resetTerm () = 
    (bu_fg := !fg; fg := T.Black;
     bu_bg := !bg; bg := T.White;
     bu_bf := !bf; bf := false;
     bu_ul := !ul; ul := false;
     bu_bl := !bl; bl := false;
     bu_dm := !dm; dm := false;
     bu_iv := !iv; iv := false;
     bu_rv := !rv; rv := false;
     setTermMode T.RESET)

(* revertTerm : unit -> unit *)
(* reverts the terminal mode settings to the ones stored in the backup mode state *)
fun revertTerm () =
    (if !fg <> !bu_fg then (fg := !bu_fg; setTermMode (T.FG (!fg))) else ();
     if !bg <> !bu_bg then (bg := !bu_bg; setTermMode (T.BG (!bg))) else ();
     (case (!bf, !bu_bf)
       of (true, false) => setTermMode T.NORMAL
	| (false, true) => setTermMode T.BF
	| _ => ());
     (case (!dm, !bu_dm)
       of (true, false) => setTermMode T.NORMAL
	| (false, true) => setTermMode T.DIM
	| _ => ());
     (case (!ul, !bu_ul)
       of (true, false) => setTermMode T.UL_OFF
	| (false, true) => setTermMode T.UL
	| _ => ());
     (case (!bl, !bu_bl)
       of (true, false) => setTermMode T.BLINK_OFF
	| (false, true) => setTermMode T.BLINK
	| _ => ());
     (case (!rv, !bu_rv)
       of (true, false) => setTermMode T.REV_OFF
	| (false, true) => setTermMode T.REV
	| _ => ());
     (case (!bf, !bu_bf)
       of (true, false) => setTermMode T.INVIS_OFF
	| (false, true) => setTermMode T.INVIS
	| _ => ()))

(* applyStyle : style -> (unit -> unit) option *)
fun applyStyle (style: S.style)  : (unit -> unit) option =
    case style
      of S.FG new_fg =>
	   if not (!fg = new_fg)
	   then (* changing foreground color *)
	        let val old_fg = !fg
		 in fg := new_fg; setTermMode (T.FG new_fg);
		    SOME (fn () => (fg := old_fg; setTermMode (T.FG old_fg)))
		end
	   else NONE
       | S.BG new_bg =>
	   if not (!bg = new_bg)
	   then (* changing background color *)
	        let val old_bg = !bg
		 in bg := new_bg; setTermMode (T.BG new_bg);
		    SOME (fn () => (fg := old_bg; setTermMode (T.BG old_bg)))
		end
	   else NONE
       | S.BF => (* not orthogonal, cancels DIM as well as BF *)
	   if not (!bf)
	   then (bf := true; setTermMode T.BF;
		 SOME (fn () => (bf := false; dm := false; setTermMode T.NORMAL)))
	   else NONE
       | S.DM => (* not orthogonal, cancels BF as well as DM *)
	   if not (!dm)
	   then (dm := true; setTermMode T.DIM;
		 SOME (fn () => (dm := false; bf := false; setTermMode T.NORMAL)))
	   else NONE
       | S.UL =>
	   if not (!ul)
	   then (ul := true; setTermMode T.UL;
		 SOME (fn () => (bf := false; setTermMode T.UL_OFF)))
	   else NONE
       | S.BL =>
	   if not (!bl)
	   then (bl := true; setTermMode T.BLINK;
		 SOME (fn () => (bf := false; setTermMode T.BLINK_OFF)))
	   else NONE
       | S.RV =>
	   if not (!rv)
	   then (iv := true; setTermMode T.REV;
		 SOME (fn () => (bf := false; setTermMode T.REV_OFF)))
	   else NONE
       | S.IV =>
	   if not (!iv)
	   then (iv := true; setTermMode T.INVIS;
		 SOME (fn () => (bf := false; setTermMode T.INVIS_OFF)))
	   else NONE
       | S.NOSTYLE =>
	   if defaultTerm ()
	   then (resetTerm ();
		 SOME (fn () => revertTerm ()))
	   else NONE

(* renderStyled : S.style * (unit -> 'a) -> 'a *)
fun 'a renderStyled (style: S.style, renderFormat : unit -> 'a) : 'a =
    let val post = applyStyle style
     in renderFormat () before
	(case post
	   of NONE => ()
	    | SOME reset => reset ())
    end

end (* top local *)
end (* structure ANSITermDevice *)
