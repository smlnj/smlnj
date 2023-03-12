(* PrettyPrint/src91/ansiterm-device.sml *)

structure ANSITermDevice =
struct

local

  structure AT = ANSITerm

in

val outstream = TextIO.stdOut

type tmode = {fg : color,  (* foreground color, default black *)
              bg : color,  (* background color, default white *)
	      bf : bool,   (* boldface? default false *)
	      ul : bool,   (* underlining? default false *)
	      bl : bool,   (* blinkiing? default false *)
	      dm : bool,   (* dim? default false *)
	      iv : bool}   (* invisible? default false *)

type tmodeSetter = {set : tmode -> tmode, reset : tmode -> tmode}

val defaultTmode: tmode =
    {fg = Black, bg = White, bf = false, ul = false, bl = false, dm = false, iv = false}

val tmode : tmode ref = ref default

fun setFg (c: color) ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {fg = c, bg = bg, bf = bf, ul = ul, bl = bl, dm = dm, iv = iv}		     

fun setBg (c: color) ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {bg = c, fg = fg, bf = bf, ul = ul, bl = bl, dm = dm, iv = iv}		     

fun setBf ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {fg = fg, bg = bg, bf = true, ul = ul, bl = bl, dm = dm, iv = iv}		     

fun setUl ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {fg = fg, bg = bg, bf = bf, ul = true, bl = bl, dm = dm, iv = iv}		     

fun setBl ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {fg = fg, bg = bg, bf = bf, ul = ul, bl = true, dm = dm, iv = iv}		     

fun setDm ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {fg = fg, bg = bg, bf = bf, ul = ul, bl = bl, dm = true, iv = iv}		     

fun setIv ({fg, bg, bf, ul, bl, dm, iv} : tmode) : tmode =
    {fg = fg, bg = bg, bf = bf, ul = ul, bl = bl, dm = dm, iv = true}		     

(* changed : style * tmode -> tmodeSetter option *)
fun changed (style, orig as {fg,bg,bf,ul,bl,dm,iv}: tmode) : tmodeSetter option =
    case style
      of FG c =>
	   if not (c = fg)
	   then SOME {set = setFg c, reset = (fn _ => orig)}
	   else NONE
       | BG c =>
	   if not (c = fg)
	   then SOME {set = setBg c, reset = (fn _ => orig)}
	   else NONE
       | BF =>
	   if not bf
	   then SOME {set = setBf, reset = (fn _ => orig)}
	   else NONE
       | UL =>
	   if not ul
	   then SOME {set = setUl, reset = (fn _ => orig)}
	   else NONE
       | BL =>
	   if not bl
	   then SOME {set = setBl, reset = (fn _ => orig)}
	   else NONE
       | DM =>
	   if not dm
	   then SOME {set = setDm, reset = (fn _ => orig)}
	   else NONE
       | IV =>
	   if not iv
	   then SOME {set = setIv, reset = (fn _ => orig)}
	   else NONE

fun styledRender (style, renderer : unit -> unit) =
    (case changed (style, !tmode)
      of NONE => renderer ()
       | SOME {set, reset} =>
	   (set (); renderer (); reset ()))

end (* top local *)
end (* structure ANSITermDevice *)
