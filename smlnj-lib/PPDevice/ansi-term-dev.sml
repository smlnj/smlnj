(* ansi-term-dev.sml
 *
 * COPYRIGHT (c) 2023 John Reppy (http://www.cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * A pretty-printing device for text output to ANSI terminals.  This device
 * supports the standard ANSI output attributes.
 *)

structure ANSITermDev : sig

    include PP_DEVICE
      where type style = ANSITerm.style list
      where type token = string

    (* create an output device; if the underlying stream is connected to a TTY,
     * then styled output is enabled, otherwise it will be disabled.
     *)
    val openDev : {dst : TextIO.outstream, wid : int} -> device

    (* enable/disable/query styled output.
     *
     *	styleMode (dev, NONE)	        -- query current mode
     *	styleMode (dev, SOME true)	-- enable styled output
     *	styleMode (dev, SOME false)	-- disable styled output
     *
     * This function returns the previous state of the device.
     *
     * NOTE: this function raises Fail if called while a style is active.
     *)
    val styleMode : (device * bool option) -> bool

  end = struct

    structure A = ANSITerm

    (* ===== The State of the ANSI Terminal ===== *)
    type state = {
        fg : A.color option,	(* NONE is default color for terminal *)
        bg : A.color option,	(* NONE is default color for terminal *)
        bold : bool,
        blink : bool,
        ul : bool,
        rev : bool,
        invis : bool
      }

    val initState : state = {
	  fg=NONE, bg=NONE,
	  bold=false, blink=false, ul=false, rev=false, invis=false
	}

    (* compute the commands to transition from one state to another *)
    fun transition (s1 : state, s2 : state) = let
	(* compute the commands to set the foreground color *)
	  val mv = (case (#fg s1, #fg s2)
		 of (SOME c1, SOME c2) => if c1 = c2 then [] else [A.FG c2]
		  | (_, SOME c) => [A.FG c]
		  | (_, NONE) => [A.FG A.Default]
		(* end case *))
	(* compute the commands to set the background color *)
	  val mv = (case (#bg s1, #bg s2)
		 of (SOME c1, SOME c2) => if c1 = c2 then mv else A.FG c2 :: mv
		  | (_, SOME c) => A.BG c :: mv
		  | (_, NONE) => A.FG A.Default :: mv
		(* end case *))
	(* compute the commands to set the other display attributes *)
	  fun add (proj, cmd, off, mv) = (case (proj s1, proj s2)
		 of (false, true) => cmd::mv
		  | (true, false) => off::mv
		  | _ => mv
		(* end case *))
	  val mv = add (#bold, A.BF, A.NORMAL, mv)
	  val mv = add (#blink, A.BLINK, A.BLINK_OFF, mv)
	  val mv = add (#ul, A.UL, A.UL_OFF, mv)
	  val mv = add (#rev, A.REV, A.REV_OFF, mv)
	  val mv = add (#invis, A.INVIS, A.INVIS_OFF, mv)
	  in
	    if null mv then "" else A.toString mv
	  end

    (* apply a command to a state *)
    fun updateState1 (cmd, style as {fg, bg, bold, blink, ul, rev, invis}) = (
	  case cmd
	   of A.FG c =>
		{fg=SOME c, bg=bg, bold=bold, blink=blink, ul=ul, rev=rev, invis=invis}
	    | A.BG c =>
		{fg=fg, bg=SOME c, bold=bold, blink=blink, ul=ul, rev=rev, invis=invis}
	    | A.BF =>
		{fg=fg, bg=bg, bold=true, blink=blink, ul=ul, rev=rev, invis=invis}
	    | A.BLINK =>
		{fg=fg, bg=bg, bold=bold, blink=true,  ul=ul, rev=rev, invis=invis}
	    | A.UL =>
		{fg=fg, bg=bg, bold=bold, blink=blink, ul=true, rev=rev, invis=invis}
	    | A.REV =>
		{fg=fg, bg=bg, bold=bold, blink=blink, ul=ul, rev=true, invis=invis}
	    | A.INVIS =>
		{fg=fg, bg=bg, bold=bold, blink=blink, ul=ul, rev=rev, invis=true}
(* TODO: add support for A.DIM *)
	    | _ => style
	  (* end case *))

    (* apply a sequence of commands to a state *)
    fun updateState (cmds, st) = List.foldl updateState1 st cmds


    (* ===== The Core Pretty-Printing Operations ===== *)
    structure DevOps =
      struct

        type t = {
            dst : TextIO.outstream,     (* the output stream *)
            tty : bool,                 (* true for TTYs, which are assumed to
                                         * understand the ANSI escape sequences
                                         *)
            mode : bool ref,            (* true if styling is enabled. *)
            stk : state list ref        (* the styling stack *)
          }

        type style = A.style list

        type token = string

        fun top [] = initState
          | top (st::r) = st

        fun pushStyle ({mode, dst, stk, ...} : t, sty) =
              if (! mode)
                then let
                  val curSt = top (!stk)
                  val newSt = updateState (sty, curSt)
                  in
                    TextIO.output (dst, transition(curSt, newSt));
                    stk := newSt :: !stk
                  end
                else ()

        fun popStyle ({mode, dst, stk, ...} : t) =
              if (! mode)
                then (case !stk
                   of [] => ()
                    | curSt::r => let
                        val newSt = top r
                        in
                          TextIO.output (dst, transition(curSt, newSt));
                          stk := r
                        end
                  (* end case *))
                else ()

        val defaultStyle = []

        (* Output operations *)
        fun space ({dst, ...} : t, n) = TextIO.output (dst, StringCvt.padLeft #" " n "")
        val indent = space
        fun newline ({dst, ...} : t) = TextIO.output1 (dst, #"\n")
        fun string ({dst, ...} : t, s) = TextIO.output (dst, s)
        fun token ({dst, ...} : t, t) = TextIO.output (dst, t)
        fun flush ({dst, ...} : t) = TextIO.flushOut dst

      end (* structure DevOps *)

    structure Device = MakePPDeviceFn (DevOps)

    open Device

  (* return true if an outstream is a TTY *)
    fun isTTY outS = let
	  val (TextPrimIO.WR{ioDesc, ...}, _) =
		TextIO.StreamIO.getWriter(TextIO.getOutstream outS)
	  in
	    case ioDesc
	     of SOME iod => (OS.IO.kind iod = OS.IO.Kind.tty)
	      | _ => false
            (* end case *)
	  end

    fun openDev {dst, wid} = let
          val tty = isTTY dst
          val devOps : DevOps.t = {dst = dst, tty = tty, mode = ref tty, stk = ref[]}
          in
            Device.newWithWidth (devOps, wid)
          end

    (* enable styled output by passing true to this function.  It returns
     * the previous state of the device.
     *)
    fun styleMode (device, arg) = (case devOps device
           of {stk = ref(_::_), ...} =>
                raise Fail "attempt to change mode inside scope of style"
            | {tty=false, mode, ...} => !mode (* cannot change mode for non-TTY streams *)
            | {mode as ref m, ...} => (case arg
                 of NONE => m
                  | SOME flg => (mode := flg; m)
                (* end case *))
          (* end case *))

  end
