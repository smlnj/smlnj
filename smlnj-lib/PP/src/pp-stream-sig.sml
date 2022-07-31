(* pp-stream-sig.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This interface provides a output stream interface to pretty printing.
 *)

signature PP_STREAM =
  sig
    type device
    type stream

    type token
	(* tokens are an abstraction of strings (allowing for different
	 * widths and style information).
	 *)
    type style

    datatype indent
      = Abs of int		(* indent relative to outer indentation *)
      | Rel of int		(* indent relative to start of box *)

    val openStream  : device -> stream
    val flushStream : stream -> unit
    val closeStream : stream -> unit
    val getDevice   : stream -> device

  (* open an horizontal box (HBox); breaks and spaces are mapped to spaces,
   * even if the line width is exceeded.
   *)
    val openHBox   : stream -> unit

  (* open a vertical box (VBox); breaks and spaces are mapped to newlines *)
    val openVBox   : stream -> indent -> unit

  (* open a horizontal/vertical box; this box behaves like an HBox, unless the
   * contents does not fit on the line (or there is an explicit newline), in
   * which case it behaves like a VBox.
   *)
    val openHVBox  : stream -> indent -> unit

  (* open a horizontal or vertical box; the contents are layed out in horizontal
   * mode until the line is full, at which point a line break is introduced and
   * a new line is started.  This box is essentially a paragraph box.
   *)
    val openHOVBox : stream -> indent -> unit

  (* similar to the `openHOVBox` function, except that breaks split the current
   * line if splitting would move to the left.
   *)
    val openBox    : stream -> indent -> unit

  (* close the most recently opened box.  Note that every openBox call must be
   * matched by a closeBox.
   *)
    val closeBox   : stream -> unit

    val token   : stream -> token -> unit
    val string  : stream -> string -> unit

    val pushStyle : (stream * style) -> unit
    val popStyle  : stream -> unit

  (* `break strm {nsp, offset}` expands to either `nsp` spaces or
   * a line break with the specified indentation `offset`.
   *)
    val break   : stream -> {nsp : int, offset : int} -> unit
  (* `space strm n` is equivalent to `break strm {nsp=n, offset=0}` *)
    val space   : stream -> int -> unit
  (* `cut strm` is equivalent to `break{nsp=0, offset=0}` *)
    val cut     : stream -> unit
  (* break the current line *)
    val newline : stream -> unit
  (* emits a nonbreakable space *)
    val nbSpace : stream -> int -> unit

    val control : stream -> (device -> unit) -> unit

  end

