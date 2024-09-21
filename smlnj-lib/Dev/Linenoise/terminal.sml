(* terminal.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *)

structure Terminal : sig

    type t

    type file_desc = Posix.IO.file_desc

    (* create a new terminal state from file descriptors.  Note that we assume
     * that `inFD` has already been verified as a TTY.
     *)
    val new : {inFD : file_desc, outFD : file_desc} -> t

    (* `withRawMode (term, f) arg` sets the terminal to "raw" mode and then
     * applies the function `f` to `arg`.  The mode of the terminal is restored
     * prior to returning the result of `f arg` (or if an exception is raised).
     * Note that calls to `withRawMode` will nest correctly.
     *)
    val withRawMode : t * ('a -> 'b) -> 'a -> 'b

    (* input a single character; this returns #"\000" on EOF *)
    val input1 : t -> char

    (* output a single character; this returns true if successful and false otherwise *)
    val output1 : t * char -> bool

    (* output a string; this returns the number of characters written *)
    val output : t * string -> int

    (* get the current cursor position by querying the terminal *)
    val getCursorPos : t -> {line : int, col : int} option

    (* emit the ASCII "Bell" character as feedback *)
    val bell : t -> unit

  end = struct

    structure W8A = Word8Array
    structure W8V = Word8Vector
    structure W8VS = Word8VectorSlice
    structure TTY = Posix.TTY

    type file_desc = Posix.IO.file_desc

    datatype t = TERM of {
        inFD : file_desc,
        outFD : file_desc,
        isRaw : bool ref
      }

    fun new {inFD, outFD} = TERM{
            inFD = inFD, outFD = outFD, isRaw = ref false
          }

    fun withRawMode (TERM{inFD, isRaw, ...}, f) = let
          val origTermIOS = TTY.TC.getattr inFD
          val {iflag, oflag, cflag, lflag, cc, ispeed, ospeed} = TTY.fieldsOf origTermIOS
          val rawTermIOS = TTY.termios {
                (* input modes: no break, no CR to NL, no parity check, no strip char,
                 * no start/stop output control.
                 *)
                iflag = TTY.I.clear (TTY.I.flags [
                    TTY.I.brkint, TTY.I.icrnl, TTY.I.inpck, TTY.I.istrip, TTY.I.ixon
                  ], iflag),
                (* output modes - disable post processing *)
                oflag = TTY.O.clear (oflag, TTY.O.opost),
                (* control modes - set 8 bit chars *)
                cflag = TTY.C.flags [cflag, TTY.C.cs8],
                (* local modes - choing off, canonical off, no extended functions,
                 * no signal chars (^Z,^C)
                 *)
                lflag = TTY.L.clear (TTY.L.flags [
                    TTY.L.echo, TTY.L.icanon, TTY.L.iexten, TTY.L.isig
                  ], lflag),
                (* control chars - set return condition: min number of bytes and timer.
                 * We want read to return every single byte, without timeout.
                 *)
                cc = TTY.V.update (cc, [(TTY.V.min, #"\001"), (TTY.V.time, #"\000")]),
                ispeed = ispeed,
                ospeed = ospeed
              }
          fun restore () = (
                TTY.TC.setattr (inFD, TTY.TC.saflush, origTermIOS);
                isRaw := false)
          fun setRaw () = (
                TTY.TC.setattr (inFD, TTY.TC.saflush, rawTermIOS);
                isRaw := true)
          (* the wrapped function *)
          fun f' arg = if !isRaw
                then f arg
                else (
                  setRaw ();
                  (f arg before restore())
                    handle ex => (restore(); raise ex))
          in
            f'
          end

    (* low-level I/O functions *)
    fun read1 inFD = let
          val v = Posix.IO.readVec (inFD, 1)
          in
            if W8V.length v <> 1
              then #"\000"
              else Byte.byteToChar(W8V.sub(v, 0))
          end
    fun write1 (outFD, c) =
          Posix.IO.writeVec (outFD, W8VS.full (W8V.fromList[Byte.charToByte c]))
    fun write (outFD, s) =
          Posix.IO.writeVec (outFD, W8VS.full (Byte.stringToBytes s))

    fun input1 (TERM{inFD, ...}) = read1 inFD

    fun output1 (TERM{outFD, ...}, c) = (write1 (outFD, c) = 1)

    fun output (TERM{outFD, ...}, s) = write (outFD, s)

    val scanIntFromList = Int.scan StringCvt.DEC List.getItem

    (* query the cursor position by sending an escape sequence to the
     * terminal.
     *)
    fun getCursorPos (TERM{inFD, outFD, ...}) = let
          val query = "\027[6n"
          fun read chrs = (case read1 inFD
                 of #"\000" => []
                  | #"R" => List.rev chrs
                  | c => read (c::chrs)
                (* end case *))
          (* the response should have the format "\027[<n>;<m>R", where
           * "<n>" is the number of rows and "<nc>" is the number of columns.
           * We have already discarded the terminatin "R" already.
           *)
          fun parse (#"\027" :: #"[" :: rest) = (case scanIntFromList rest
                 of SOME(nr, #";" :: rest) => (case scanIntFromList rest
                       of SOME(nc, []) => SOME{line=nr, col=nc}
                        | _ => NONE
                      (* end case *))
                  | _ => NONE
                (* end case *))
            | parse _ = NONE
          in
            if (write (outFD, query) <> size query)
              then NONE
              else parse (read [])
          end
            handle _ => NONE

    fun bell (TERM{outFD, ...}) = ignore (write1 (outFD, #"\007"))

  end
