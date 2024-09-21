(* console-edit.sml
 *
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Simple support for console I/O with line editing.  This implementation is based
 * on the linenoise library (http://github.com/antirez/linenoise), with some ideas
 * from the Swift version of the library (https://github.com/andybest/linenoise-swift).
 *)

structure ConsoleEdit : sig

  (* raised when the input is connected to a TTY, but the TERM environment variable
   * is one of ["", "dumb", "cons25", "emacs"]
   *)
    exception NotSupported

  (* raised when the input is not connected to a TTY *)
    exception NotTTY

  (* raised when an IO or state setting operation is attempted on a closed console *)
    exception ClosedConsole

  (* represents the console *)
    type t

  (* the editor mode; currently only "EMACS" is supported *)
    datatype mode = EMACS | VI

  (* a completion callback is given the current contents of the buffer
   * and should return a list of possible completions.
   *)
    type completion_cb = string -> string list

  (* attempts to create a new console from the TextIO.stdIn and TextIO.stdOut
   * streams.  If successful, the console will replace the default I/O
   * streams so that subsequent attempts to read from TextIO.stdIn will result
   * in line-edited input.
   *)
    val new : {
            mode : mode,
            historyLimit : int option,          (* the history limit; `NONE` means
                                                 * unlimited, while `SOME 0` means
                                                 * no history.
                                                 *)
            complete : completion_cb option,    (* command completion callback *)
            prompt : string                     (* prompt for input *)
          } -> t

  (* is the console closed? *)
    val isClosed : t -> bool

  (* close the console. *)
    val close : t -> unit

  (* `getLine console` gets a line of input from the standard input using the
   * console editing features.  Like `TextIO.inputLine` the input is guaranteed
   * to be terminated with a newline.
   *)
    val getLine : t -> string option

  (* output to the console *)
    val output : t * string -> unit

  (* get/set the history limit; `NONE` means unlimited, while `SOME 0`
   * means no history.
   *)
    val getHistoryLimit : t -> int option
    val setHistoryLimit : t * int option -> unit

  (* get/set the prompt *)
    val getPrompt : t -> string
    val setPrompt : t * string -> unit

  (* get/set the completion callback *)
    val getCompletionCB : t -> completion_cb option
    val setCompletionCB : t * completion_cb option -> unit

  end = struct

    structure CStrs = CmdStrings
    structure ES = EditState
    structure Ed = Editor

    exception NotSupported
    exception NotTTY
    exception ClosedConsole

    datatype mode = EMACS | VI

    type completion_cb = string -> string list

  (* representation of the console device *)
    datatype t = CONSOLE of {
        closed : bool ref,
        term : Terminal.t,
        state : EditState.t,
        editor : Editor.t ref,
        complete : completion_cb option ref,
        prompt : string ref
      }

  (* get the input I/O descriptor from TextIO.stdIn.   Because of the semantics
   * of SML's input streams, we have to rebuild the input stack after, since the
   * original stream will have been closed.
   *)
    fun getInputDesc () = let
          val inS = TextIO.getInstream TextIO.stdIn
          val (rd as TextPrimIO.RD{ioDesc, ...}, buf) = TextIO.StreamIO.getReader inS
          val inS' = TextIO.StreamIO.mkInstream (rd, buf)
          in
            TextIO.setInstream (TextIO.stdIn, inS');
            ioDesc
          end

  (* get the output I/O descriptor from TextIO.stdOut. *)
    fun getOutputDesc () = (
          case TextIO.StreamIO.getWriter(TextIO.getOutstream TextIO.stdOut)
           of (TextPrimIO.WR{ioDesc, ...}, IO.LINE_BUF) => ioDesc
            | _ => NONE
          (* end case *))

  (* get the input and output descriptors for TextIO.stdIn and TextIO.stdOut.  If
   * the input is *not* a TTY or the output is not line buffered, then the NotTTY
   * exception is raised.
   *)
    fun getIODescs () = let
          val iod2fd = Option.mapPartial Posix.FileSys.iodToFD
          in
            case (iod2fd (getInputDesc()), iod2fd (getOutputDesc()))
             of (SOME inFD, SOME outFD) =>
                  if Posix.ProcEnv.isatty inFD
                    then {inFD = inFD, outFD = outFD}
                    else raise NotTTY
              | _ => raise NotTTY
            (* end case *)
          end

    fun pickEditor EMACS = EmacsMode.editor
      | pickEditor VI = raise Fail "VI mode not yet supported"

    fun new {mode, historyLimit, complete, prompt} = let
          val term = Terminal.new (getIODescs ())
          in
            CONSOLE{
                closed = ref false,
                term = term,
                state = EditState.new (),
                editor = ref(pickEditor mode),
                prompt = ref prompt
              }
          end

    fun isClosed (CONSOLE{closed, ...}) = !closed

    fun close (CONSOLE{closed, ...}) = (closed := true)

    fun getHistoryLimit (CONSOLE{...}) = SOME 0
    fun setHistoryLimit (CONSOLE{closed, ...}, limit) = if !closed
          then raise ClosedConsole
          else raise Fail "TODO"

    fun getPrompt (CONSOLE{prompt, ...}) = !prompt
    fun setPrompt (CONSOLE{closed, prompt, ...}, s) = if !closed
          then raise ClosedConsole
          else (prompt := s)

    fun getCompletionCB (CONSOLE{complete, ...}) = !complete
    fun setCompletionCB (CONSOLE{closed, ...}, cb) = if !closed
          then raise ClosedConsole
          else (complete := cb)

    fun getLine (CONSOLE{term, state, editor, complete, prompt, ...}) = let
          val editor = !editor (fn () => Terminal.input1 term)
          fun refresh () = let
                val cmd = String.concat [
                        CStrs.moveToBOL,        (* return to beginning of the line *)
                        !prompt,                (* write the prompt *)
                        ES.getContents state,   (* write the line's contents *)
(* TODO: hints *)
                        CStrs.clearToEOL,
                        CStrs.moveTo(size(!prompt) + ES.getPos state)
                      ]
                in
(* TODO: what if the write fails in some way? *)
                  ignore (Terminal.output (term, cmd))
                end
          fun newline () = ignore (Terminal.output (term, "\n"))
        (* command interpreter *)
          fun interp () = (case editor state
                 of Ed.ENTER => SOME(ES.getContents state ^ "\n")
                  | Ed.MV_TO_HOME => check (ES.moveHome state)
                  | Ed.MV_TO_END => check (ES.moveEnd state)
                  | Ed.MV_LEFT => check (ES.moveL state)
                  | Ed.MV_RIGHT => check (ES.moveR state)
                  | Ed.PREV => raise Fail "TODO"
                  | Ed.NEXT => raise Fail "TODO"
(* FIXME: should we raise an exception? *)
                  | Ed.INTERRUPT => SOME "\n"
                  | Ed.DEL_LEFT => check (ES.deleteL state)
                  | Ed.DEL_RIGHT => check (ES.deleteR state)
                  | Ed.CLEAR => raise Fail "TODO"
                  | Ed.SWAP => raise Fail "TODO"
                  | Ed.DEL_TO_EOL => (
                      ES.delToEOL state;
                      continue ())
                  | Ed.DEL_LINE => (
                      ES.delLine state;
                      continue ())
                  | Ed.DEL_PREV_WORD => check (ES.delPrevWord state)
                  | Ed.COMPLETE => (case !complete
                       of NONE => error ()
                        | SOME cb => completeLine (cb (ES.getContents state))
                      (* end case *))
                  | Ed.EOF => NONE
                  | Ed.SKIP => error ()
                  | Ed.INSERT c => (
                      ES.insert (state, c);
                      continue ())
                (* end case *))
          and completeLine [] = error () (* no completion *)
            | completeLine [s] = raise Fail "TODO: unique completion"
            | completeLine sl = raise Fail "TODO: completion"
          and check true = continue ()
            | check false = error ()
          and continue () = (refresh(); interp())
          and error () = (Terminal.bell term; interp())
          in
            Terminal.output (term, !prompt);
            Terminal.withRawMode (term, interp) ()
              before (ES.clear state; newline())
          end

     fun output (CONSOLE{term, ...}, s) = Terminal.output (term, s)

  end
