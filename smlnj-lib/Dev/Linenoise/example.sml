(* example.sml
 *
 * COPYRIGHT (c) 2024 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 *
 * A simple example of the console editor that just echos its input in blue.
 *)

structure Example : sig

    val doit : unit -> unit

  end = struct

    structure C = ConsoleEdit
    structure AT = ANSITerm

    fun doit () = let
          val console = C.new {
                  mode = C.EMACS,
                  historyLimit = SOME 0,
                  complete = NONE,
                  prompt = ">> "
                }
          fun lp () = (case C.getLine console
                 of SOME ln => (
                      print (concat [
                          AT.toString[AT.FG AT.Blue], String.toString ln,
                          AT.toString[AT.RESET], "\n"
                        ]);
                      lp ())
                  | NONE => ()
                (* end case *))
          in
            lp ()
          end

  end
