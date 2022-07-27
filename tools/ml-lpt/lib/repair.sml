(* repair.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Representation and pretty-printing of ml-antlr repair actions
 *)

structure AntlrRepair :> sig

    datatype 'tok repair_action
      = Insert of 'tok list
      | Delete of 'tok list
      | Subst of {
          old : 'tok list, 
          new : 'tok list
        }
      | FailureAt of 'tok

    type 'tok repair = AntlrStreamPos.pos * 'tok repair_action

    val actionToString : ('tok -> string) -> 'tok repair_action -> string
    val repairToString : ('tok -> string) -> AntlrStreamPos.sourcemap -> 'tok repair -> string

  (* used to define the context of a repair action *)
    datatype add_or_delete = ADD | DEL

  (* return a string representation of the repair action.  This version uses the add_or_delete
   * information to allow different token names for deletion (more specific) and addition (more
   * general).
   *)
    val actionToString' : (add_or_delete -> 'tok -> string) -> 'tok repair_action -> string
    val repairToString' : (add_or_delete -> 'tok -> string) -> AntlrStreamPos.sourcemap -> 'tok repair -> string

  end = struct

    datatype 'a repair_action
      = Insert of 'a list
      | Delete of 'a list
      | Subst of {
          old : 'a list, 
          new : 'a list
        }
      | FailureAt of 'a

    type 'a repair = AntlrStreamPos.pos * 'a repair_action

  (* used to define the context of a repair action *)
    datatype add_or_delete = ADD | DEL

    fun actionToString' tokToString repair = let
	  val join = String.concatWith " "
          in
            case repair
             of Insert toks => join ("try inserting" :: List.map (tokToString ADD) toks)
              | Delete toks => join ("try deleting" :: List.map (tokToString DEL) toks)
              | Subst {old, new} => join(
		  "try substituting" :: List.map (tokToString ADD) new @
		  "for" :: List.map (tokToString DEL) old)
              | FailureAt tok => "syntax error at " ^ tokToString DEL tok
            (* end case *)
          end

    fun repairToString' tokToString sm (pos, repair) = 
          (AntlrStreamPos.toString sm pos ^ ": " ^ actionToString' tokToString repair)

    fun actionToString tokToString = let
	  fun tok2s _ tok = tokToString tok
	  in
	    actionToString' tok2s
	  end

    fun repairToString tokToString = let
	  fun tok2s _ tok = tokToString tok
	  in
	    repairToString' tok2s
	  end

  end
