(* action.sml
 *
 * COPYRIGHT (c) 2006
 * John Reppy (http://www.cs.uchicago.edu/~jhr)
 * Aaron Turon (http://www.cs.uchicago.edu/~adrassi)
 * All rights reserved.
 *
 * Simple (opaque) encapsulation of semantic actions.
 *)

structure Action :>
  sig

    type action

    val action : Err.span * string -> action
    val empty : action
    val concat : action * action -> action

    val toString : action -> string
    val name : action -> string
    val span : action -> Err.span
    val code : action -> string
    val same : (action * action) -> bool

  end = struct

    datatype action 
      = ACT of {
	  id : int,
	  code : string,
	  span : Err.span
        }

    local
      val cnt = ref 0
    in
    fun nextId() = (cnt := !cnt + 1; !cnt)
    end

    fun action (i, s) = ACT {id = nextId(), code = s, span = i}
    fun toString (ACT {code, span, ...}) = code
(*	  if span = 1 then
	    "(*#line " ^ Int.toString span ^ ".0*)" ^ code
	  else
	    "(*#line " ^ Int.toString (span - 1) ^ ".0*) \n" ^ code
*)

    fun name (ACT{id, ...}) = Int.toString id
    fun same (ACT{id = id1, ...}, ACT {id = id2, ...}) = (id1 = id2)

    fun span (ACT{span, ...}) = span
    fun code (ACT{code, ...}) = code

    val empty = action (Err.emptySpan, "")
    fun concat (a, b) = 
	  if same (a, empty) then b 
	  else if same (b, empty) then a
	  else action (span a, code a ^ code b)

  end
